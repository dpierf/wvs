# =========================================================================== #
#                         Funções do Pipeline ML: WVS                         #
# =========================================================================== #

# Carregamento generalizado da base WVS a partir de um arquivo ZIP
carregar_base <- function(path_zip, wave_min = 1) {
  local({
    tmp <- tempdir()
    rds <- unzip(path_zip, list = TRUE)$Name[1]
    unzip(path_zip, exdir = tmp)
    readRDS(file.path(tmp, rds))
  }) |>
    filter(haven::zap_labels(S002VS) >= wave_min)
}

# Preparando a base (filtrando) para dashboard
preparar_base <- function(df, vars = VARS_DASHBOARD) {
  resultado <- df |>
    select(any_of(vars)) |>
    mutate(across(
      !all_of(c('Y003', 'Y010')) & where(~ !is.character(.)),
      ~ labelled::set_na_values(., -1:-5)
    )) |>
    mutate(across(
      !all_of(c('Y003', 'Y010')) & where(~ !is.character(.)),
      labelled::user_na_to_na
    )) |>
    mutate(
      continent = countrycode(COUNTRY_ALPHA, origin = 'iso3c', destination = 'continent'),
      region    = countrycode(COUNTRY_ALPHA, origin = 'iso3c', destination = 'region')
    )
  gc()
  resultado
}

# Preparando a base para o modelo de machine learning
preparar_wave <- function(base, wave_num, vars_excluir) {
  # Completar vars_excluir com S* não desejados
  vars_s <- names(base)[
    startsWith(names(base), 'S') &
      !names(base) %in% c('S002VS', 'S007', 'S017', 'SurvSAgg')
  ]
  vars_remover <- unique(c(vars_excluir, vars_s))
  
  df <- base |>
    as.data.frame() |>
    filter(S002VS == wave_num) |>
    mutate(
      target     = if_else(as.numeric(X047_WVS) < 4, 1L, 0L, missing = NA_integer_),
      region_tmp = countrycode(COUNTRY_ALPHA, origin = 'iso3c', destination = 'region')  # ← antes do select
    ) |>
    filter(!is.na(target)) |>
    select(-all_of(vars_remover[vars_remover != 'S002VS']), -X047_WVS)
  
  message('Wave ', wave_num, ': ', nrow(df), ' linhas, ',
          ncol(df) - 2, ' atributos (excl. S007 e target)')
  df
}

fazer_split <- function(df, prop_oos = 0.30, prop_val = 0.20, seed = 18692) {
  set.seed(seed)
  
  df <- df |>
    mutate(strato = if_else(is.na(region_tmp),
                            as.character(target),
                            paste0(target, '_', region_tmp)))
  
  split_oos <- initial_split(df, prop = 1 - prop_oos, strata = strato)
  trainval  <- training(split_oos)
  oos       <- testing(split_oos)
  
  split_val <- initial_split(trainval, prop = 1 - prop_val, strata = strato)
  dev       <- training(split_val)
  val       <- testing(split_val)
  
  dev <- dev |> select(-region_tmp, -strato)
  val <- val |> select(-region_tmp, -strato)
  oos <- oos |> select(-region_tmp, -strato)
  
  message('DEV: ', nrow(dev), ' | VAL: ', nrow(val), ' | OOS: ', nrow(oos))
  message('Target (DEV): ', round(mean(dev$target) * 100, 1), '% positivos')
  
  list(dev = dev, val = val, oos = oos)
}

filtros_qualidade <- function(df_dev,
                              miss_threshold = 0.20,
                              freq_threshold = 0.95) {
  vars_pred <- setdiff(names(df_dev), c('S007', 'S017', 'target', 'COUNTRY_ALPHA', 'region_tmp'))
  
  miss_rate <- sapply(df_dev[vars_pred], function(x) mean(is.na(x)))
  vars_miss <- names(miss_rate[miss_rate > miss_threshold])
  message('Removidas por missing (>', miss_threshold * 100, '%): ', length(vars_miss))
  
  vars_restantes <- setdiff(vars_pred, vars_miss)
  freq_dom <- sapply(df_dev[vars_restantes], function(x) {
    tb <- table(x, useNA = 'no')
    if (length(tb) == 0) return(1)
    max(tb) / sum(tb)
  })
  vars_freq <- names(freq_dom[freq_dom > freq_threshold])
  message('Removidas por dominância (>', freq_threshold * 100, '%): ', length(vars_freq))
  
  vars_aprovadas <- setdiff(vars_pred, union(vars_miss, vars_freq))
  message('Atributos aprovados: ', length(vars_aprovadas))
  vars_aprovadas
}

fs_univariado <- function(df_dev, vars_candidatas,
                          iv_threshold = 0.02,
                          cv_threshold = 0.05) {
  df_fs <- df_dev |>
    select(all_of(vars_candidatas), target) |>
    mutate(across(everything(), as.numeric))
  
  message('Calculando IV...')
  iv_result <- create_infotables(data = df_fs, y = 'target',
                                 bins = 10, parallel = FALSE)
  iv_df <- iv_result$Summary |>
    filter(IV >= iv_threshold) |>
    arrange(desc(IV))
  message('Aprovadas por IV (>= ', iv_threshold, '): ', nrow(iv_df))
  
  vars_iv <- iv_df$Variable
  message('Calculando V de Cramér...')
  cv_df <- tibble(
    variavel  = vars_iv,
    cramers_v = sapply(vars_iv, function(v) {
      tryCatch(
        cramer_v(as.character(df_dev[[v]]), as.character(df_dev$target)),
        error = function(e) NA_real_
      )
    })
  ) |> filter(!is.na(cramers_v), cramers_v >= cv_threshold)
  message('Aprovadas por V de Cramér (>= ', cv_threshold, '): ', nrow(cv_df))
  
  iv_df |>
    inner_join(cv_df, by = c('Variable' = 'variavel')) |>
    rename(variavel = Variable)
}

fs_multivariado <- function(df_dev, vars_candidatas,
                            n_features_mrmr = NULL,
                            prop_mrmr       = 0.50,
                            max_mrmr        = 120,
                            seed            = 42) {
  
  n_mrmr <- if (!is.null(n_features_mrmr)) {
    n_features_mrmr
    } else {
    min(round(length(vars_candidatas) * prop_mrmr), max_mrmr)
  }
  
  message('mRMR: selecionando ', n_mrmr, ' de ', length(vars_candidatas), ' candidatas')
  
  df_fs <- df_dev |>
    select(all_of(vars_candidatas), target) |>
    mutate(across(everything(),
                  ~ as.numeric(.) |> (\(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))()))
  
  set.seed(seed)
  mrmr_data   <- mRMR.data(data = as.data.frame(df_fs))
  mrmr_result <- mRMR.ensemble(
    data           = mrmr_data,
    target_indices = which(names(df_fs) == 'target'),
    solution_count = 1,
    feature_count  = min(n_mrmr, length(vars_candidatas))
  )
  vars_mrmr <- names(df_fs)[mrmr_result@filters[[1]]]
  message('Selecionadas pelo mRMR: ', length(vars_mrmr))
  
  message('Calculando Lasso...')
  set.seed(seed)
  cv_lasso <- cv.glmnet(
    x = as.matrix(df_fs[vars_mrmr]),
    y = df_fs$target,
    family = 'binomial', alpha = 1,
    nfolds = 5, type.measure = 'auc'
  )
  coefs      <- coef(cv_lasso, s = 'lambda.1se')
  vars_lasso <- setdiff(rownames(coefs)[coefs[, 1] != 0], '(Intercept)')
  message('Selecionadas pelo Lasso: ', length(vars_lasso))
  vars_lasso
}

treinar_lgbm <- function(dev, val, vars_finais,
                         seed            = 18692,
                         learning_rate   = 0.05,
                         num_leaves      = 31,
                         min_data_in_leaf = 20,
                         feature_fraction = 0.8,
                         bagging_fraction = 0.8,
                         bagging_freq     = 5,
                         nrounds          = 1000,
                         early_stop       = 50) {
  features <- intersect(vars_finais, names(dev))
  
  ddev <- lgb.Dataset(
    data   = as.matrix(dev[features] |> mutate(across(everything(), as.numeric))),
    label  = dev$target,
    weight = dev$S017
  )
  dval <- lgb.Dataset(
    data      = as.matrix(val[features] |> mutate(across(everything(), as.numeric))),
    label     = val$target,
    weight    = val$S017,
    reference = ddev
  )
  
  set.seed(seed)
  modelo <- lgb.train(
    params = list(
      objective        = 'binary',
      metric           = 'auc',
      learning_rate    = learning_rate,
      num_leaves       = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      feature_fraction = feature_fraction,
      bagging_fraction = bagging_fraction,
      bagging_freq     = bagging_freq,
      seed             = seed,
      verbose          = -1
    ),
    data                  = ddev,
    nrounds               = nrounds,
    valids                = list(val = dval),
    early_stopping_rounds = early_stop,
    eval_freq             = 50
  )
  
  message('Melhor iteração: ', modelo$best_iter,
          ' | AUC val: ', round(modelo$best_score, 4))
  list(modelo = modelo, features = features)
}

calcular_ks <- function(target, score) {
  ord    <- order(score, decreasing = TRUE)
  target <- target[ord]
  n_pos  <- sum(target)
  n_neg  <- sum(1 - target)
  tpr    <- cumsum(target) / n_pos
  fpr    <- cumsum(1 - target) / n_neg
  max(abs(tpr - fpr))
}

avaliar_particao <- function(modelo_obj, df, label = 'OOS') {
  x    <- as.matrix(df[modelo_obj$features] |> mutate(across(everything(), as.numeric)))
  pred <- predict(modelo_obj$modelo, x)
  auc  <- as.numeric(pROC::auc(pROC::roc(df$target, pred, quiet = TRUE)))
  ks   <- calcular_ks(df$target, pred)
  message(label, ': AUC: ', round(auc, 4), ' | KS: ', round(ks, 4))
  list(auc = auc, ks = ks, pred = pred)
}

calcular_shap <- function(modelo_obj, oos) {
  x_oos <- as.matrix(oos[modelo_obj$features] |> mutate(across(everything(), as.numeric)))
  shapviz(modelo_obj$modelo, X_pred = x_oos, X = x_oos)
}

adicionar_geo <- function(resultados, waves = c('W5', 'W6', 'W7')) {
  for (w in waves) {
    for (part in c('dev', 'val', 'oos')) {
      resultados[[w]]$splits[[part]] <- resultados[[w]]$splits[[part]] |>
        mutate(
          region    = countrycode(COUNTRY_ALPHA, 'iso3c', 'region'),
          continent = countrycode(COUNTRY_ALPHA, 'iso3c', 'continent')
        )
    }
  }
  resultados
}

salvar_resultados <- function(resultados, path_outputs, path_models,
                              waves = c('W5', 'W6', 'W7')) {
  dir.create(path_outputs, showWarnings = FALSE, recursive = TRUE)
  dir.create(path_models,  showWarnings = FALSE, recursive = TRUE)
  
  saveRDS(resultados, file.path(path_outputs, 'resultados_lgbm.rds'))
  message('resultados_lgbm.rds salvo.')
  
  for (w in waves) {
    lgb.save(resultados[[w]]$modelo$modelo,
             file.path(path_models, paste0('modelo_', tolower(w), '.txt')))
    message('modelo_', tolower(w), '.txt salvo.')
  }
}

pipeline_wvs <- function(base, waves       = c(5, 6, 7),
                         vars_excluir      = VARS_EXCLUIR,
                         prop_oos          = PROP_OOS,
                         prop_val          = PROP_VAL,
                         seed_split        = SEED_SPLIT,
                         miss_threshold    = MISS_THRESHOLD,
                         freq_threshold    = FREQ_THRESHOLD,
                         iv_threshold      = IV_THRESHOLD,
                         cv_threshold      = CV_THRESHOLD,
                         prop_mrmr         = PROP_MRMR,
                         max_mrmr          = MAX_MRMR,
                         seed_lgb          = SEED_LGB,
                         path_outputs      = PATH_OUTPUTS,
                         path_models       = PATH_MODELS) {
  
  # Verificando existência de outputs
  path_rds    <- file.path(path_outputs, 'resultados_lgbm.rds')
  paths_txt   <- file.path(path_models, paste0('modelo_w', waves, '.txt'))
  tudo_existe <- file.exists(path_rds) && all(file.exists(paths_txt))
  
  if (tudo_existe) {
    message('Outputs encontrados: carregando sem re-rodar o pipeline.')
    resultados <- readRDS(path_rds)
    for (w in paste0('W', waves))
      resultados[[w]]$modelo$modelo <- lgb.load(
        file.path(path_models, paste0('modelo_', tolower(w), '.txt'))
      )
    return(resultados)
  }
  
  message('Outputs não encontrados: iniciando pipeline.')
  resultados <- list()
  
  for (wave in waves) {
    message('\n===== WAVE ', wave, ' =====')
    
    df_wave     <- preparar_wave(base, wave, vars_excluir)
    splits      <- fazer_split(df_wave, prop_oos, prop_val, seed_split)
    
    vars_ok     <- filtros_qualidade(splits$dev, miss_threshold, freq_threshold)
    univ        <- fs_univariado(splits$dev, vars_ok, iv_threshold, cv_threshold)
    vars_finais <- fs_multivariado(splits$dev, univ$variavel, prop_mrmr = prop_mrmr, max_mrmr = max_mrmr)
    
    modelo_obj  <- treinar_lgbm(splits$dev, splits$val, vars_finais, seed = seed_lgb)
    
    resultados[[paste0('W', wave)]] <- list(
      splits      = splits,
      vars_finais = vars_finais,
      univ        = univ,
      modelo      = modelo_obj,
      metricas    = list(
        dev = avaliar_particao(modelo_obj, splits$dev, 'DEV'),
        val = avaliar_particao(modelo_obj, splits$val, 'VAL'),
        oos = avaliar_particao(modelo_obj, splits$oos, 'OOS')
      ),
      shap = calcular_shap(modelo_obj, splits$oos)
    )
  }
  
  resultados <- adicionar_geo(resultados, paste0('W', waves))
  salvar_resultados(resultados, path_outputs, path_models, paste0('W', waves))
  resultados
}