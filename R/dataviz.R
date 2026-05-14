# =========================================================================== #
#                        Funções de Visualização - WVS                        #
# =========================================================================== #

# ── Funções auxiliares ───────────────────────────────────────────────────────

importancia_pct <- function(modelo_obj, label = 'W5') {
  lgb.importance(modelo_obj$modelo) |>
    as_tibble() |>
    mutate(pct = Gain / sum(Gain) * 100, pct_acum = cumsum(pct), wave = label) |>
    select(Feature, Gain, pct, pct_acum, wave)
}

avaliar_particoes <- function(modelo_obj, splits, label = 'W5') {
  predict_part <- function(df) {
    x <- as.matrix(df[modelo_obj$features] |> mutate(across(everything(), as.numeric)))
    predict(modelo_obj$modelo, x)
  }
  tibble(
    wave     = label,
    particao = c('DEV', 'VAL', 'OOS'),
    ks  = c(calcular_ks(splits$dev$target, predict_part(splits$dev)),
            calcular_ks(splits$val$target, predict_part(splits$val)),
            calcular_ks(splits$oos$target, predict_part(splits$oos))),
    auc = c(
      as.numeric(pROC::auc(pROC::roc(splits$dev$target, predict_part(splits$dev), quiet = TRUE))),
      as.numeric(pROC::auc(pROC::roc(splits$val$target, predict_part(splits$val), quiet = TRUE))),
      as.numeric(pROC::auc(pROC::roc(splits$oos$target, predict_part(splits$oos), quiet = TRUE)))
    )
  )
}

ks_por_regiao <- function(splits_oos, pred, label = 'W5') {
  splits_oos |>
    mutate(score = pred) |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarise(n   = n(), n_pos = sum(target),
              ks  = calcular_ks(target, score),
              auc = as.numeric(pROC::auc(pROC::roc(target, score, quiet = TRUE))),
              .groups = 'drop') |>
    arrange(desc(ks)) |>
    mutate(wave = label)
}

tabela_decis <- function(df_oos, pred, target_col = 'target', label = 'W5') {
  df_oos |>
    mutate(score = pred, decil = ntile(-score, 10)) |>
    group_by(decil) |>
    summarise(n = n(), n_pobres = sum(.data[[target_col]]),
              taxa_pob = mean(.data[[target_col]]),
              score_min = min(score), score_max = max(score),
              .groups = 'drop') |>
    arrange(decil) |>
    mutate(wave = label)
}

calcular_gains <- function(target, score, label = 'W5') {
  ord    <- order(score, decreasing = TRUE)
  target <- target[ord]
  n      <- length(target)
  n_pos  <- sum(target)
  tibble(pct_pop = seq_len(n) / n,
         gains   = cumsum(target) / n_pos,
         lift    = (cumsum(target) / seq_len(n)) / (n_pos / n),
         wave    = label) |>
    filter(row_number() %% 100 == 0 | row_number() == n)
}

calcular_calibracao <- function(target, score, n_bins = 10, label = 'W5') {
  tibble(target = target, score = score) |>
    mutate(bin = ntile(score, n_bins)) |>
    group_by(bin) |>
    summarise(score_medio = mean(score), taxa_obs = mean(target),
              n = n(), .groups = 'drop') |>
    mutate(wave = label)
}

dist_scores <- function(splits_oos, pred, label = 'W5') {
  tibble(score  = pred,
         classe = factor(splits_oos$target, levels = c(1, 0),
                         labels = c('Pobre', 'Não-pobre')),
         wave   = label)
}

taxa_regiao <- function(splits_oos, pred, label = 'W5') {
  splits_oos |>
    mutate(score = pred) |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarise(taxa_obs = mean(target), score_medio = mean(score),
              n = n(), .groups = 'drop') |>
    pivot_longer(c(taxa_obs, score_medio), names_to = 'tipo', values_to = 'valor') |>
    mutate(wave = label,
           tipo = recode(tipo, 'taxa_obs' = 'Observada', 'score_medio' = 'Predita'))
}

shap_comparison <- function(shap_a, shap_b, label_a = 'W5', label_b = 'W6') {
  imp_a <- tibble(variavel = colnames(shap_a$S), !!label_a := colMeans(abs(shap_a$S)))
  imp_b <- tibble(variavel = colnames(shap_b$S), !!label_b := colMeans(abs(shap_b$S)))
  full_join(imp_a, imp_b, by = 'variavel') |>
    replace_na(setNames(list(0, 0), c(label_a, label_b))) |>
    mutate(delta    = .data[[label_b]] - .data[[label_a]],
           destaque = if_else(abs(delta) > quantile(abs(delta), 0.75), variavel, ''))
}

threshold_ks <- function(target, score) {
  ord    <- order(score, decreasing = TRUE)
  target <- target[ord]
  score  <- score[ord]
  n_pos  <- sum(target)
  n_neg  <- sum(1 - target)
  idx    <- which.max(abs(cumsum(target) / n_pos - cumsum(1 - target) / n_neg))
  score[idx]
}

classificar_erros <- function(splits_oos, pred, label = 'W5') {
  thr <- threshold_ks(splits_oos$target, pred)
  message(label, ': Threshold KS: ', round(thr, 4))
  splits_oos |>
    mutate(score    = pred,
           pred_bin = if_else(score >= thr, 1L, 0L),
           tipo     = case_when(target == 1 & pred_bin == 1 ~ 'VP',
                                target == 1 & pred_bin == 0 ~ 'FN',
                                target == 0 & pred_bin == 1 ~ 'FP',
                                target == 0 & pred_bin == 0 ~ 'VN'),
           wave = label, threshold = thr)
}

plot_metrica <- function(var, titulo, metricas_regiao, metricas_global) {
  ggplot() +
    geom_line(data  = metricas_regiao,
              aes(x = wave, y = .data[[var]], color = region, group = region),
              linewidth = 0.8) +
    geom_point(data = metricas_regiao,
               aes(x = wave, y = .data[[var]], color = region, group = region),
               size = 2) +
    geom_line(data      = metricas_global,
              aes(x = wave, y = .data[[var]], group = 1),
              linewidth = 1.2, linetype = 'dashed', color = 'black') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = titulo, x = 'Wave', y = '', color = 'Região') +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = 'none')
}

# ── Orquestrador ─────────────────────────────────────────────────────────────

plotar_wvs <- function(resultados,
                       waves       = c('W5', 'W6', 'W7'),
                       path_plots  = PATH_PLOTS,
                       salvar      = TRUE) {
  
  dir.create(path_plots, showWarnings = FALSE, recursive = TRUE)
  
  salvar_plot <- function(p, nome, w = 14, h = 8) {
    if (salvar) ggsave(file.path(path_plots, nome), p, width = w, height = h, dpi = 150)
    p
  }
  
  # ── Pré-computação ──────────────────────────────────────────────────────
  
  imp   <- lapply(waves, function(w) importancia_pct(resultados[[w]]$modelo, w))
  names(imp) <- waves
  
  pontos_ref <- bind_rows(imp) |>
    group_by(wave) |>
    mutate(rank = row_number()) |>
    summarise(n_50 = min(rank[pct_acum >= 50]),
              n_80 = min(rank[pct_acum >= 80]), .groups = 'drop')
  
  ks_part <- bind_rows(lapply(waves, function(w)
    avaliar_particoes(resultados[[w]]$modelo, resultados[[w]]$splits, w)))
  
  ks_reg <- lapply(waves, function(w)
    ks_por_regiao(resultados[[w]]$splits$oos, resultados[[w]]$metricas$oos$pred, w))
  names(ks_reg) <- waves
  
  decis <- lapply(waves, function(w)
    tabela_decis(resultados[[w]]$splits$oos, resultados[[w]]$metricas$oos$pred, label = w))
  
  gains <- lapply(waves, function(w)
    calcular_gains(resultados[[w]]$splits$oos$target,
                   resultados[[w]]$metricas$oos$pred, w))
  
  cal <- lapply(waves, function(w)
    calcular_calibracao(resultados[[w]]$splits$oos$target,
                        resultados[[w]]$metricas$oos$pred, label = w))
  
  df_dist <- bind_rows(lapply(waves, function(w)
    dist_scores(resultados[[w]]$splits$oos, resultados[[w]]$metricas$oos$pred, w)))
  
  roc <- lapply(waves, function(w)
    pROC::roc(resultados[[w]]$splits$oos$target,
              resultados[[w]]$metricas$oos$pred, quiet = TRUE))
  names(roc) <- waves
  
  erros    <- lapply(waves, function(w)
    classificar_erros(resultados[[w]]$splits$oos, resultados[[w]]$metricas$oos$pred, w))
  df_erros <- bind_rows(erros)
  
  erros_regiao <- df_erros |>
    filter(!is.na(region), tipo %in% c('FN', 'FP')) |>
    count(wave, region, tipo) |>
    group_by(wave, region) |>
    mutate(total_regiao = sum(n), pct = n / total_regiao * 100) |>
    ungroup()
  
  metricas_regiao <- df_erros |>
    filter(!is.na(region)) |>
    group_by(wave, region) |>
    summarise(VP = sum(tipo == 'VP'), FN = sum(tipo == 'FN'),
              FP = sum(tipo == 'FP'), VN = sum(tipo == 'VN'), .groups = 'drop') |>
    mutate(miss_rate   = FN / (FN + VP), fall_out    = FP / (FP + VN),
           precision   = VP / (VP + FP), recall      = VP / (VP + FN),
           f1          = 2 * precision * recall / (precision + recall),
           specificity = VN / (VN + FP))
  
  metricas_global <- df_erros |>
    group_by(wave) |>
    summarise(VP = sum(tipo == 'VP'), FN = sum(tipo == 'FN'),
              FP = sum(tipo == 'FP'), VN = sum(tipo == 'VN'), .groups = 'drop') |>
    mutate(miss_rate   = FN / (FN + VP), fall_out    = FP / (FP + VN),
           precision   = VP / (VP + FP), recall      = VP / (VP + FN),
           f1          = 2 * precision * recall / (precision + recall),
           specificity = VN / (VN + FP))
  
  # ── Visualizações ───────────────────────────────────────────────────────
  
  # A: Importância acumulada
  p_a <- bind_rows(imp) |>
    group_by(wave) |> mutate(rank = row_number()) |>
    ggplot(aes(x = rank, y = pct_acum, color = wave)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = c(50, 80), linetype = 'dashed', color = 'grey60') +
    geom_vline(data = pontos_ref, aes(xintercept = n_50, color = wave),
               linetype = 'dotted', linewidth = 0.8) +
    geom_vline(data = pontos_ref, aes(xintercept = n_80, color = wave),
               linetype = 'dotted', linewidth = 0.8) +
    geom_label(data = pontos_ref,
               aes(x = n_50, y = 50, label = paste0(n_50, ' atr.'), color = wave),
               vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
    geom_label(data = pontos_ref,
               aes(x = n_80, y = 80, label = paste0(n_80, ' atr.'), color = wave),
               vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = 'Concentração de importância acumulada: Gain',
         subtitle = 'Linhas tracejadas em 50% e 80%',
         x = 'Número de atributos', y = '% acumulada', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_a, 'A_importancia_acumulada.png')
  
  # B: SHAP beeswarm
  shap_plots <- lapply(waves, function(w)
    sv_importance(resultados[[w]]$shap, kind = 'beeswarm', max_display = 20))
  names(shap_plots) <- waves
  p_b <- (shap_plots[[1]] + shap_plots[[2]] + shap_plots[[3]]) +
    plot_layout(ncol = 3) +
    plot_annotation(title = 'Importância SHAP por wave: OOS',
                    subtitle = 'Top 20 | Cor = valor da feature')
  salvar_plot(p_b, 'B_shap_beeswarm.png', w = 20, h = 8)
  
  # C: Comparação SHAP
  pares <- list(c(waves[1], waves[2]), c(waves[2], waves[3]), c(waves[1], waves[3]))
  for (par in pares) {
    df_comp <- shap_comparison(resultados[[par[1]]]$shap, resultados[[par[2]]]$shap,
                               par[1], par[2])
    la <- par[1]; lb <- par[2]
    p_c <- df_comp |>
      filter(.data[[la]] > 0 | .data[[lb]] > 0) |>
      slice_max(abs(delta), n = 30) |>
      ggplot(aes(x = reorder(variavel, delta), y = delta, fill = delta > 0)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c('TRUE' = '#4575b4', 'FALSE' = '#d73027'),
                        labels = c('TRUE' = paste('Subiu na', lb),
                                   'FALSE' = paste('Caiu na', lb))) +
      labs(title = paste0('Variação SHAP: ', lb, ' - ', la),
           x = '', y = 'Δ SHAP', fill = '') +
      theme_minimal()
    salvar_plot(p_c, paste0('C_shap_delta_', la, '_', lb, '.png'))
  }
  
  # D: ROC
  p_d <- pROC::ggroc(setNames(roc, waves)) +
    geom_abline(slope = 1, intercept = 1, linetype = 'dashed', color = 'grey') +
    labs(title    = 'Curva ROC: OOS',
         subtitle = paste(sapply(waves, function(w)
           paste0(w, ' AUC: ', round(pROC::auc(roc[[w]]), 3))), collapse = ' | '),
         x = '1 - Especificidade', y = 'Sensibilidade', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_d, 'D_roc.png')
  
  # E: KS — tabelas no console
  message('\n── KS e AUC por partição ──')
  ks_part |>
    pivot_wider(names_from = particao, values_from = c(ks, auc)) |>
    (\(df) {
      cat('\nKS\n')
      cat(sprintf('       %8s %8s %8s\n', 'DEV', 'VAL', 'OOS'))
      for (i in seq_len(nrow(df)))
        cat(sprintf('  %-4s %8.4f %8.4f %8.4f\n',
                    df$wave[i], df$ks_DEV[i], df$ks_VAL[i], df$ks_OOS[i]))
      cat('\nAUC\n')
      cat(sprintf('       %8s %8s %8s\n', 'DEV', 'VAL', 'OOS'))
      for (i in seq_len(nrow(df)))
        cat(sprintf('  %-4s %8.4f %8.4f %8.4f\n',
                    df$wave[i], df$auc_DEV[i], df$auc_VAL[i], df$auc_OOS[i]))
    })()
  
  # F: Decis
  p_f <- bind_rows(decis) |>
    ggplot(aes(x = decil, y = taxa_pob, color = wave, group = wave)) +
    geom_line(linewidth = 1) + geom_point(size = 3) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = 'Taxa de pobreza por decil de score: OOS',
         subtitle = 'Decil 1 = maior score', x = 'Decil', y = 'Taxa observada', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_f, 'F_decis.png')
  
  # G: Gains e Lift
  df_gains <- bind_rows(gains)
  p_g1 <- ggplot(df_gains, aes(x = pct_pop, y = gains, color = wave)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey60') +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = 'Curva de Gains: OOS', x = '% população', y = '% pobres capturados', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_g1, 'G1_gains.png')
  
  p_g2 <- ggplot(df_gains, aes(x = pct_pop, y = lift, color = wave)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 1, linetype = 'dashed', color = 'grey60') +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(title = 'Curva de Lift: OOS', x = '% população', y = 'Lift', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_g2, 'G2_lift.png')
  
  # H: Calibração
  p_h <- bind_rows(cal) |>
    ggplot(aes(x = score_medio, y = taxa_obs, color = wave)) +
    geom_point(aes(size = n), alpha = 0.8) + geom_line(linewidth = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey60') +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = 'Calibração: OOS', x = 'Score médio predito', y = 'Taxa observada', color = 'Wave') +
    theme_minimal()
  salvar_plot(p_h, 'H_calibracao.png')
  
  # I: Distribuição scores por classe
  p_i <- ggplot(df_dist, aes(x = score, fill = classe)) +
    geom_density(alpha = 0.5) + facet_wrap(~wave) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('Pobre' = '#ff7f00', 'Não-pobre' = '#7fbf7b')) +
    labs(title = 'Distribuição de scores por classe: OOS',
         x = 'Score predito', y = 'Densidade', fill = 'Classe') +
    theme_minimal()
  salvar_plot(p_i, 'I_dist_scores.png', w = 16, h = 6)
  
  # J: KS por região
  p_j <- bind_rows(ks_reg) |>
    ggplot(aes(x = reorder(region, ks), y = ks, fill = wave)) +
    geom_col(position = 'dodge') + coord_flip() +
    scale_fill_manual(values = c('W5' = '#762a83', 'W6' = '#1b7837', 'W7' = '#377eb8')) +
    labs(title = 'KS por região: OOS', x = '', y = 'KS', fill = 'Wave') +
    theme_minimal()
  salvar_plot(p_j, 'J_ks_regiao.png')
  
  # K: Taxa observada vs predita
  p_k <- bind_rows(lapply(waves, function(w)
    taxa_regiao(resultados[[w]]$splits$oos, resultados[[w]]$metricas$oos$pred, w))) |>
    ggplot(aes(x = reorder(region, valor), y = valor, fill = tipo)) +
    geom_col(position = 'dodge') + facet_wrap(~wave) + coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('Observada' = '#d73027', 'Predita' = '#4575b4')) +
    labs(title = 'Taxa observada vs predita por região: OOS', x = '', y = '', fill = '') +
    theme_minimal()
  salvar_plot(p_k, 'K_taxa_regiao.png', w = 16, h = 7)
  
  # L: Matriz de confusão
  message('\n── Matriz de confusão ──')
  df_erros |>
    count(wave, tipo) |>
    group_by(wave) |>
    mutate(pct = n / sum(n) * 100) |>
    pivot_wider(names_from = tipo, values_from = c(n, pct)) |>
    print()
  
  # M: FN e FP por região
  p_m <- ggplot(erros_regiao, aes(x = reorder(region, pct), y = pct, fill = tipo)) +
    geom_col(position = 'dodge') + facet_wrap(~wave) + coord_flip() +
    scale_fill_manual(values = c('FN' = '#d73027', 'FP' = '#4575b4'),
                      labels  = c('FN' = 'Falso Negativo', 'FP' = 'Falso Positivo')) +
    labs(title = 'Taxa de erros por região: OOS', x = '', y = '% na região', fill = '') +
    theme_minimal()
  salvar_plot(p_m, 'M_erros_regiao.png', w = 16, h = 7)
  
  # N: FN vs VP — scores dos pobres
  p_n <- df_erros |>
    filter(target == 1, tipo %in% c('VP', 'FN')) |>
    ggplot(aes(x = score, fill = tipo)) +
    geom_density(alpha = 0.5) + facet_wrap(~wave) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('VP' = '#4575b4', 'FN' = '#d73027'),
                      labels  = c('VP' = 'Identificado', 'FN' = 'Não identificado')) +
    labs(title = 'Distribuição de scores: pobres identificados vs não identificados',
         x = 'Score', y = 'Densidade', fill = '') +
    theme_minimal()
  salvar_plot(p_n, 'N_fn_vs_vp.png', w = 16, h = 6)
  
  # O: Evolução de FN por região
  p_o <- df_erros |>
    filter(!is.na(region)) |>
    group_by(wave, region) |>
    summarise(taxa_fn = mean(tipo == 'FN' & target == 1), .groups = 'drop') |>
    ggplot(aes(x = wave, y = taxa_fn, color = region, group = region)) +
    geom_line(linewidth = 1) + geom_point(size = 3) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = 'Evolução da taxa de FN por região',
         x = 'Wave', y = 'Taxa de FN', color = 'Região') +
    theme_minimal()
  salvar_plot(p_o, 'O_fn_evolucao.png')
  
  # P: Métricas de confusão 2x3
  pm <- list(
    plot_metrica('miss_rate',   'Taxa de Falso Negativo (Miss Rate)',   metricas_regiao, metricas_global),
    plot_metrica('fall_out',    'Taxa de Falso Positivo (Fall-Out)',    metricas_regiao, metricas_global),
    plot_metrica('precision',   'Taxa de Precisão (PPV)',               metricas_regiao, metricas_global),
    plot_metrica('recall',      'Taxa de Recall (Sensibilidade)',       metricas_regiao, metricas_global),
    plot_metrica('f1',          'Score F1',                             metricas_regiao, metricas_global),
    plot_metrica('specificity', 'Taxa de Especificidade (TNR)',         metricas_regiao, metricas_global)
  )
  legenda_base <- ggplot(metricas_regiao,
                         aes(x = wave, y = miss_rate, color = region, group = region)) +
    geom_line() + geom_point() +
    geom_line(data = metricas_global, aes(x = wave, y = miss_rate, group = 1),
              linetype = 'dashed', color = 'black') +
    labs(color = 'Região') + theme_minimal() +
    theme(legend.position = 'bottom', legend.title = element_text(face = 'bold'))
  
  p_p <- cowplot::plot_grid(
    (pm[[1]] + pm[[2]] + pm[[3]]) / (pm[[4]] + pm[[5]] + pm[[6]]),
    cowplot::get_legend(legenda_base),
    ncol = 1, rel_heights = c(1, 0.12)
  )
  salvar_plot(p_p, 'P_metricas_confusao.png', w = 18, h = 10)
  
  message('\nTodas as visualizações geradas em: ', path_plots)
  invisible(NULL)
}