# =========================================================================== #
#                              Dashboard dos WVS                              #
# =========================================================================== #

dashboard_wvs <- function(df) {
  
  
  # 1. PRÉ-PROCESSAMENTO ======================================================
  
  wave_labels <- c('1' = 'W1 (1981–1984)', '2' = 'W2 (1990–1994)', '3' = 'W3 (1995–1998)', '4' = 'W4 (1999–2004)',
                   '5' = 'W5 (2005–2009)', '6' = 'W6 (2010–2014)', '7' = 'W7 (2017–2022)')
  
  # Criando o objeto necessário
  wvs <- df |>
    mutate(
      # Identificação
      wave_num   = as.integer(S002VS),
      wave       = factor(wave_num, levels = 1:7, labels = unname(wave_labels)),
      
      # Renda
      income     = as.numeric(X047_WVS),
      income_grp = cut(income,
                       breaks         = c(0, 1, 3, 6, 8, 10),
                       labels         = c('Pobre','Vulnerável','Classe Média','Independente','Rico'),
                       include.lowest = TRUE),
      
      # Sociodemográficas
      sex        = haven::as_factor(X001),
      age_raw    = as.numeric(X003),
      age_grp    = cut(age_raw,
                       breaks = c(17, 24, 29, 39, 49, 59, Inf),
                       labels = c('18–24', '25–29', '30–39', '40–49', '50–59', '60+'),
                       right  = TRUE),
      marital    = haven::as_factor(X007),
      marital    = forcats::fct_collapse(haven::as_factor(X007),
                                         'Casado'    = 'Married',
                                         'Em união'  = 'Living together as married',
                                         'Solteiro'  = 'Single/Never married',
                                         'Separado'  = c('Separated', 'Divorced'),
                                         'Viúvo'     = 'Widowed'
      ),
      
      hh_size    = cut(as.numeric(X013),
                       breaks = c(-Inf, 0, 1, 3, 6, Inf),
                       labels = c('0', '1', '2–3', '4–6', '7+'),
                       right  = TRUE),
      literate   = haven::as_factor(X024B),
      education  = haven::as_factor(X025),
      education = forcats::fct_collapse(education,
                                        'Fundam. incompleto'    = 'Inadequately completed elementary education',
                                        'Fundam. completo'      = 'Completed (compulsory) elementary education',
                                        'Médio incompleto'    = c(
                                          'Incomplete secondary school: technical/vocational type/(Compulsory) elementary education and basic vocational qualificat',
                                          'Incomplete secondary: university-preparatory type/Secondary, intermediate general qualification'
                                        ),
                                        'Médio completo'      = c(
                                          'Complete secondary school: technical/vocational type/Secondary, intermediate vocational qualification',
                                          'Complete secondary: university-preparatory type/Full secondary, maturity level certificate'
                                        ),
                                        'Superior incompleto' = 'Some university without degree/Higher education - lower-level tertiary certificate',
                                        'Superior completo'   = 'University with degree/Higher education - upper-level tertiary certificate'
      ),
      
      emp_status = haven::as_factor(X028),
      emp_status = forcats::fct_collapse(haven::as_factor(X028),
                                         'Empregado'    = 'Full time',
                                         'Part-Time'    = 'Part time',
                                         'Autônomo'     = 'Self employed',
                                         'Desempregado' = 'Unemployed',
                                         'Aposentado'   = 'Retired',
                                         'Do lar'       = 'Housewife',
                                         'Estudante'    = 'Students',
                                         'Outro'        = 'Other'
      ),
      
      
      # Escalas de valores
      postmat    = as.numeric(Y001),
      autonomy   = as.numeric(Y003),
      secular    = as.numeric(Y010),
      
      # Bem-estar (escalas originais
      happiness = (4 - as.numeric(A008)) / 3,   # original: 1=melhor → 4=pior
      life_sat  = (as.numeric(A170) - 1) / 9,   # original: 1=pior → 10=melhor
      freedom   = (as.numeric(A173) - 1) / 9,   # original: 1=pior → 10=melhor
      fin_sat   = (as.numeric(C006) - 1) / 9,   # original: 1=pior → 10=melhor
      
      # Importâncias: original 1=muito → 4=nada; novo 0=nada → 1=muito
      imp_family   = (4 - as.numeric(A001)) / 3,
      imp_friends  = (4 - as.numeric(A002)) / 3,
      imp_leisure  = (4 - as.numeric(A003)) / 3,
      imp_politics = (4 - as.numeric(A004)) / 3,
      imp_work     = (4 - as.numeric(A005)) / 3,
      imp_religion = (4 - as.numeric(A006)) / 3,
      
      # Político (todos convertidos para 0–1)
      pol_interest = (4  - as.numeric(E023)) / 3,        # 1=muito → 1, 4=nenhum → 0
      pol_lr       = (as.numeric(E033) - 1)  / 9,        # 1=esq → 0, 10=dir → 1
      inc_equal    = (10 - as.numeric(E035)) / 9,        # 1=igualdade → 1, 10=desig → 0
      
      # Espectro político
      pol_spectrum = cut(as.numeric(E033),
                         breaks         = c(0, 2, 4, 6, 8, 10),
                         labels         = c('Esquerda', 'Centro-Esquerda', 'Centro',
                                            'Centro-Direita', 'Direita'),
                         include.lowest = TRUE),
      
      # Inseguranças: original 1=frequente → 4=nunca; novo 0=nunca → 1=frequente
      insec_food   = (4 - as.numeric(H008_01)) / 3,
      insec_crime  = (4 - as.numeric(H008_02)) / 3,
      insec_health = (4 - as.numeric(H008_03)) / 3,
      insec_income = (4 - as.numeric(H008_04)) / 3,
      
      # Nome do país (coalesce para cobrir códigos não-padrão da WVS)
      country_name = coalesce(
        countrycode(COUNTRY_ALPHA, origin = 'iso3c', destination = 'country.name'),
        COUNTRY_ALPHA
      )
    ) |>
    mutate(
      autonomy = (autonomy + 2) / 4,
      postmat  = (postmat - min(postmat, na.rm = TRUE)) /
        (max(postmat, na.rm = TRUE) - min(postmat, na.rm = TRUE)),
      education = forcats::fct_relevel(education,
                                       'Fundam. incompleto',  'Fundam. completo',
                                       'Médio incompleto',    'Médio completo',
                                       'Superior incompleto', 'Superior completo'
      )
    ) |>
    filter(!is.na(wave_num))
  
  # Criando objetos secundários
  country_lookup <- wvs |>
    distinct(COUNTRY_ALPHA, country_name) |>
    arrange(country_name)
  
  continents_list <- sort(unique(na.omit(wvs$continent)))
  regions_list    <- sort(unique(na.omit(wvs$region)))
  age_grp_list    <- c('18–24', '25–29', '30–39', '40–49', '50–59', '60+')
  sex_list        <- levels(wvs$sex)
  edu_list        <- levels(wvs$education)
  marital_list    <- levels(wvs$marital)
  spectrum_list   <- c('Esquerda', 'Centro-Esquerda', 'Centro', 'Centro-Direita', 'Direita')
  wave_choices    <- setNames(1:7, unname(wave_labels))
  
  imp_vars   <- c('Família'  = 'imp_family',  'Amigos'   = 'imp_friends',
                  'Lazer'    = 'imp_leisure', 'Política' = 'imp_politics',
                  'Trabalho' = 'imp_work',    'Religião' = 'imp_religion')
  insec_vars <- c('Alimentar'  = 'insec_food',   'Crime'      = 'insec_crime',
                  'Saúde'      = 'insec_health', 'Financeira' = 'insec_income')
  pol_vars   <- c('Interesse político' = 'pol_interest',
                  'Posição esq–dir'    = 'pol_lr',
                  'Apoio à igualdade'  = 'inc_equal')
  
  
  # 2. HELPERS ================================================================
  
  # Pesos amostrais
  wmean <- function(x, w) weighted.mean(x, w = w, na.rm = TRUE)
  
  # Paletas de cores
  pal_income   <- c('Pobre'        = '#762a83',  # roxo escuro
                    'Vulnerável'   = '#af8dc3',  # lilás médio
                    'Classe Média' = '#8da9c4',  # cinza neutro
                    'Independente' = '#7fbf7b',  # verde médio
                    'Rico'         = '#1b7837'   # verde escuro
  )
  pal_spectrum <- c('Esquerda'        = '#d73027', 'Centro-Esquerda' = '#fc8d59',
                    'Centro'          = '#ffdfcf', 'Centro-Direita'  = '#91bfdb',
                    'Direita'         = '#4575b4')
  pal_insec    <- c('Alimentar' = '#e41a1c', 'Crime'      = '#ff7f00',
                    'Saúde'     = '#4daf4a', 'Financeira' = '#377eb8')
  
  plotly_theme <- function(p) {
    p |> layout(
      font          = list(family = 'Arial, sans-serif', size = 12),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor  = 'rgba(0,0,0,0)',
      legend        = list(orientation = 'h', y = -0.25)
    )
  }
  
  # Barras por faixa de renda agrupada
  bar_by_income <- function(df_filt, var, label_y) {
    df <- df_filt |>
      filter(!is.na(income_grp)) |>
      group_by(income_grp) |>
      summarise(media = wmean(.data[[var]], S017), .groups = 'drop')
    
    plot_ly(df, x = ~income_grp, y = ~media, color = ~income_grp,
            colors = pal_income, type = 'bar', showlegend = FALSE) |>
      layout(xaxis = list(title = ''), yaxis = list(title = label_y)) |>
      plotly_theme()
  }
  
  # Linhas por wave segmentadas por grupo
  seg_line_wave <- function(df, var, group_var,
                            title_y = 'Média (0–1)',
                            y_range = NULL,
                            colors  = NULL) {
    df_sum <- df |>
      filter(!is.na(.data[[group_var]]), !is.na(wave)) |>
      group_by(wave, grupo = as.character(.data[[group_var]])) |>
      summarise(media = wmean(.data[[var]], S017), .groups = 'drop')
    
    p <- plot_ly(df_sum, x = ~wave, y = ~media, color = ~grupo,
                 type = 'scatter', mode = 'lines+markers')
    
    if (!is.null(colors)) {
      grps_presentes <- intersect(unique(df_sum$grupo), names(colors))
      if (length(grps_presentes) > 0)
        p <- p |> layout(colorway = unname(colors[grps_presentes]))
    }
    
    p |> layout(
      xaxis = list(title = 'Wave'),
      yaxis = list(title = title_y, range = y_range)
    ) |> plotly_theme() |>
      layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left', y = 0.5))
  }
  
  # 100% barras empilhadas por wave
  pct_bar_wave <- function(df_filt, group_var) {
    df <- df_filt |>
      filter(!is.na(wave), !is.na(.data[[group_var]])) |>
      count(wave, cat = as.character(.data[[group_var]])) |>
      group_by(wave) |>
      mutate(pct = n / sum(n) * 100) |>
      ungroup()
    
    plot_ly(df, x = ~wave, y = ~pct, color = ~cat, type = 'bar') |>
      layout(barmode = 'stack',
             xaxis   = list(title = 'Wave'),
             yaxis   = list(title = '%', range = c(0, 100))) |>
      plotly_theme() |>
      layout(legend = list(orientation = 'v',
                           x           = 1.02,
                           xanchor     = 'left',
                           y           = 0.5))
  }
  
  # Boxplot de renda por variável de grupo
  box_cross <- function(df_filt, group_var) {
    df <- df_filt |>
      filter(!is.na(.data[[group_var]]), !is.na(income)) |>
      mutate(grupo = as.character(.data[[group_var]]))
    
    plot_ly(df, x = ~grupo, y = ~income, color = ~grupo,
            type = 'box', showlegend = FALSE) |>
      layout(xaxis = list(title = '', tickangle = -30),
             yaxis = list(title = 'Renda (1–10)', range = c(0, 11))) |>
      plotly_theme()
  }
  
  # Lista de atributos para mapas
  map_vars <- list(
    'Valores'        = c('Autonomia'          = 'autonomy',
                         'Post-Materialismo'  = 'postmat',
                         'Valores Seculares'  = 'secular'),
    'Bem-estar'      = c('Satisfação vida'    = 'life_sat',
                         'Felicidade'         = 'happiness',
                         'Liberdade'          = 'freedom',
                         'Satisf. Financeira' = 'fin_sat'),
    'Político'       = c('Interesse Político' = 'pol_interest',
                         'Posição Política'   = 'pol_lr',
                         'Apoio à Igualdade'  = 'inc_equal'),
    'Insegurança'    = c('Alimentar'          = 'insec_food',
                         'Crime'              = 'insec_crime',
                         'Saúde'              = 'insec_health',
                         'Financeira'         = 'insec_income'),
    'Socioeconômico' = c('Renda Média'        = 'income')
  )
  
  
  # 3. UI =====================================================================
  
  ui <- page_navbar(
    title    = 'World Values Survey (WVS)',
    theme    = bs_theme(bootswatch = 'flatly'),
    fillable = FALSE,
    header   = tags$head(tags$style(HTML(
      '.bootstrap-select .dropdown-toggle { background-color: #fff; border: 1px solid #ced4da; }'
    ))),
    
    sidebar = sidebar(
      title = 'Filtros globais',
      width = 280,
      
      shinyWidgets::pickerInput('sel_wave', 'Wave',
                                choices  = wave_choices,
                                selected = as.character(1:7),
                                multiple = TRUE,
                                options  = shinyWidgets::pickerOptions(
                                  actionsBox        = TRUE,
                                  selectAllText     = 'Todas',
                                  deselectAllText   = 'Nenhuma',
                                  selectedTextFormat = 'count > 6',
                                  countSelectedText  = 'Todas',
                                  liveSearch        = FALSE
                                )),
      
      selectInput('sel_continent', 'Continente',
                  choices  = c('Todos', continents_list),
                  selected = 'Todos'),
      
      uiOutput('ui_region'),
      uiOutput('ui_country'),
      
      selectInput('sel_sex', 'Sexo',
                  choices  = c('Todos', sex_list),
                  selected = 'Todos'),
      
      selectInput('sel_age_grp', 'Faixa etária',
                  choices  = c('Todas', age_grp_list),
                  selected = 'Todas'),
      
      selectInput('sel_edu', 'Escolaridade',
                  choices  = c('Todos', edu_list),
                  selected = 'Todos'),
      
      selectInput('sel_marital', 'Estado conjugal',
                  choices  = c('Todos', marital_list),
                  selected = 'Todos'),
      
      selectInput('sel_pol_spec', 'Espectro político',
                  choices  = c('Todos', spectrum_list),
                  selected = 'Todos'),
      
      hr(),
      tags$small(em('WVS Longitudinal 1981–2022 v5.0'), style = 'color: grey;')
    ),
    
    
    ## -- ABA 1: Perfil Sociodemográfico ----------------------------------------
    nav_panel('Perfil Sociodemográfico',
              
              card(card_header('Composição demográfica por wave (%)'),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(card_header('Sexo'),          plotlyOutput('comp_sex',     height = '300px')),
                     card(card_header('Faixa etária'),  plotlyOutput('comp_age',     height = '300px')),
                     card(card_header('Est. conjugal'), plotlyOutput('comp_marital', height = '300px')),
                     card(card_header('Escolaridade'),  plotlyOutput('comp_edu',     height = '300px'))
                   )
              ),
              
              layout_columns(
                col_widths = c(4, 8),
                
                card(
                  card_header('Pirâmide etária'),
                  selectInput('lpyr_wave', 'Wave', choices = wave_choices, selected = 7),
                  plotlyOutput('plot_pyramid', height = '340px')
                ),
                
                layout_columns(
                  col_widths = 12,
                  card(
                    card_header('% Ocupação por wave'),
                    plotlyOutput('plot_occ_wave', height = '200px')
                  ),
                  card(
                    card_header('% Ocupação por faixa etária'),
                    selectInput('locc_wave', 'Wave', choices = wave_choices, selected = 7),
                    plotlyOutput('plot_occ_age', height = '200px')
                  )
                )
              ),
              
              card(
                card_header('Distribuição de renda (escala 1–10) por wave'),
                plotlyOutput('plot_income_dist', height = '300px')
              ),
              
              card(card_header('Renda × Escolaridade, Sexo e Faixa etária'),
                   layout_columns(
                     col_widths = c(4, 4, 4),
                     card(card_header('× Escolaridade'), plotlyOutput('inc_x_edu', height = '260px')),
                     card(card_header('× Sexo'),         plotlyOutput('inc_x_sex', height = '260px')),
                     card(card_header('× Faixa etária'), plotlyOutput('inc_x_age', height = '260px'))
                   )
              )
    ),
    
    
    ## -- ABA 2: Valores × Renda ------------------------------------------------
    nav_panel('Valores × Renda',
              
              card(card_header('Médias por nível de renda (escala 1–10)'),
                   plotlyOutput('val_income10', height = '340px')),
              
              card(card_header('Médias por faixa de renda agrupada'),
                   plotlyOutput('val_income_grp', height = '320px')),
              
              layout_columns(
                col_widths = c(6, 6),
                card(card_header('Autonomia por faixa × wave'),
                     plotlyOutput('wave_autonomy', height = '320px')),
                card(card_header('Post-Materialismo por faixa × wave'),
                     plotlyOutput('wave_postmat', height = '320px')),
                card(card_header('Valores Seculares por faixa × wave'),
                     plotlyOutput('wave_secular', height = '320px')),
                card(card_header('Autonomia × Valores Seculares por faixa de renda'),
                     plotlyOutput('scatter_aut_sec', height = '320px'))
              )
    ),
    
    
    ## -- ABA 3: Bem-estar e Prioridades de Vida --------------------------------
    
    nav_panel('Bem-estar e Prioridades de Vida',
              
              card(card_header('Indicadores de bem-estar por nível de renda (1–10)'),
                   plotlyOutput('wb_line', height = '340px')),
              
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                card(card_header('Satisfação vida'),  plotlyOutput('wb_lifesat',   height = '240px')),
                card(card_header('Felicidade'),       plotlyOutput('wb_happiness', height = '240px')),
                card(card_header('Liberdade'),        plotlyOutput('wb_freedom',   height = '240px')),
                card(card_header('Sat. financeira'),  plotlyOutput('wb_finsat',    height = '240px'))
              ),
              
              layout_columns(
                col_widths = c(5, 7),
                
                card(
                  card_header('Radar de importâncias por faixa de renda (0 = pouco, 1 = muito)'),
                  plotlyOutput('plot_radar', height = '400px')
                ),
                
                card(
                  card_header('Evolução de importância por wave (por faixa de renda)'),
                  selectInput('sel_imp_dim', 'Dimensão',
                              choices  = names(imp_vars),
                              selected = 'Família'),
                  plotlyOutput('imp_wave', height = '340px')
                )
              )
    ),
    
    
    ## -- ABA 4: Posicionamento Político ----------------------------------------
    
    nav_panel('Posicionamento Político',
              
              card(card_header('Evolução geral dos 3 indicadores por wave'),
                   plotlyOutput('pol_overview', height = '320px')),
              
              card(
                card_header('Segmentação por wave'),
                selectInput('sel_pol_var', 'Indicador',
                            choices  = names(pol_vars),
                            selected = 'Interesse político'),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  card(card_header('Por sexo'),           plotlyOutput('pol_sex',    height = '260px')),
                  card(card_header('Por faixa etária'),   plotlyOutput('pol_age',    height = '260px')),
                  card(card_header('Por faixa de renda'), plotlyOutput('pol_income', height = '260px'))
                )
              )
    ),
    
    
    ## -- ABA 5: Insegurança e Pobreza ------------------------------------------
    
    nav_panel('Insegurança e Pobreza',
              
              layout_columns(
                col_widths = c(6, 6),
                card(card_header('Inseguranças gerais por faixa de renda'),
                     plotlyOutput('insec_income_grp', height = '320px')),
                card(card_header('Evolução das 4 inseguranças por wave'),
                     plotlyOutput('insec_wave', height = '320px'))
              ),
              
              card(
                card_header('Segmentação por tipo de insegurança'),
                selectInput('sel_insec_var', 'Insegurança',
                            choices  = names(insec_vars),
                            selected = 'Alimentar'),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  card(card_header('Por renda (1–10)'),      plotlyOutput('insec_inc10',    height = '260px')),
                  card(card_header('Por faixa etária'),      plotlyOutput('insec_age',      height = '260px')),
                  card(card_header('Por espectro político'), plotlyOutput('insec_spectrum', height = '260px'))
                )
              )
    ),
    
    
    ## -- ABA 6: Mapa Mundial ---------------------------------------------------
    
    nav_panel('Mapa',
              
              layout_columns(
                col_widths = c(5, 3, 4),
                selectInput('map_var',   'Variável',
                            choices  = map_vars,
                            selected = 'autonomy'),
                selectInput('map_level', 'Visualizar por',
                            choices  = c('País' = 'country', 'Região' = 'region'),
                            selected = 'country'),
                NULL
              ),
              
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header('Mapa A'),
                  selectInput('map_wave_l', 'Wave',
                              choices  = wave_choices,
                              selected = 1),
                  plotlyOutput('map_left', height = '460px')
                ),
                card(
                  card_header('Mapa B'),
                  selectInput('map_wave_r', 'Wave',
                              choices  = wave_choices,
                              selected = 7),
                  plotlyOutput('map_right', height = '460px')
                )
              )
    ),
    
    
    ## -- ABA 7: Mapa Inglehart-Welzel ------------------------------------------
    
    nav_panel('Mapa Inglehart-Welzel',
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                selectInput('iw_continent', 'Região',
                            choices  = c('Todos', regions_list),
                            selected = 'Todos'),
                shinyWidgets::pickerInput('iw_waves', 'Waves',
                                          choices  = wave_choices,
                                          selected = as.character(1:7),
                                          multiple = TRUE,
                                          options  = shinyWidgets::pickerOptions(
                                            actionsBox         = TRUE,
                                            selectAllText      = 'Todas',
                                            deselectAllText    = 'Nenhuma',
                                            selectedTextFormat = 'count > 6',
                                            countSelectedText  = 'Todas'
                                          )),
                checkboxInput('iw_trajectory', 'Mostrar trajetórias', value = TRUE),
                checkboxInput('iw_brazil',     'Destacar Brasil',     value = FALSE)
              ),
              
              card(
                card_header('Autonomia × Valores Seculares por país'),
                plotlyOutput('plot_iw', height = '720px')
              )
    )
  )
  
  # 4. SERVER =================================================================
  
  server <- function(input, output, session) {
    
    ## -- Filtros dinâmicos encadeados ------------------------------------------
    
    output$ui_region <- renderUI({
      regs <- if (input$sel_continent == 'Todos') {
        regions_list
      } else {
        sort(unique(na.omit(wvs$region[wvs$continent == input$sel_continent])))
      }
      selectInput('sel_region', 'Região',
                  choices  = c('Todas', regs),
                  selected = 'Todas')
    })
    
    output$ui_country <- renderUI({
      req(input$sel_region)
      
      df_tmp <- wvs
      if (input$sel_continent != 'Todos') df_tmp <- df_tmp |> filter(continent == input$sel_continent)
      if (input$sel_region    != 'Todas') df_tmp <- df_tmp |> filter(region    == input$sel_region)
      
      alpha <- sort(unique(df_tmp$COUNTRY_ALPHA))
      nomes <- country_lookup$country_name[match(alpha, country_lookup$COUNTRY_ALPHA)]
      nomes <- ifelse(is.na(nomes), alpha, nomes)
      
      selectInput('sel_country', 'País',
                  choices  = c('Todos' = 'Todos', setNames(alpha, nomes)),
                  selected = 'Todos')
    })
    
    ## -- Reativo central -------------------------------------------------------
    
    wvs_f <- reactive({
      req(input$sel_wave)
      
      df <- wvs |> filter(wave_num %in% as.integer(input$sel_wave))
      
      if (!is.null(input$sel_continent) && input$sel_continent != 'Todos')
        df <- df |> filter(continent == input$sel_continent)
      if (!is.null(input$sel_region) && input$sel_region != 'Todas')
        df <- df |> filter(region == input$sel_region)
      if (!is.null(input$sel_country) && input$sel_country != 'Todos')
        df <- df |> filter(COUNTRY_ALPHA == input$sel_country)
      if (input$sel_sex     != 'Todos') df <- df |> filter(as.character(sex)          == input$sel_sex)
      if (input$sel_age_grp != 'Todas') df <- df |> filter(as.character(age_grp)      == input$sel_age_grp)
      if (input$sel_edu     != 'Todos') df <- df |> filter(as.character(education)    == input$sel_edu)
      if (input$sel_marital != 'Todos') df <- df |> filter(as.character(marital)      == input$sel_marital)
      if (input$sel_pol_spec!= 'Todos') df <- df |> filter(as.character(pol_spectrum) == input$sel_pol_spec)
      
      df
    })
    
    
    ## -- ABA 1: Perfil Sociodemográfico ----------------------------------------
    
    output$comp_sex     <- renderPlotly(pct_bar_wave(wvs_f(), 'sex'))
    output$comp_age     <- renderPlotly(pct_bar_wave(wvs_f(), 'age_grp'))
    output$comp_marital <- renderPlotly(pct_bar_wave(wvs_f(), 'marital'))
    output$comp_hh      <- renderPlotly(pct_bar_wave(wvs_f(), 'hh_size'))
    output$comp_lit     <- renderPlotly(pct_bar_wave(wvs_f(), 'literate'))
    output$comp_edu     <- renderPlotly(pct_bar_wave(wvs_f(), 'education'))
    
    output$plot_pyramid <- renderPlotly({
      req(input$lpyr_wave)
      
      df <- wvs_f() |>
        filter(wave_num == as.integer(input$lpyr_wave),
               !is.na(age_grp), !is.na(sex)) |>
        count(sex = as.character(sex), age_grp) |>
        mutate(pct = n / sum(n) * 100) |>   # % do total, sem group_by
        ungroup()
      
      sexos <- sort(unique(df$sex))   # 'Female', 'Male' — alfabético
      s_fem <- sexos[1]
      s_mal <- sexos[2]
      
      plot_ly() |>
        add_trace(data        = df |> filter(sex == s_mal),
                  x           = ~-pct,
                  y           = ~age_grp,
                  type        = 'bar',
                  orientation = 'h',
                  name        = s_mal) |>
        add_trace(data        = df |> filter(sex == s_fem),
                  x           = ~pct,
                  y           = ~age_grp,
                  type        = 'bar',
                  orientation = 'h',
                  name        = s_fem) |>
        layout(
          barmode = 'overlay',
          xaxis   = list(title    = '← Masculino | Feminino →',
                         tickvals = seq(-20, 20, 5),
                         ticktext = paste0(abs(seq(-20, 20, 5)), '%')),
          yaxis   = list(title = 'Faixa etária')
        ) |>
        plotly_theme()
    })
    
    output$plot_occ_wave <- renderPlotly(pct_bar_wave(wvs_f(), 'emp_status'))
    
    output$plot_occ_age <- renderPlotly({
      req(input$locc_wave)
      
      df <- wvs_f() |>
        filter(wave_num == as.integer(input$locc_wave),
               !is.na(age_grp), !is.na(emp_status)) |>
        count(age_grp, cat = as.character(emp_status)) |>
        group_by(age_grp) |>
        mutate(pct = n / sum(n) * 100) |>
        ungroup()
      
      plot_ly(df, x = ~age_grp, y = ~pct, color = ~cat,
              type = 'bar') |>
        layout(xaxis   = list(title = 'Faixa etária'),
               yaxis   = list(title = '%', range = c(0, 100)),
               barmode = 'stack') |>
        plotly_theme() |>
        layout(legend = list(orientation = 'v',
                             x           = 1.02,
                             xanchor     = 'left',
                             y           = 0.5))
    })
    
    output$plot_income_dist <- renderPlotly({
      df <- wvs_f() |> filter(!is.na(wave), !is.na(income))
      
      plot_ly(df, x = ~wave, y = ~income, color = ~wave,
              type = 'box', showlegend = FALSE) |>
        layout(xaxis = list(title = 'Wave'),
               yaxis = list(title = 'Renda (1–10)', range = c(0, 11))) |>
        plotly_theme()
    })
    
    output$inc_x_edu <- renderPlotly(box_cross(wvs_f(), 'education'))
    output$inc_x_sex <- renderPlotly(box_cross(wvs_f(), 'sex'))
    output$inc_x_age <- renderPlotly(box_cross(wvs_f(), 'age_grp'))
    
    
    ## -- ABA 2: Valores × Renda ------------------------------------------------
    
    output$val_income10 <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income)) |>
        group_by(income) |>
        summarise(
          'Post-Materialismo'    = wmean(postmat,  S017),
          'Autonomia'            = wmean(autonomy, S017),
          'Valores Seculares'    = wmean(secular,  S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-income, names_to = 'escala', values_to = 'media')
      
      plot_ly(df, x = ~income, y = ~media, color = ~escala,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Nível de renda (1–10)', dtick = 1),
               yaxis = list(title = 'Média')) |>
        plotly_theme()
    })
    
    output$val_income_grp <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income_grp)) |>
        group_by(income_grp) |>
        summarise(
          'Post-Materialismo'    = wmean(postmat,  S017),
          'Autonomia'            = wmean(autonomy, S017),
          'Valores Seculares'    = wmean(secular,  S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-income_grp, names_to = 'escala', values_to = 'media')
      
      plot_ly(df, x = ~income_grp, y = ~media, color = ~escala,
              type = 'bar', barmode = 'group') |>
        layout(xaxis = list(title = ''), yaxis = list(title = 'Média')) |>
        plotly_theme()
    })
    
    val_wave <- function(var, label_y) {
      df <- wvs_f() |>
        filter(!is.na(income_grp), !is.na(wave)) |>
        group_by(wave, income_grp) |>
        summarise(media = wmean(.data[[var]], S017), .groups = 'drop')
      
      plot_ly(df, x = ~wave, y = ~media, color = ~income_grp,
              colors = pal_income, type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Wave'), yaxis = list(title = label_y)) |>
        plotly_theme() |>
        layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left', y = 0.5))
    }
    
    output$wave_autonomy <- renderPlotly(val_wave('autonomy', 'Autonomia'))
    output$wave_postmat  <- renderPlotly(val_wave('postmat',  'Post-Materialismo'))
    output$wave_secular  <- renderPlotly(val_wave('secular',  'Val. Seculares'))
    
    output$scatter_aut_sec <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income_grp), !is.na(wave), !is.na(autonomy), !is.na(secular)) |>
        group_by(income_grp, wave) |>
        summarise(
          autonomy = wmean(autonomy, S017),
          secular  = wmean(secular,  S017),
          .groups  = 'drop'
        ) |>
        mutate(label = paste0(income_grp, ': ', substr(wave,1,2)))
      
      plot_ly(df,
              x            = ~secular,
              y            = ~autonomy,
              color        = ~income_grp,
              colors       = pal_income,
              type         = 'scatter',
              mode         = 'markers+text',
              text         = ~label,
              textposition = 'top center',
              marker       = list(size = 10),
              showlegend   = FALSE) |>
        layout(xaxis = list(title = 'Valores Seculares (0–1)'),
               yaxis = list(title = 'Autonomia (0–1)')) |>
        plotly_theme()
    })
    
    
    ## -- ABA 3: Bem-estar e Prioridades de Vida --------------------------------
    
    output$wb_line <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income)) |>
        group_by(income) |>
        summarise(
          'Satisfação vida' = wmean(life_sat,  S017),
          'Felicidade'      = wmean(happiness, S017),
          'Liberdade'       = wmean(freedom,   S017),
          'Sat. financeira' = wmean(fin_sat,   S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-income, names_to = 'indicador', values_to = 'media')
      
      plot_ly(df, x = ~income, y = ~media, color = ~indicador,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Nível de renda (1–10)', dtick = 1),
               yaxis = list(title = 'Média')) |>
        plotly_theme()
    })
    
    output$wb_lifesat   <- renderPlotly(bar_by_income(wvs_f(), 'life_sat',  'Satisf. vida'))
    output$wb_happiness <- renderPlotly(bar_by_income(wvs_f(), 'happiness', 'Felicidade'))
    output$wb_freedom   <- renderPlotly(bar_by_income(wvs_f(), 'freedom',   'Liberdade'))
    output$wb_finsat    <- renderPlotly(bar_by_income(wvs_f(), 'fin_sat',   'Sat. financeira'))
    
    output$plot_radar <- renderPlotly({
      cats <- names(imp_vars)
      
      df <- wvs_f() |>
        filter(!is.na(income_grp)) |>
        group_by(income_grp) |>
        summarise(across(all_of(unname(imp_vars)),
                         ~ wmean(., S017)),
                  .groups = 'drop')
      
      p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
      
      for (i in seq_len(nrow(df))) {
        grp  <- as.character(df$income_grp[i])
        vals <- as.numeric(df[i, -1])
        p <- p |> add_trace(
          r     = c(vals, vals[1]),
          theta = c(cats, cats[1]),
          name  = grp,
          line  = list(color = pal_income[grp])
        )
      }
      
      p |> layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 1)))
      ) |> plotly_theme()
    })
    
    output$imp_wave <- renderPlotly({
      var_sel <- imp_vars[[input$sel_imp_dim]]
      
      df <- wvs_f() |>
        filter(!is.na(income_grp), !is.na(wave)) |>
        group_by(wave, income_grp) |>
        summarise(media = wmean(.data[[var_sel]], S017), .groups = 'drop')
      
      plot_ly(df, x = ~wave, y = ~media, color = ~income_grp,
              colors = pal_income, type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Wave'),
               yaxis = list(title = paste0(input$sel_imp_dim, ' (0–1)'))) |>
        plotly_theme()
    })
    
    
    ## -- ABA 4: Posicionamento Político ----------------------------------------
    
    output$pol_overview <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(wave)) |>
        group_by(wave) |>
        summarise(
          'Interesse por Política'  = wmean(pol_interest, S017),
          'Posição Política'        = wmean(pol_lr,       S017),
          'Apoio à Igualdade'       = wmean(inc_equal,    S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-wave, names_to = 'indicador', values_to = 'media')
      
      plot_ly(df, x = ~wave, y = ~media, color = ~indicador,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Wave'),
               yaxis = list(title = 'Média (0–1)', range = c(0, 1))) |>
        plotly_theme()
    })
    
    pol_var_sel <- reactive(pol_vars[[input$sel_pol_var]])
    
    output$pol_sex    <- renderPlotly(seg_line_wave(wvs_f(), pol_var_sel(), 'sex',       y_range = NULL))
    output$pol_age    <- renderPlotly(seg_line_wave(wvs_f(), pol_var_sel(), 'age_grp',   y_range = NULL))
    output$pol_income <- renderPlotly(seg_line_wave(wvs_f(), pol_var_sel(), 'income_grp',
                                                    y_range = NULL, colors = pal_income))
    
    
    ## -- ABA 5: Insegurança e Pobreza ------------------------------------------
    
    output$insec_income_grp <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income_grp)) |>
        group_by(income_grp) |>
        summarise(
          'Alimentar'  = wmean(insec_food,   S017),
          'Crime'      = wmean(insec_crime,  S017),
          'Saúde'      = wmean(insec_health, S017),
          'Financeira' = wmean(insec_income, S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-income_grp, names_to = 'tipo', values_to = 'media')
      
      plot_ly(df, x = ~income_grp, y = ~media, color = ~tipo,
              colors = unname(pal_insec), type = 'bar', barmode = 'group') |>
        layout(xaxis = list(title = ''),
               yaxis = list(title = 'Frequência (0–1)')) |>
        plotly_theme()
    })
    
    output$insec_wave <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(wave)) |>
        group_by(wave) |>
        summarise(
          'Alimentar'  = wmean(insec_food,   S017),
          'Crime'      = wmean(insec_crime,  S017),
          'Saúde'      = wmean(insec_health, S017),
          'Financeira' = wmean(insec_income, S017),
          .groups = 'drop'
        ) |>
        pivot_longer(-wave, names_to = 'tipo', values_to = 'media')
      
      plot_ly(df, x = ~wave, y = ~media, color = ~tipo,
              colors = unname(pal_insec),
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Wave'),
               yaxis = list(title = 'Frequência (0–1)')) |>
        plotly_theme()
    })
    
    insec_var_sel <- reactive(insec_vars[[input$sel_insec_var]])
    
    output$insec_age <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(age_grp), !is.na(wave)) |>
        group_by(age_grp, wave) |>
        summarise(media = wmean(.data[[insec_var_sel()]], S017), .groups = 'drop')
      
      plot_ly(df, x = ~age_grp, y = ~media, color = ~wave,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Faixa etária'),
               yaxis = list(title = 'Frequência (0–1)')) |>
        plotly_theme() |>
        layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left', y = 0.5))
    })
    
    output$insec_spectrum <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(pol_spectrum), !is.na(wave)) |>
        group_by(pol_spectrum, wave) |>
        summarise(media = wmean(.data[[insec_var_sel()]], S017), .groups = 'drop')
      
      plot_ly(df, x = ~pol_spectrum, y = ~media, color = ~wave,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Espectro político'),
               yaxis = list(title = 'Frequência (0–1)')) |>
        plotly_theme() |>
        layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left', y = 0.5))
    })
    
    output$insec_inc10 <- renderPlotly({
      df <- wvs_f() |>
        filter(!is.na(income), !is.na(wave)) |>
        group_by(wave, income) |>
        summarise(media = wmean(.data[[insec_var_sel()]], S017), .groups = 'drop')
      
      plot_ly(df, x = ~income, y = ~media, color = ~wave,
              type = 'scatter', mode = 'lines+markers') |>
        layout(xaxis = list(title = 'Renda (1–10)', dtick = 1),
               yaxis = list(title = 'Frequência (0–1)')) |>  # range removido
        plotly_theme() |>
        layout(legend = list(orientation = 'v', x = 1.02, xanchor = 'left', y = 0.5))
    })
    
    
    ## -- ABA 6: Mapa Mundial ---------------------------------------------------
    
    make_map <- function(wave_sel, var_sel, level, zmin = NULL, zmax = NULL) {
      label_var <- names(unlist(map_vars))[unlist(map_vars) == var_sel]
      
      df_base <- wvs |>
        filter(wave_num == as.integer(wave_sel),
               !is.na(.data[[var_sel]]))
      
      if (level == 'country') {
        
        df <- df_base |>
          group_by(COUNTRY_ALPHA, country_name) |>
          summarise(media = wmean(.data[[var_sel]], S017), .groups = 'drop')
        
        plot_ly(df,
                type         = 'choropleth',
                locations    = ~COUNTRY_ALPHA,
                z            = ~media,
                zmin         = zmin,
                zmax         = zmax,
                text         = ~country_name,
                colorscale   = 'Viridis',
                colorbar     = list(title = label_var),
                marker       = list(line = list(color = 'white', width = 0.5))) |>
          layout(geo = list(
            showframe      = FALSE,
            showcoastlines = TRUE,
            projection     = list(type = 'natural earth')
          )) |>
          plotly_theme()
        
      } else {
        
        df <- df_base |>
          filter(!is.na(region)) |>
          group_by(region) |>
          summarise(media = wmean(.data[[var_sel]], S017), .groups = 'drop') |>
          arrange(media)
        
        plot_ly(df,
                x           = ~media,
                y           = ~reorder(region, media),
                type        = 'bar',
                orientation = 'h',
                marker      = list(color = '#4575b4')) |>
          layout(xaxis = list(title = label_var),
                 yaxis = list(title = '')) |>
          plotly_theme()
      }
    }
    
    map_zrange <- reactive({
      req(input$map_wave_l, input$map_wave_r, input$map_var)
      
      vals <- wvs |>
        filter(wave_num %in% c(as.integer(input$map_wave_l),
                               as.integer(input$map_wave_r)),
               !is.na(.data[[input$map_var]])) |>
        pull(.data[[input$map_var]])
      
      list(zmin = min(vals, na.rm = TRUE),
           zmax = max(vals, na.rm = TRUE))
    })
    
    output$map_left <- renderPlotly({
      req(input$map_wave_l, input$map_var, input$map_level)
      zr <- map_zrange()
      make_map(input$map_wave_l, input$map_var, input$map_level, zr$zmin, zr$zmax)
    })
    
    output$map_right <- renderPlotly({
      req(input$map_wave_r, input$map_var, input$map_level)
      zr <- map_zrange()
      make_map(input$map_wave_r, input$map_var, input$map_level, zr$zmin, zr$zmax)
    })
    
    
    ## -- ABA 7: Mapa Inglehart-Welzel ------------------------------------------
    
    output$plot_iw <- renderPlotly({
      req(input$iw_waves)
      
      df <- wvs |>
        filter(wave_num %in% as.integer(input$iw_waves),
               !is.na(autonomy), !is.na(secular), !is.na(continent)) |>
        group_by(COUNTRY_ALPHA, country_name, continent, region, wave, wave_num) |>
        summarise(
          autonomy    = wmean(autonomy, S017),
          secular     = wmean(secular,  S017),
          income_mean = wmean(income,   S017),
          .groups = 'drop'
        )
      
      if (input$iw_continent != 'Todos')
        df <- df |> filter(.data[['region']] == input$iw_continent)
      
      df_main   <- df |> filter(COUNTRY_ALPHA != 'BRA')
      df_brazil <- df |> filter(COUNTRY_ALPHA == 'BRA')
      
      x_rng <- range(df$secular,  na.rm = TRUE)
      y_rng <- range(df$autonomy, na.rm = TRUE)
      
      p <- plot_ly()
      
      # Trajetórias (linhas cinzas conectando waves por país)
      if (isTRUE(input$iw_trajectory)) {
        df_lines <- df_main |>
          arrange(COUNTRY_ALPHA, wave_num) |>
          group_by(COUNTRY_ALPHA) |>
          filter(n() > 1) |>
          group_modify(~ bind_rows(.x, tibble(secular = NA_real_, autonomy = NA_real_,
                                              wave_num = NA_integer_))) |>
          ungroup()
        
        if (nrow(df_lines) > 0)
          p <- p |> add_trace(
            data       = df_lines,
            x          = ~secular,
            y          = ~autonomy,
            type       = 'scatter',
            mode       = 'lines',
            line       = list(color = '#cccccc', width = 0.7),
            showlegend = FALSE,
            hoverinfo  = 'none'
          )
      }
      
      # Pontos por continente
      modo_exibicao <- if (length(input$iw_waves) == 1 && input$iw_continent != 'Todos') {
        'markers+text'
      } else {
        'markers'
      }
      
      for (cont in sort(unique(df_main$continent))) {
        df_cont <- df_main |> filter(continent == cont)
        p <- p |> add_trace(
          data         = df_cont,
          x            = ~secular,
          y            = ~autonomy,
          type         = 'scatter',
          mode         = modo_exibicao,
          name         = cont,
          text         = ~country_name,
          textposition = 'top center',
          hovertext    = ~paste0('<b>', country_name, '</b><br>',
                                 'Wave: ', wave, '<br>',
                                 'Autonomia: ', round(autonomy, 3), '<br>',
                                 'Val. Seculares: ', round(secular, 3), '<br>',
                                 'Renda média: ', round(income_mean, 2)),
          hoverinfo    = 'text',
          marker       = list(size = 7, opacity = 0.8)
        )
      }
      
      # Brasil destacado
      if (isTRUE(input$iw_brazil) && nrow(df_brazil) > 0) {
        df_brazil <- df_brazil |> arrange(wave_num)
        
        if (isTRUE(input$iw_trajectory) && nrow(df_brazil) > 1)
          p <- p |> add_trace(
            data       = df_brazil,
            x          = ~secular,
            y          = ~autonomy,
            type       = 'scatter',
            mode       = 'lines',
            line       = list(color = '#e63946', width = 2),
            showlegend = FALSE,
            hoverinfo  = 'none'
          )
        
        p <- p |> add_trace(
          data         = df_brazil,
          x            = ~secular,
          y            = ~autonomy,
          type         = 'scatter',
          mode         = 'markers+text',
          name         = 'Brasil',
          text         = ~wave,
          textposition = 'top center',
          customdata   = ~paste0('<b>', country_name, '</b><br>',
                                 'Wave: ', wave, '<br>',
                                 'Autonomia: ', round(autonomy, 3), '<br>',
                                 'Val. Seculares: ', round(secular, 3), '<br>',
                                 'Renda média: ', round(income_mean, 2)),
          hovertemplate = '%{customdata}<extra></extra>',
          marker       = list(size = 12, color = '#e63946',
                              line = list(color = 'white', width = 1.5))
        )
      }
      
      p |> layout(
        xaxis  = list(title = '← Tradicional | Secular-racional →', zeroline = FALSE),
        yaxis  = list(title = '← Sobrevivência | Autoexpressão →',  zeroline = FALSE),
        shapes = list(
          list(type = 'line',
               x0   = mean(x_rng), x1 = mean(x_rng),
               y0   = y_rng[1],    y1 = y_rng[2],
               line = list(dash = 'dot', color = 'grey', width = 1)),
          list(type = 'line',
               x0   = x_rng[1],    x1 = x_rng[2],
               y0   = mean(y_rng), y1 = mean(y_rng),
               line = list(dash = 'dot', color = 'grey', width = 1))
        )
      ) |> plotly_theme()
    })
  }
  
  # 5. RUN ======================================================================
 
  shinyApp(ui, server)
}