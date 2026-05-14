# =========================================================================== #
#                         Pipeline ML — WVS | Main                           #
# =========================================================================== #

# 0. Pacotes ------------------------------------------------------------------

if (!'here' %in% installed.packages()[, 'Package']) install.packages('here')
require(here)

setwd(here::here())

pacotes <- c('lightgbm', 'ggplot2', 'scales', 'cowplot', 'Information',
             'rstatix', 'mRMRe', 'pROC', 'glmnet', 'shapviz', 'rsample',
             'tidyr', 'patchwork', 'countrycode', 'haven', 'dplyr', 'shiny',
             'bslib', 'plotly', 'data.table', 'labelled', 'shinyWidgets')

ausentes <- pacotes[!pacotes %in% installed.packages()[, 'Package']]
if (length(ausentes) > 0) install.packages(ausentes)
invisible(lapply(pacotes, library, character.only = TRUE))

rm(ausentes, pacotes) ; gc() ; cat('\014')


# 1. Configurações e funções --------------------------------------------------

vars  <- new.env(parent = .GlobalEnv)
source('R/config.R',    local = vars) # Parâmetros básicos

funs  <- new.env(parent = vars)
source('R/pipeline.R',  local = funs) # Processo de modelagem
source('R/dataviz.R',   local = funs) # Visualização de resultados
source('R/dashboard.R', local = funs) # Montagem do dashboard


# 2. Base ---------------------------------------------------------------------

# Base original
baseWVS <- funs$carregar_base(vars$PATH_DATA, wave_min = 1)

# Objetos derivados
baseMOD <- baseWVS |> filter(haven::zap_labels(S002VS) >= 5) # Modelo LGB
baseVIZ <- funs$preparar_base(baseWVS)                       # Dashboard

rm(baseWVS) ; gc() ; cat('\014')


# 3. Implementações -----------------------------------------------------------

# Machine Learning
modeloLGB <- funs$pipeline_wvs(baseMOD)    # Execução do modelo
funs$plotar_wvs(modeloLGB)                 # Plotagem de resultados

# Dashboard
funs$dashboard_wvs(baseVIZ)                # Visualização global
