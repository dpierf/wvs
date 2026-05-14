# =========================================================================== #
#                          Configurações do Pipeline                          #
# =========================================================================== #

# Waves a processar
WAVES <- c(5, 6, 7)

# Paths
PATH_DATA    <- 'data/F00011935-WVS_Time_Series_1981-2022_Rds_v5_0.zip'
PATH_OUTPUTS <- 'outputs/'
PATH_MODELS  <- 'outputs/models/'
PATH_PLOTS   <- 'outputs/plots/'

# Parâmetros de split
SEED_SPLIT   <- 18692
PROP_OOS     <- 0.30
PROP_VAL     <- 0.20

# Parâmetros de FS
MISS_THRESHOLD <- 0.20
FREQ_THRESHOLD <- 0.95
IV_THRESHOLD   <- 0.02
CV_THRESHOLD   <- 0.05
PROP_MRMR      <- 0.60
MAX_MRMR       <- 150

# Parâmetros LightGBM
SEED_LGB          <- 18692
LGB_ROUNDS        <- 1000
LGB_EARLY_STOP    <- 50
LGB_LEARNING_RATE <- 0.05
LGB_NUM_LEAVES    <- 31
LGB_MIN_DATA      <- 20
LGB_FEAT_FRAC     <- 0.8
LGB_BAG_FRAC      <- 0.8
LGB_BAG_FREQ      <- 5

# Variáveis excluídas intencionalmente
VARS_EXCLUIR <- c(
  'S002VS', 'S020',
  'COW_NUM', 'COW_ALPHA', 'X051', 'X048WVS',
  'C006', 'H008_04', 'X044', 'X045', 'X045B', 'X047R_WVS', 'X047CS', 'X028'
)

# Variáveis para dashboard
VARS_DASHBOARD <- c(
  'S002VS', 'COUNTRY_ALPHA', 'S007', 'mode', 'S020', 'S017',
  paste0('A00', 1:9),
  'A170', 'A173', 'C006',
  'E023', 'E033', 'E035',
  paste0('E13', 0:3),
  paste0('H008_0', 1:4),
  'X001', 'X003', 'X007', 'X013',
  'X023', 'X024B', 'X025',
  'X028', 'X036',
  'X045', 'X045B', 'X047_WVS',
  'X049', 'X050B',
  'Y001', 'Y003', 'Y010'
)
