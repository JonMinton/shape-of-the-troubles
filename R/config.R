# Configuration for Shape of the Troubles analysis

# HMD country code for Northern Ireland
NI_CODE <- "GBR_NIR"

# Comparator country groups
UK_CODES <- c("GBR_NIR", "GBR_SCO", "GBRTENW")
WESTERN_EUROPE_CODES <- c(
  "GBR_NIR", "GBR_SCO", "GBRTENW", "IRL",
  "FRATNP", "BEL", "LUX", "NLD",
  "DEUTNP", "CHE", "AUT"
)

# Model parameters
OPTIMAL_DECAY_K <- 0.09748423
TROUBLES_HALFLIFE <- log(0.5) / log(1 - OPTIMAL_DECAY_K) # ~6.76 years
TROUBLES_START_YEAR <- 1972

# Phase boundaries for mortality improvement model
PHASE_1_END <- 1938
PHASE_2_START <- 1939
PHASE_2_END <- 1955
PHASE_3_START <- 1956

# Analysis age range
AGE_MIN <- 15
AGE_MAX <- 45
