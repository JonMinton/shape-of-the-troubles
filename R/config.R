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

# All HMD country codes for systematic scan
ALL_HMD_CODES <- c(
  # UK components
  "GBR_NIR", "GBR_NP", "GBR_SCO", "GBRCENW", "GBRTENW",
  # Western Europe
  "AUT", "BEL", "CHE", "DEUTNP", "DEUTE", "DEUTW",
  "FRATNP", "FRACNP", "IRL", "LUX", "NLD",
  # Southern Europe
  "ESP", "GRC", "ITA", "PRT",
  # Scandinavia
  "DNK", "FIN", "ISL", "NOR", "SWE",
  # Central & Eastern Europe
  "BGR", "CZE", "HRV", "HUN", "POL", "SVK", "SVN",
  # Baltic states
  "EST", "LTU", "LVA",
  # Former Soviet Union
  "BLR", "RUS", "UKR",
  # Middle East
  "ISR",
  # Americas
  "CAN", "CHL", "USA",
  # Asia-Pacific
  "AUS", "JPN", "KOR", "NZL_MA", "NZL_NM", "NZL_NP", "TWN"
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
