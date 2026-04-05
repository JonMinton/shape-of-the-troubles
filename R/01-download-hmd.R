# Download and prepare HMD data for Northern Ireland and comparator countries
#
# This script downloads fresh data from the Human Mortality Database.
# You need an HMD account: https://www.mortality.org/
#
# Usage:
#   Set HMD_USERNAME and HMD_PASSWORD environment variables, then:
#   Rscript R/01-download-hmd.R
#
# Output: data/processed/hmd_counts.csv

library(tidyverse)

# --- Configuration ---
source("R/config.R")

countries <- unique(c(UK_CODES, WESTERN_EUROPE_CODES))

hmd_username <- Sys.getenv("HMD_USERNAME")
hmd_password <- Sys.getenv("HMD_PASSWORD")

if (hmd_username == "" || hmd_password == "") {
  stop("Set HMD_USERNAME and HMD_PASSWORD environment variables.\n",
       "Register at https://www.mortality.org/ if you don't have an account.")
}

# --- Download functions ---

download_hmd_file <- function(country, file_type, username, password) {
  base_url <- "https://www.mortality.org/File/GetDocument/hmd.v6"
  url <- paste0(base_url, "/", country, "/STATS/", file_type)

  response <- httr::GET(url, httr::authenticate(username, password))
  if (httr::status_code(response) != 200) {
    warning("Failed to download ", file_type, " for ", country,
            " (status ", httr::status_code(response), ")")
    return(NULL)
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  read.table(text = content, skip = 2, header = TRUE,
             stringsAsFactors = FALSE) |>
    as_tibble()
}

reshape_hmd <- function(df) {
  names(df) <- tolower(names(df))
  df |>
    select(-total) |>
    filter(age != "110+") |>
    mutate(age = as.integer(age), year = as.integer(year)) |>
    pivot_longer(c(female, male), names_to = "sex", values_to = "count") |>
    mutate(count = as.numeric(count))
}

grab_country <- function(code, username, password) {
  message("Downloading ", code, "...")

  deaths <- download_hmd_file(code, "Deaths_1x1.txt", username, password)
  exposure <- download_hmd_file(code, "Exposures_1x1.txt", username, password)
  population <- download_hmd_file(code, "Population.txt", username, password)

  if (is.null(deaths) || is.null(exposure) || is.null(population)) {
    warning("Skipping ", code, " due to download failure")
    return(NULL)
  }

  deaths_tidy <- reshape_hmd(deaths) |> rename(deaths = count)
  exposure_tidy <- reshape_hmd(exposure) |> rename(exposure = count)
  population_tidy <- reshape_hmd(population) |> rename(population = count)

  deaths_tidy |>
    full_join(exposure_tidy, by = c("year", "age", "sex")) |>
    full_join(population_tidy, by = c("year", "age", "sex")) |>
    mutate(country_code = code) |>
    select(country_code, year, age, sex, deaths, population, exposure) |>
    arrange(year, age, sex)
}

# --- Download all countries ---

all_data <- map(countries, grab_country,
                username = hmd_username,
                password = hmd_password) |>
  compact() |>
  list_rbind()

# --- Save ---

write_csv(all_data, "data/processed/hmd_counts.csv")
message("Saved ", nrow(all_data), " rows to data/processed/hmd_counts.csv")
