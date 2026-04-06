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

# --- Session-based authentication ---
# HMD no longer supports basic auth; we need cookie-based login

hmd_login <- function(username, password) {
  # Get login page and CSRF token
  login_page <- httr::GET("https://www.mortality.org/Account/Login")
  page_content <- httr::content(login_page, as = "text", encoding = "UTF-8")
  token <- stringr::str_extract(
    page_content,
    '(?<=__RequestVerificationToken" type="hidden" value=")[^"]*'
  )
  cookies <- httr::cookies(login_page)

  # POST login
  resp <- httr::POST(
    "https://www.mortality.org/Account/Login",
    httr::set_cookies(.cookies = setNames(cookies$value, cookies$name)),
    body = list(
      Email = username,
      Password = password,
      `__RequestVerificationToken` = token
    ),
    encode = "form",
    httr::config(followlocation = TRUE)
  )

  if (httr::status_code(resp) >= 400) {
    stop("HMD login failed with status ", httr::status_code(resp))
  }

  # Return session cookies for subsequent requests
  httr::cookies(resp)
}

message("Logging in to HMD...")
session_cookies <- hmd_login(hmd_username, hmd_password)
cookie_jar <- setNames(session_cookies$value, session_cookies$name)

# --- Download functions ---

download_hmd_file <- function(country, file_type, cookie_jar) {
  base_url <- "https://www.mortality.org/File/GetDocument/hmd.v6"
  url <- paste0(base_url, "/", country, "/STATS/", file_type)

  response <- httr::GET(url, httr::set_cookies(.cookies = cookie_jar))
  if (httr::status_code(response) != 200) {
    warning("Failed to download ", file_type, " for ", country,
            " (status ", httr::status_code(response), ")")
    return(NULL)
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")

  # Check we got data, not a login page
  if (grepl("<!DOCTYPE html>", content, fixed = TRUE)) {
    warning("Got HTML instead of data for ", file_type, " / ", country,
            " — authentication may have failed")
    return(NULL)
  }

  # HMD files have variable whitespace; use read.table which handles this
  read.table(text = content, skip = 2, header = TRUE,
             stringsAsFactors = FALSE, fill = TRUE) |>
    as_tibble()
}

reshape_hmd <- function(df) {
  names(df) <- tolower(names(df))
  df |>
    filter(age != "110+") |>
    mutate(age = as.integer(age), year = as.integer(year))
}

reshape_deaths_or_exposure <- function(df) {
  reshape_hmd(df) |>
    select(year, age, female, male) |>
    pivot_longer(c(female, male), names_to = "sex", values_to = "count") |>
    mutate(count = as.numeric(count))
}

reshape_population <- function(df) {
  # Population files may have Female.1 Male.1 columns (Jan 1 vs mid-year)
  # We use the first set (Jan 1 estimates)
  reshaped <- reshape_hmd(df)
  names_lower <- tolower(names(reshaped))

  # Use just female and male columns (first occurrence)
  reshaped |>
    select(year, age, female, male) |>
    pivot_longer(c(female, male), names_to = "sex", values_to = "count") |>
    mutate(count = as.numeric(count))
}

grab_country <- function(code, cookie_jar) {
  message("Downloading ", code, "...")

  deaths <- download_hmd_file(code, "Deaths_1x1.txt", cookie_jar)
  exposure <- download_hmd_file(code, "Exposures_1x1.txt", cookie_jar)
  population <- download_hmd_file(code, "Population.txt", cookie_jar)

  if (is.null(deaths) || is.null(exposure) || is.null(population)) {
    warning("Skipping ", code, " due to download failure")
    return(NULL)
  }

  deaths_tidy <- reshape_deaths_or_exposure(deaths) |> rename(deaths = count)
  exposure_tidy <- reshape_deaths_or_exposure(exposure) |> rename(exposure = count)
  population_tidy <- reshape_population(population) |> rename(population = count)

  deaths_tidy |>
    full_join(exposure_tidy, by = c("year", "age", "sex")) |>
    full_join(population_tidy, by = c("year", "age", "sex")) |>
    mutate(country_code = code) |>
    select(country_code, year, age, sex, deaths, population, exposure) |>
    arrange(year, age, sex)
}

# --- Download all countries ---

all_data <- map(countries, grab_country, cookie_jar = cookie_jar) |>
  compact() |>
  list_rbind()

# --- Save ---

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(all_data, "data/processed/hmd_counts.csv")
message("Saved ", nrow(all_data), " rows to data/processed/hmd_counts.csv")
