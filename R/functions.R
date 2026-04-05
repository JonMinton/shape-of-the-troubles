# Shared functions for Shape of the Troubles analysis

#' Calculate log10 mortality rate with continuity correction
calc_lmr <- function(deaths, exposure, correction = 0.5) {
  mr <- (deaths + correction) / (exposure + correction)
  log(mr, base = 10)
}

#' Create Troubles decay term for a given decay rate k
#' Returns 0 before 1972, then (1-k)^(year-1972) from 1972 onwards
troubles_decay <- function(year, k) {
  ifelse(year < 1972, 0, (1 - k) ^ (year - 1972))
}

#' Prepare modelling data for NI males aged 15-45
#' Adds phase indicators and Troubles decay term
prepare_model_data <- function(dta, sex_filter = "male",
                                age_range = c(15, 45), k = 0.09748423) {
  dta |>
    filter(sex == sex_filter) |>
    filter(age >= age_range[1] & age <= age_range[2]) |>
    mutate(
      lmr = calc_lmr(deaths, exposure),
      yr_pre_38 = year <= 1938,
      early_phase = ifelse(yr_pre_38, year - 1922, 0),
      impr_phase = year >= 1939 & year <= 1955,
      yr_since_imp = ifelse(impr_phase, year - 1939, 0),
      post_55 = year > 1955,
      yr_since_imp2 = ifelse(post_55, year - 1955, 0),
      trbls_kpc = troubles_decay(year, k)
    )
}

#' Fit the full model (model 5 from original analysis)
fit_troubles_model <- function(model_data) {
  lm(lmr ~ as.factor(age) * (yr_pre_38 + early_phase + impr_phase +
       yr_since_imp + yr_since_imp2 + trbls_kpc),
     data = model_data)
}

#' Find optimal decay rate by minimising AIC
#' @param dta Raw NI data
#' @param criterion "AIC" or "BIC"
find_optimal_decay <- function(dta, criterion = "AIC") {
  obj_fn <- function(k) {
    model_data <- prepare_model_data(dta, k = k)
    mod <- fit_troubles_model(model_data)
    if (criterion == "AIC") AIC(mod) else BIC(mod)
  }
  optimize(obj_fn, interval = c(0, 1))
}

#' Estimate counterfactual excess deaths
estimate_excess_deaths <- function(dta, model, model_data) {
  # Predictions with Troubles
  preds_active <- model_data |>
    add_predictions(model, var = "pred_active") |>
    select(year, age, lmr, pred_active)

  # Predictions without Troubles (counterfactual)
  preds_counter <- model_data |>
    mutate(trbls_kpc = 0) |>
    augment(model, newdata = _) |>
    select(year, age, pred_notrbls = .fitted)

  # Combine and estimate deaths
  dta |>
    filter(sex == "male") |>
    select(year, age, deaths, exposure) |>
    right_join(preds_active, by = c("year", "age")) |>
    left_join(preds_counter, by = c("year", "age")) |>
    mutate(
      deaths_active = exposure * 10 ^ pred_active,
      deaths_counter = exposure * 10 ^ pred_notrbls,
      excess = deaths_active - deaths_counter
    )
}

#' Colour palette for Lexis surfaces (qualitative)
lexis_palette <- function(n = 200) {
  colorRampPalette(rev(brewer.pal(12, "Paired")))(n)
}

#' Colour palette for residuals (diverging red-blue)
residual_palette <- function(n = 200) {
  colorRampPalette(rev(brewer.pal(5, "RdBu")))(n)
}

#' Root mean square error
rms <- function(residuals) {
  sqrt(mean(residuals^2, na.rm = TRUE))
}
