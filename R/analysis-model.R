# Aim: use a GAMM to model the effects of model structure and country target type on WIS
# Model:
# Method: model method (mechanistic, statistical, etc.)
# CountryTargets: model predicts for single- vs multi-country
# Trend: epidemic trend (stable, increasing, decreasing)
# Incidence: log of current incidence level (smooth)
# Location: location (random effect)
# VariantPhase: dominant variant phase (random effect)
# Horizon: forecast horizon (smooth, by model)
# Model: individual model (random effect)
#
# Response: WIS (log-transformed, Gaussian family with log link)

library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
library(gratia)
library(ggplot2)
source(here("R", "process-data.R"))

# Joint-model RHS: default formula for model_wis()
m.formula_joint <- wis ~
  Epi_target +
  s(Method, bs = "re") +
  s(CountryTargets, bs = "re") +
  s(Incidence) +
  s(Trend, bs = "re") +
  s(Location, bs = "re") +
  s(VariantPhase, bs = "re") +
  s(Horizon, by = Model, k = 3, bs = "sz") +
  s(Model, bs = "re")

# Univariate formulas (one term each), for unadjusted estimates
m.formulas_uni <- list(
  method = wis ~ s(Method, bs = "re"),
  epi_target = wis ~ Epi_target,
  target = wis ~ s(CountryTargets, bs = "re"),
  incidence = wis ~ s(Incidence),
  trend = wis ~ s(Trend, bs = "re"),
  location = wis ~ s(Location, bs = "re"),
  variant = wis ~ s(VariantPhase, bs = "re"),
  horizon = wis ~ s(Horizon, by = Model, k = 3, bs = "sz"),
  model = wis ~ s(Model, bs = "re")
)

# --- Data handling ---
# Takes process_data() output; filters and transforms for modelling
prepare_model_data <- function(data, scoring_scale = "log") {
  if (!scoring_scale %in% c("log", "natural")) {
    stop("scoring_scale must be either 'log' or 'natural'")
  }
  data <- data |>
    filter(!grepl("EuroCOVIDhub-", Model)) |>
    filter(!is.na(wis)) |> # drop unscored forecasts explicitly
    mutate(Epi_target = as.factor(epi_target))

  if (scoring_scale == "log") {
    # log-transform incidence to match scoring on log scale
    data <- data |>
      mutate(Incidence = log(Incidence + 1))
  }
  return(data)
}

# Family to fit under each scoring scale / response combination
wis_family <- function(scoring_scale = "log", family_link = "log") {
  if (scoring_scale == "log") {
    gaussian(link = family_link)
  } else {
    Gamma(link = family_link)
  }
}

# --- Model fitting ---
fit_wis <- function(formula, data, family) {
  bam(
    formula = formula,
    data = data,
    family = family,
    method = "fREML",
    control = gam.control(trace = TRUE),
    discrete = TRUE
  )
}

fit_univariate <- function(data, family, formulas = m.formulas_uni) {
  map(formulas, \(f) fit_wis(f, data, family))
}

# --- Output handling ---
# Epi_target is a fixed parametric term, so it is not returned by extract_ranef.
# Pull the Deaths-vs-Cases contrast from the parametric table and shape it to
# match the random-effects columns, so downstream plotting/tables treat it as a
# pseudo group_var = "Epi_target".
extract_fixed_effect <- function(fit, model_label) {
  fe <- gammit::extract_fixed(fit)
  ci_cols <- grep("^lower_|^upper_", names(fe), value = TRUE)
  fe |>
    filter(term == "Epi_targetDeaths") |>
    transmute(
      group_var = "Epi_target",
      group = "Deaths",
      value,
      se,
      lower_2.5 = .data[[ci_cols[grepl("^lower", ci_cols)]]],
      upper_97.5 = .data[[ci_cols[grepl("^upper", ci_cols)]]],
      model = model_label
    )
}

# Combine unadjusted (univariate) and adjusted (joint) effects.
# fits_uni = NULL for a joint-only run (returns "Adjusted" rows only).
extract_all_effects <- function(fits_uni, fit_joint) {
  effects <- extract_ranef(fit_joint) |>
    mutate(model = "Adjusted") |>
    bind_rows(extract_fixed_effect(fit_joint, "Adjusted"))

  if (!is.null(fits_uni)) {
    # Univariate random effects (exclude smooth-only and the fixed target fit)
    effects_uni <- fits_uni[
      !grepl("horizon|incidence|epi_target", names(fits_uni))
    ] |>
      map(extract_ranef) |>
      list_rbind() |>
      mutate(model = "Unadjusted") |>
      bind_rows(extract_fixed_effect(fits_uni$epi_target, "Unadjusted"))
    effects <- bind_rows(effects, effects_uni)
  }
  return(effects)
}

save_model_outputs <- function(
  fit_joint,
  data,
  effects,
  output_dir = "output"
) {
  dir.create(here(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

  results <- list(
    effects = effects,
    checks = k.check(fit_joint),
    formula = fit_joint$formula
  )
  saveRDS(results, here(output_dir, "results.rds"))

  # Observed vs fitted, for a model-fit diagnostic plot in the supplement.
  # NAs are filtered upstream, so data rows align 1:1 with the fitted values.
  stopifnot(nrow(data) == length(fit_joint$y))
  fit_obs <- tibble::tibble(
    observed = fit_joint$y,
    fitted = fitted(fit_joint),
    epi_target = data$epi_target
  )
  saveRDS(fit_obs, here(output_dir, "fit_obs.rds"))

  # save png appraise() plots
  p <- appraise(fit_joint)
  ggsave(here(output_dir, "plots", "check_joint.png"), p, dpi = 300)
}

# --- Orchestrator ---
# Fits univariate (optional) and joint models, saves effects + diagnostics.
# Returns the joint fit invisibly for further inspection.
model_wis <- function(formula = m.formula_joint,
                      scoring_scale = "log", family_link = "log",
                      univariate = TRUE,
                      output_dir = "output") {
  m.data <- prepare_model_data(process_data(scoring_scale = scoring_scale),
                               scoring_scale, response)
  m.family <- wis_family(scoring_scale, family_link, response)

  m.fits_uni <- NULL
  if (univariate) {
    message("--------fitting univariate models")
    m.fits_uni <- fit_univariate(m.data, m.family)
  }
  message("--------fitting joint model")
  m.fit_joint <- fit_wis(formula, m.data, m.family)

  effects <- extract_all_effects(m.fits_uni, m.fit_joint)
  save_model_outputs(m.fit_joint, m.data, effects, output_dir)
  invisible(m.fit_joint)
}
