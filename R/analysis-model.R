# Aim: use a GAMM to model the effects of model structure and country target type on WIS
# Model:
# Method: model method (mechanistic, statistical, etc.)
# CountryTargets: model predicts for single- vs multi-country
# Trend: epidemic trend (stable, increasing, decreasing)
# Incidence: current incidence level (smooth)
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
source(here("R", "process-data.R"))

model_wis <- function(scoring_scale = "log", output_dir = "output") {
  # --- Get data ---
  m.data <- process_data(scoring_scale = scoring_scale)
  m.data <- m.data |>
    filter(!grepl("EuroCOVIDhub-", Model))
  # log-transform incidence to match scoring on log scale
  if (scoring_scale == "log") {
    m.data <- m.data |>
      mutate(Incidence = log(Incidence + 1))
  }
  outcomes <- unique(m.data$outcome_target)

  # --- Model formula ---
  # Univariate for each explanatory variable
  m.formulas_uni <- list(
    method = wis ~ s(Method, bs = "re"),
    target = wis ~ s(CountryTargets, bs = "re"),
    incidence = wis ~ s(Incidence),
    trend = wis ~ s(Trend, bs = "re"),
    location = wis ~ s(Location, bs = "re"),
    variant = wis ~ s(VariantPhase, bs = "re"),
    horizon = wis ~ s(Horizon, by = Model, k = 3, bs = "sz"),
    model = wis ~ s(Model, bs = "re")
  )

  # Full joint model
  m.formula_joint <- wis ~
    s(Method, bs = "re") +
    s(CountryTargets, bs = "re") +
    s(Incidence) +
    s(Trend, bs = "re") +
    s(Location, bs = "re") +
    s(VariantPhase, bs = "re") +
    s(Horizon, by = Model, k = 3, bs = "sz") +
    s(Model, bs = "re")

  # --- Model fitting ---
  # Set up to fit to each outcome target (cases, deaths)
  m.fit <- function(outcomes, m.formula) {
    outcomes |>
      set_names() |>
      map(\(outcome) {
        bam(
          formula = m.formula,
          data = m.data |> filter(outcome_target == outcome),
          family = gaussian(link = "log"),
          method = "fREML",
          control = gam.control(trace = TRUE),
          discrete = TRUE
        )
      })
  }
  # Fit
  message("--------fitting univariate models")
  m.fits_uni <- map(m.formulas_uni, ~ m.fit(outcomes, .x))

  message("--------fitting joint model")
  m.fits_joint <- m.fit(outcomes, m.formula_joint)

  # --- Output handling ---
  # Extract estimates for random effects
  random_effects_uni <- m.fits_uni[!grepl("horizon|incidence", names(m.fits_uni))] |>
    map_depth(.depth = 2, ~ extract_ranef(.x)) |>
    map(~ list_rbind(.x, names_to = "outcome_target")) |>
    list_rbind() |>
    mutate(model = "Unadjusted")

  random_effects_joint <- map_df(m.fits_joint,
                                 extract_ranef,
                                 .id = "outcome_target") |>
    mutate(model = "Adjusted")

  random_effects <- random_effects_joint |>
    bind_rows(random_effects_uni)

  # Extract model checks
  checks <- map(m.fits_joint, k.check)
  formula <- m.fits_joint[[1]]$formula
  results <- list(
    effects = random_effects,
    checks = checks,
    formula = formula
  )

  saveRDS(results, here(output_dir, "results.rds"))

  iwalk(m.fits_joint, \(x, target) {
    p <- appraise(x)
    ggsave(here(output_dir, "plots", paste0("check_", target, ".pdf")), p)
  })
}
