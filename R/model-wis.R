# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
library(gratia)
source(here("R", "prep-data.R"))
source(here("R", "descriptive.R"))

# --- Get data ---
data <- prep_data(scoring_scale = "log")
outcomes <- unique(data$outcome_target)
classification <- classify_models()
targets <- table_targets(data)

m.data <- data |>
  filter(!grepl("EuroCOVIDhub-", Model)) |>
  mutate(location = factor(location)) |>
  group_by(location) |>
  mutate(
    time = as.numeric(forecast_date - min(forecast_date)) / 7,
    Horizon = as.numeric(Horizon),
    wis = wis + 1e-7
  ) |>
  ungroup()

# --- Model formula ---
# Univariate for explanatory variables
m.formula_uni_type <- wis ~ s(Method, bs = "re")
m.formula_uni_tgt <- wis ~ s(CountryTargets, bs = "re")
m.formula_uni_model <- wis ~ s(Model, bs = "re")
m.formula_uni_variant <- wis ~ s(VariantPhase, bs = "re")

# Full model
m.formula <- wis ~
  # Method
  s(Method, bs = "re") +
  # Number of target countries
  s(CountryTargets, bs = "re") +
  # -----------------------------
  # Trend
  s(Trend, bs = "re") +
  # Variant phase
  s(VariantPhase, bs = "re") +
  # Location
  s(location, bs = "re") +
  # Horizon
  s(Horizon, k = 3, by = Model, bs = "sz") +
  # Individual model
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
cat("--------fitting univariate models")
m.fits_uni_type <- m.fit(outcomes, m.formula_uni_type)
m.fits_uni_tgt <- m.fit(outcomes, m.formula_uni_tgt)
m.fits_uni_model <- m.fit(outcomes, m.formula_uni_model)
cat("--------fitting joint model")
m.fits_joint <- m.fit(outcomes, m.formula)
cat("finished fitting")

# --- Output handling ---
# Extract estimates for random effects
random_effects_uni <- map_df(
  c(m.fits_uni_type, m.fits_uni_tgt, m.fits_uni_model, m.fits_uni_variant),
  extract_ranef,
  .id = "outcome_target") |>
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

saveRDS(results, here("output", "results.rds"))

