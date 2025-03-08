# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
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
m.formula_uni_mod <- wis ~ s(Method, bs = "re")
m.formula_uni_tgt <- wis ~ s(CountryTargets, bs = "re")

# Full model
m.formula <- wis ~
  # Method
  s(Method, bs = "re") +
  # Number of target countries
  s(CountryTargets, bs = "re") +
  # -----------------------------
  # Trend
  s(Trend, bs = "re") +
  # Location
  s(location, bs = "re") +
  # Week
  s(time, by = location, k = 40) +
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
      control = gam.control(trace = TRUE)
    )
  })
}
# Fit
m.fits_uni_mod <- m.fit(outcomes, m.formula_uni_mod)
m.fits_uni_tgt <- m.fit(outcomes, m.formula_uni_tgt)
m.fits_full <- m.fit(outcomes, m.formula)

# --- Output handling ---
# Extract estimates for random effects
random_effects_uni <- map_df(
  c(m.fits_uni_mod, m.fits_uni_tgt),
  extract_ranef,
  .id = "outcome_target") |>
  mutate(model = "Unadjusted")

random_effects <- map_df(m.fits, extract_ranef,
                        .id = "outcome_target")|>
  mutate(model = "Adjusted") |>
  bind_rows(random_effects_uni)

checks <- map(m.fits, k.check)

saveRDS(random_effects, here("output", "random-effects.rds"))
saveRDS(checks, here("output", "checks.rds"))
