# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
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
  mutate(outcome_target = factor(outcome_target)) |>
  mutate(
    unique_outcome_target = as.factor(paste(
      outcome_target,
      location,
      sep = "_"
    ))
  ) |>
  group_by(location) |>
  mutate(
    time = as.numeric(forecast_date - min(forecast_date)) / 7,
    Horizon = as.numeric(Horizon),
    wis = wis + 1e-7
  ) |>
  ungroup()

# --- Model ---
# Formula
m.formula <- wis ~
  # Intercept for outcome
  outcome_target +
    # -----------------------------
    # Method (3 levels*)
    s(Method, bs = "re") +
    s(outcome_target, Method, bs = "re") +
    # Number of target countries (2 levels*)
    s(CountryTargets, bs = "re") +
    s(outcome_target, CountryTargets, bs = "re") +
    # Trend (3 levels)
    s(Trend, bs = "re") +
    s(outcome_target, Trend, bs = "re") +
    # Location (country)
    s(location, bs = "re") +
    s(location, outcome_target, bs = "re") +
    # Horizon (4 levels, ordinal)
    s(Horizon, k = 4) +
    s(Horizon, k = 4, by = outcome_target, bs = "sz") +
    s(Horizon, k = 4, by = Method, bs = "sz") +
    s(Horizon, k = 4, by = CountryTargets, bs = "sz") +
    s(Horizon, k = 4, by = Trend, bs = "sz") +
    # Individual model (35 levels*): random effect, nested within method
    s(Model, bs = "re") +
    # Observed incidence: interacting with trend; thin plate reg. spline (default)
    s(time, k = 30) +
    s(time, by = outcome_target, bs = "sz", k = 30) +
    s(time, by = unique_outcome_target, bs = "sz", k = 30)

# Fit GAMM with normal distribution
m.fits <- bam(
  formula = m.formula,
  data = m.data,
  family = gaussian(link = "log"),
  discrete = TRUE
)

saveRDS(m.fits, here("output", "fits.rds"))
