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
  # -----------------------------
  # Method (3 levels*)
  s(Method, bs = "re") +
  # Number of target countries (2 levels*)
  s(CountryTargets, bs = "re") +
  # Trend (3 levels)
  s(Trend, bs = "re") +
  # Location (country)
  s(location, bs = "re") +
  # Affiliation same as target country
  # CountryTargetAffiliated +
  # -----------------------------
  # Observed incidence: interacting with trend; thin plate reg. spline (default)
  s(time, by = location) +
  # Horizon (4 levels, ordinal)
  s(Horizon, k = 3, by = Model) +
  # Individual model (35 levels*): random effect, nested within method
  s(Model, bs = "re")

# Fit GAMM with normal distribution
m.fits <- outcomes |>
  set_names() |>
  map(\(outcome) {
    bam(
      formula = m.formula,
      data = m.data |> filter(outcome_target == outcome),
      family = gaussian(link = "log")
    )
  })

saveRDS(m.fits, here("output", "fits.rds"))
