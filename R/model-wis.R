# Aim: use a GAMM to model the effects of model structure and country target type on WIS (calculated on log scale)
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(mgcv)
library(gratia) # devtools::install_github('gavinsimpson/gratia')
library(itsadug)
library(broom)
library(ggplot2)
theme_set(theme_classic())

# --- Get data ---
prep_data <- function() {
  # Get raw WIS
  wis <- read_csv(here("data", "scores-raw.csv")) |>
    filter(scale == "log") |>
    filter(!grepl("EuroCOVIDhub", model))

  # Extra explanatory vars ------
  # Target type
  targets <- read_csv(here("data", "targets-by-model.csv")) |>
    mutate(target_f = ifelse(target_type == "Single-country", 1, 2),
           target_f = factor(target_f))
  # Method type
  methods <- read_csv(here("data", "model-classification.csv")) |>
    select(model, method_type = classification) |>
    mutate(method_f = case_when(method_type == "Qualitative" ~ NA,
                                method_type == "Mechanistic" ~ 1,
                                method_type == "Semi-mechanistic" ~ 2,
                                method_type == "Statistical"~ 3),
           method_f = factor(method_f))
  # Incidence level + trend (see: R/import-data.r)
  obs <- read_csv(here("data", "observed.csv")) |>
    mutate(trend_f = case_when(trend == "Stable" ~ 1,
                               trend == "Increasing" ~ 2,
                               trend == "Decreasing" ~ 3),
           trend_f = factor(trend_f))

  m.data <- wis |>
    left_join(obs, by = c("location", "target_end_date")) |>
    left_join(targets, by = "model") |>
    left_join(methods, by = "model") |>
    mutate(model_f = as.factor(model),
           horizon_f = ordered(horizon,
                               levels = 1:4, labels = 1:4),
           log_interval_score = log(interval_score + 0.01)
    )
  return(m.data)
}
m.data <- prep_data()
# plot(density(m.data$log_interval_score)) # Plot pdf

# --- Model ---
# Formula
m.formula <- log_interval_score ~
  # Method (3 levels*): random effect
  s(method_f, bs = "re") +
  # Number of target countries (2 levels*): random effect
  s(target_f, bs = "re") +
  # Observed incidence: interacting with trend
  s(observed, by = trend_f)  +
  # Trend (3 levels*): random effect
  s(trend_f, bs = "re") +
  # Horizon (4 levels, ordinal): smoothed factor
  s(horizon_f, bs = "fs") +
  # Individual model (35 levels*): random effect
  s(model_f, bs = "re")

# Fit GAMM with normal distribution
m.fit <- bam(formula = m.formula,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")

# Model checking ----------------------------------------
# * Key to factors:
# Method: 1 = Mechanistic, 2 = Semi-mech, 3 = Statistical
# Target: 1 = Single country, 2 = Multi-country
# Trend: 1 = Stable, 2 = Increasing, 3 = Decreasing
# Model: 35 included (excl. 3 qualitative models)

# Check output
summary(m.fit)

# Check fit
glance(m.fit)

# Plot smooth terms
draw(m.fit)

# Get the smooth estimates
smooth_estimates(m.fit)

# QQ plot, residuals
appraise(m.fit)

# Concurvity: checking for (generalisation of) collinearity
concurvity(m.fit, full = FALSE)

# Random effects as their equivalent variance components
variance_comp(m.fit)

# Model selection -------
# Tested alternative model terms using REML
# - Removing trend as separate term [s(trend_f, bs = "re")]
# - Re-specifying interaction as factor smooth [s(observed, trend_f, bs = "fs")]
# - Removing model as a term [s(model_f, bs = "re")]

# Usual model selection scores (AIC/BIC) don't work well for smooths [?gam.selection]

