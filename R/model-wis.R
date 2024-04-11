# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(mgcv)
library(gratia) # devtools::install_github('gavinsimpson/gratia')
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
    mutate(Targets = ifelse(target_type == "Single-country", 1, 2),
           Targets = factor(Targets))
  # Method type
  methods <- read_csv(here("data", "model-classification.csv")) |>
    select(model, method_type = classification) |>
    mutate(Method = case_when(method_type == "Qualitative" ~ NA,
                                method_type == "Mechanistic" ~ 1,
                                method_type == "Semi-mechanistic" ~ 2,
                                method_type == "Statistical"~ 3),
           Method = factor(Method))
  # Incidence level + trend (see: R/import-data.r)
  obs <- read_csv(here("data", "observed.csv")) |>
    mutate(Trend = case_when(trend == "Stable" ~ 1,
                               trend == "Increasing" ~ 2,
                               trend == "Decreasing" ~ 3),
           Trend = factor(Trend)) |>
    rename(Incidence = observed)

  m.data <- wis |>
    left_join(obs, by = c("location", "target_end_date")) |>
    left_join(targets, by = "model") |>
    left_join(methods, by = "model") |>
    mutate(Model = as.factor(model),
           Horizon = ordered(horizon,
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
  # Method (3 levels*)
  Method +
  # Number of target countries (2 levels*)
  Targets +
  # Observed incidence: interacting with trend; thin plate reg. spline (default)
  s(Incidence)  +
  # Trend (3 levels*)
  Trend +
  # Horizon (4 levels, ordinal)
  Horizon +
  # Individual model (35 levels*): random effect
  s(Model, bs = "re")

# Fit GAMM with normal distribution
m.fit <- bam(formula = m.formula,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")

# Parametric terms
m.anova <- anova(m.fit)

# Model checking
# * Key to factors:
# Method: 1 = Mechanistic, 2 = Semi-mech, 3 = Statistical
# Target: 1 = Single country, 2 = Multi-country
# Trend: 1 = Stable, 2 = Increasing, 3 = Decreasing
# Model: 35 included (excl. 3 qualitative models)

# Check output
summary(m.fit)

# Check fit
glance(m.fit)


# Without random effect ---------------------------------------------------
m.formula_no_re <- log_interval_score ~
  # Method (3 levels*)
  Method +
  # Number of target countries (2 levels*)
  Targets +
  # Observed incidence: interacting with trend; thin plate reg. spline (default)
  s(Incidence)  +
  # Trend (3 levels*)
  Trend +
  # Horizon (4 levels, ordinal)
  Horizon

m.fit_no_re <- bam(formula = m.formula_no_re,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")

# Parametric terms
m.anova_no_re <- anova(m.fit_no_re)
