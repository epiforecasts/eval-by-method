# Model:
# WIS on the log scale ~
# target type + method type,
# + level of incidence : fixed effect
# + trend of incidence : fixed effect
# + horizon: fixed effect
# + model : group effect

library(here)
library(dplyr)
library(readr)
library(tidyr)
library(brms)
library(tidybayes)
library(bayestestR)
library(loo)
library(ggplot2)
theme_set(theme_classic())

# Get raw WIS
wis <- read_csv(here("data", "scores-raw.csv")) |>
  # Select log or natural scale for WIS ?
  filter(scale == "natural") |>
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
# Incidence level + trend
obs <- read_csv(here("data", "observed.csv")) |>
  mutate(trend_f = case_when(trend == "Stable" ~ 1,
                             trend == "Increasing" ~ 2,
                             trend == "Decreasing" ~ 3),
         trend_f = factor(trend_f))

# ---------------------------------------------------------------------
m.data <- wis |>
  left_join(obs, by = c("location", "target_end_date")) |>
  left_join(targets, by = "model") |>
  left_join(methods, by = "model") |>
  mutate(score = ifelse(interval_score == 0, 0.1, interval_score),
         score = log(score))
# Plot pdf
plot(density(m.data$score))

# --- Model ---
# Formula
# Model: WIS ~ target type + method type,
# + random effect for model
#  + horizon
#  + level of incidence
#  + trend of incidence
m.formula <- bf(score ~ 0 +
                  method_f + target_f +
                  observed +
                  trend_f +
                  horizon +
                  (1 | model)
                )
# Family
m.family <- gaussian
# Priors
# get_prior(m.formula, m.data)
m.priors <- c(
  prior(class = b,
        prior = normal(0,1)),
  prior(class = sd, coef = Intercept, group = model,
        prior = normal(0,1)),
  prior(class = sigma,
        prior = normal(0,1))
)

# Estimate
m.fit <- brm(
  sample_prior = "only", # test on simulated data
  formula = m.formula,
  data = m.data,
  prior = m.priors,
  family = m.family,
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4)

summary(m.fit)
pp_check(m.fit, ndraws=100)
