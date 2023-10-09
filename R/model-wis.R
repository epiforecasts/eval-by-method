# Model: WIS ~ target type + method type, random effect for model
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
  filter(scale == "natural") |>
  filter(!grepl("EuroCOVIDhub", model))
plot(density(wis$interval_score))

# explanatory vars ------
# target type; method type; horizon
# random effect: model
targets <- read_csv(here("data", "targets-by-model.csv")) |>
  mutate(target_f = ifelse(target_type == "Single-country", 1, 2),
         target_f = factor(target_f))
methods <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification) |>
  mutate(method_f = case_when(method_type == "Qualitative" ~ NA,
                              method_type == "Mechanistic" ~ 1,
                              method_type == "Semi-mechanistic" ~ 2,
                              method_type == "Statistical"~ 3),
         method_f = factor(method_f))


# ---------------------------------------------------------------------
m.data <- wis |>
  left_join(targets, by = "model") |>
  left_join(methods, by = "model") |>
  filter(!grepl("Qualitative", method_type)) |>
  mutate(log_score = ifelse(interval_score == 0, 0.1, interval_score),
         log_score = log(log_score))
plot(density(m.data$log_score))

# --- Model ---
# Family
m.family <- gaussian
# Formula
m.formula <- bf(log_score ~ 0 +
                    method_f + target_f +
                       (1 | model))
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
  # sample_prior = "only", # test on simulated data
  formula = m.formula,
  data = m.data,
  prior = m.priors,
  family = m.family,
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4)

summary(m.fit)
pp_check(m.fit, ndraws=100)
