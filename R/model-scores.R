# Model: relative WIS ~ target type or method type, random effect for model
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(brms)
library(tidybayes)
library(ggplot2)
theme_set(theme_classic())


# DV: all horizons' pairwise tournament score -------------------------
# Get data
scores <- read_csv(here("data", "scores-pw.csv")) |>
  filter(scale == "log" & !grepl("EuroCOVIDhub-ensemble", model))
# Log transform response
scores <- scores |>
  mutate(rel_wis_log = log(rel_wis))
# check distribution
mean(scores$rel_wis_log); sd(scores$rel_wis_log)
plot(density(scores$rel_wis_log))

# Target type ----------------------------------------------------------
# --- Data ---
# IV: target types by model
targets <- read_csv(here("data", "targets-by-model.csv")) |>
  mutate(target_f = ifelse(target_type == "Single-country", 1, 2),
         target_f = factor(target_f))

# --- Model ---
# Data
tgt_data <- scores |>
  left_join(targets, by = "model")
# Family
tgt_family <- gaussian
# Formula
tgt_formula <- bf(rel_wis_log ~ 0 + # each type gets own intercept
                    target_f + # fixed effect for each target type
                    (1 | model)) # random intercept for model
# Priors
# get_prior(tgt_formula, tgt_data)
tgt_priors <- c(
  # Fixed effects
  prior(class = b,
        prior = normal(0,0.1)),
  # Random effects
  prior(class = sd, coef = Intercept, group = model,
        prior = normal(0,0.1)),
  # residual variance
  prior(class = sigma,
        prior = normal(0,1))
)
# Estimate
tgt_fit <- brm(#sample_prior = "only", # explore prior
               formula = tgt_formula,
               data = tgt_data,
               prior = tgt_priors,
               family = tgt_family,
               iter = 2000, warmup = 1000,
               chains = 4, cores = 4)

# --- Evaluation ---
## check posterior sample fit
pp_check(tgt_fit, ndraws = 100)
fixef(tgt_fit)
conditional_effects(tgt_fit)
plot(tgt_fit) # plot chains
summary(tgt_fit)

tgt_posterior <- as_draws_df(tgt_fit) |>
  # transform back to original scale
  mutate(across(b_target_f1:sigma, ~ exp(.)))

# Contrast
tgt_contrast <- tgt_posterior |>
  # take difference
  mutate(diff = b_target_f1 - b_target_f2) |>
  pivot_longer(cols = c(b_target_f1:sigma, diff)) |>
  group_by(name) |>
  # summarise difference
  mean_qi(value, .width = .95)

# Method type ----------------------------------------------------------
# --- Data ---
# IV: method types by model
methods <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification) |>
  mutate(method_f = case_when(method_type == "Qualitative" ~ NA,
                              method_type == "ABM" ~ NA,
                              method_type == "Mechanistic" ~ 1,
                              method_type == "Semi-mechanistic" ~ 2,
                              method_type == "Statistical"~ 3),
         method_f = factor(method_f))

# --- Model ---
# Data
mtd_data <- scores |>
  left_join(methods, by = "model")
# Family
mtd_family <- gaussian
# Formula
mtd_formula <- bf(rel_wis_log ~ 0 +
                    method_f +
                    (1 | model))
# Priors
# get_prior(mtd_formula, mtd_data)
mtd_priors <- c(
  prior(class = b,
        prior = normal(0,0.1)),
  prior(class = sd, coef = Intercept, group = model,
        prior = normal(0,0.1)),
  prior(class = sigma,
        prior = normal(0,1))
)
# Estimate
mtd_fit <- brm(
  #sample_prior = "only", # test on simulated data
  formula = mtd_formula,
  data = mtd_data,
  prior = mtd_priors,
  family = mtd_family,
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4)

# divergent transitions:
#

# --- Evaluation ---
# check posterior sample fit
pp_check(mtd_fit, ndraws = 100)

# transform back to original scale
mtd_post <- as_draws_df(mtd_fit) |>
  mutate(across(b_method_f1:sigma, ~ exp(.)))

fixef(mtd_fit)
mcmc_plot(mtd_fit)
conditional_effects(mtd_fit)
plot(mtd_fit) # plot chains
summary(mtd_fit)

# Contrast
mtd_contrast <- mtd_post |>
  # take difference
  mutate(diff = b_method_f1 - b_method_f3) |> # mechanistic v statistical
  pivot_longer(cols = c(b_method_f1:sigma, diff)) |>
  group_by(name) |>
  # summarise difference
  mean_qi(value, .width = .95)

# HDI

# ROPE

