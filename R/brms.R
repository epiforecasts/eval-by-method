library(here)
library(dplyr)
library(readr)
library(tidyr)
library(brms)
library(tidybayes)
theme_set(theme_classic())

# Predictors ----
# Get method types
method_factor <- c("Qualitative", "ABM",
                   "Mechanistic", "Semi-mechanistic", "Statistical")
names(method_factor) <- method_factor
metadata <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification) |>
  mutate(method_type = factor(method_type,
                              method_factor, names(method_factor),
                              ordered = TRUE)) |>
  filter(!method_type %in% c("Qualitative", "ABM"))

# Get target types
target_factor <- c("Single-country", "Multi-country")
names(target_factor) <- target_factor
targets <- read_csv(here("data", "targets-by-model.csv")) |>
  mutate(target_type = factor(target_type,
                              target_factor, names(target_factor),
                              ordered = TRUE))
# DV -----
# Load score data: 1 week horizon
#scores <- read_csv(here("data", "scores-pw-horizon.csv")) |>
#  filter(horizon == 1 &
#           scale == "log" & !grepl("EuroCOVIDhub-ensemble", model))

# All horizons' pairwise tournament score
scores <- read_csv(here("data", "scores-pw.csv")) |>
  filter(scale == "log" & !grepl("EuroCOVIDhub-ensemble", model))

# Data set up -----
# Scores by method type and horizon
scores <- scores |>
  left_join(metadata, by = "model") |>
  left_join(targets, by = "model")
# Log transform response
scores <- scores |>
  mutate(rel_wis_log = log(rel_wis))

# Distribution characteristics -------------------------------------------
# Mean & SD
mean(scores$rel_wis); sd(scores$rel_wis)
# Probability density
plot(density(scores$rel_wis_log))

# by method
scores |>
  group_by(method_type) |>
  summarise(mid = median(rel_wis),
            l99 = quantile(rel_wis, 0.01),
            u99 = quantile(rel_wis, 0.99),
            mean = mean(rel_wis),
            sd = sd(rel_wis))

# ---- Model: target type -----------------------------------------------
# Predictor as factor
scores <- scores |>
  mutate(target_f = ifelse(target_type == "Single-country", 1, 2),
         target_f = factor(target_f))
# Formula
tgt_formula <- bf(rel_wis_log ~ 0 + target_f)
# Family
tgt_family <- gaussian
# Priors
tgt_priors <- c(
  prior(normal(0.5,1), class = b),
  prior(normal(0,1), class = sigma)
)

## Prior predictive (ignore data, sample from prior)
#tgt_pre <- brm(sample_prior = "only",
#           formula = tgt_formula,
#           data = scores,
#           prior = tgt_priors,
#           family = tgt_family,
#           iter = 2000, warmup = 1000,
#           chains = 4, cores = 4)
#pp_check(tgt_pre, ndraws = 100)

# Estimate posterior
tgt_fit <- brm(formula = tgt_formula,
                data = scores,
                prior = tgt_priors,
                family = tgt_family,
                iter = 2000, warmup = 1000,
                chains = 4, cores = 4)
## check posterior sample fit
# pp_check(tgt_fit, ndraws = 100)

fixef(tgt_fit)
conditional_effects(tgt_fit)

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

# ----- Model: method type -----------------------------------------------
# Predictor as factor
scores <- scores |>
  mutate(mtd_f = case_when(method_type == "Statistical" ~ 1,
                           method_type == "Semi-mechanistic" ~ 2,
                           method_type == "Mechanistic" ~ 3,
                           .default = NA),
         mtd_f = as.factor(mtd_f))
# Formula
mtd_formula <- bf(rel_wis_log ~ 0 + mtd_f)
# Family
mtd_family <- gaussian
# Priors
mtd_priors <- c(
  #prior(normal(0.5,1), class = Intercept),
  prior(normal(0,1), class = b),
  prior(normal(0,1), class = sigma)
)

## Prior predictive
# mtd_pre <- brm(sample_prior = "only",
#               formula = mtd_formula,
#               data = scores,
#               prior = mtd_priors,
#               family = mtd_family,
#               iter = 2000, warmup = 1000,
#               chains = 4, cores = 4)
#pp_check(mtd_pre, ndraws = 100)

# Estimate posterior
mtd_fit <- brm(formula = mtd_formula,
               data = scores,
               prior = mtd_priors,
               family = mtd_family,
               iter = 2000, warmup = 1000,
               chains = 4, cores = 4)

# check posterior samples
pp_check(mtd_fit, ndraws = 100)
fixef(mtd_fit)
conditional_effects(mtd_fit)
# transform back to original scale
mtd_posterior <- as_draws_df(mtd_fit) |>
  mutate(across(b_mtd_f1:sigma, ~ exp(.)))
# Contrast
mtd_contrast <- mtd_posterior |>
  # take difference
  mutate(diff2_1 = b_mtd_f2 - b_mtd_f1,
         diff3_1 = b_mtd_f3 - b_mtd_f1) |>
  pivot_longer(cols = c(b_mtd_f1:sigma, diff2_1, diff3_1)) |>
  group_by(name) |>
  # summarise difference
  mean_qi(value, .width = .95)
