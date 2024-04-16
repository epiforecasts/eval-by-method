# Model:
# WIS (log-transformed for normality) ~
# target type + method type,
# + level of incidence : fixed effect
# + trend of incidence : fixed effect
# + horizon: fixed effect
# + model : group effect
library(here)
library(dplyr)
library(readr)
library(tidyr)
theme_set(theme_classic())

# load data from
# source(here('R', 'model-wis.R'); prep_data()

# --- BRMS ---
library(brms)
# Formula
m.formula <- bf(log_interval_score ~ 0 +
                  method_f + target_f +
                  observed +
                  trend_f +
                  horizon_f +
                  (1 | model_f)
)

# Family
m.family <- gaussian(link = "identity")
# Priors
# get_prior(m.formula, m.data)
m.priors <- c(
  prior(class = b,
        prior = normal(0,1)),
  prior(class = sd,
        coef = Intercept,
        group = model,
        prior = normal(0,1)),
  prior(class = sigma,
        prior = normal(0,1))
)
# Estimate
m.fit <- brm(
  #sample_prior = "only", # test on simulated data
  formula = m.formula,
  data = m.data,
  prior = m.priors,
  family = m.family,
  algorithm = "meanfield", # alternative to mcmc sampling for speed
  iter = 2000, warmup = 1000,
  chains = 4, cores = 4)

pp_check(m.fit, ndraws=100)
summary(m.fit)
