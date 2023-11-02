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
library(mgcv)
library(broom)
theme_set(theme_classic())

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
  # Incidence level + trend
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
           location_f = as.factor(location),
           horizon_f = ordered(horizon,
                               levels = 1:4,
                               labels = 1:4),
           log_interval_score = ifelse(interval_score == 0, 0.1,
                                   interval_score),
           log_interval_score = log(log_interval_score)
    )
  return(m.data)
}

# --- Get data ---
m.data <- prep_data()
# Plot pdf
# plot(density(m.data$log_interval_score))

# --- Model ---
m.formula <- log_interval_score ~
  # Method: linear factor
  method_f +
  # Target: linear factor
  target_f +
  # 3-week trend of observed deaths: linear factor
  trend_f +
  # Observed deaths: smoothed continuous
  s(observed)  +
  # Horizon: smoothed ordinal factor
  s(horizon_f, bs = "fs") +
  # Model: random effect (per model)
  s(model_f, bs = "re")

# Fit GAM
m.fit <- bam(formula = m.formula,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")

# Check output
summary(m.fit)
broom::tidy(m.fit)

# Check fit
gam.check(m.fit)
concurvity(m.fit, full = F)
broom::glance(m.fit)

# --- Visualise ---
plot(m.fit, all.terms = TRUE,
     # exponentiate because we log-transformed earlier
     trans = exp,
     # Shift by intercept
     shift = coef(m.fit)[1],
     # combine uncertainty with SE of model intercept
     seWithMean = TRUE,
     pages = 1)

vis.gam(m.fit)
vis.gam(m.fit,
        plot.type = "pers",
        phi = 30,
        theta = 300
        )
points(m.data)


plot(m.fit,
     trans = exp,
     shade = TRUE,
     rug = TRUE,
     residuals = TRUE,
     #pch = 1, cex = 0.1,
     pages = 1)

# Alternative visualisation
# devtools::install_github('gavinsimpson/gratia')
library(gratia)
# plot smooth terms
draw(m.fit)
appraise(m.fit)
draw.evaluated_re_smooth(m.fit)
m.smooths <- smooth_estimates(m.fit, smooth = "horizon", partial_match = T)
draw(m.smooths)
