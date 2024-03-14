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
library(gratia) # devtools::install_github('gavinsimpson/gratia')
library(broom)
library(ggplot2)
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
           location_f = as.factor(location),
           horizon_f = ordered(horizon,
                               levels = 1:4,
                               labels = 1:4),
           log_interval_score = log(interval_score + 0.01),
           log_observed = log(observed + 0.01)
    )
  return(m.data)
}

# --- Get data ---
m.data <- prep_data()
# Plot pdf
# plot(density(m.data$log_interval_score))

# --- Model ---
m.formula <- log_interval_score ~
  # Method: random effect
  s(method_f, bs = "re") +
  # Target: random effect
  s(target_f, bs = "re") +
  # 3-wk trend (Stable/Incr/Decreasing) of observed deaths: random effect
  s(trend_f, bs = "re") +
  # Observed deaths: smoothed interacting with trend (factor smooth)
  s(observed, by = trend_f)  +
  # Horizon: smoothed ordinal factor
  s(horizon_f, bs = "fs")
#+
#  # Model: random effect (per model)
#  s(model_f, bs = "re")

# Fit GAMM with normal distribution
m.fit <- bam(formula = m.formula,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")


# Model checking ----------------------------------------------------------
# https://r.qcbs.ca/workshop08/book-en/gam-model-checking.html
# https://noamross.github.io/gams-in-r-course/chapter2

# Check output
summary(m.fit)
broom::tidy(m.fit)

# Check fit
gam.check(m.fit)
conc <- concurvity(m.fit, full = FALSE)
broom::glance(m.fit)

# extract the variance components: the random effects as their equivalent variance components that you’d see in a mixed model output
variance_comp(m.fit)

# Compare with/without interaction between incidence/trend
m.formula_no.interact <- log_interval_score ~
  s(method_f, bs = "re") + s(target_f, bs = "re") +
  s(trend_f, bs = "re") +
  s(observed)  +
  s(horizon_f, bs = "fs") + s(model_f, bs = "re")
m.fit_no.interact <- bam(formula = m.formula_no.interact,
             data = m.data,
             family = gaussian(link = "identity"),
             method = "REML")
BIC(m.fit, m.fit_no.interact) # compare BIC

# Visualisation -----------------------------------------------------------
library(itsadug)
par(mfrow = c(1, 2), cex = 1.1)
# Plot the summed effect of x0 (without random effects)
plot_smooth(m.fit, view = "observed",
            rm.ranef = TRUE)
# Plot each level of the random effect
plot_smooth(m.fit, view = "observed",
            rm.ranef = FALSE, cond = list(method_f = "1"),
            col = "orange")
plot_smooth(m.fit, view = "observed",
            rm.ranef = FALSE, cond = list(method_f = "2"),
            add = TRUE, col = "red")
plot_smooth(m.fit, view = "observed",
            rm.ranef = FALSE, cond = list(method_f = "3"),
            add = TRUE, col = "purple")

#------



m.smooths <- smooth_estimates(object = m.fit)
draw(m.smooths)
# QQ plot, residuals
appraise(m.fit)




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
     # pch = 1, cex = 0.1,
     pages = 1)




# Next steps
# - Plot GAM
# - Model under/over dispersion separately / together?
# - Model cases?

# Interpretation:
# Trend:
# 1 = Stable, 2 = Increasing, 3 = Decreasing
