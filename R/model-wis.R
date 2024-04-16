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
source(here("R", "prep-data.R"))

# --- Get data ---
m.data <- prep_data() |>
  filter(!grepl("EuroCOVIDhub-ensemble", Model) &
         Method != "Qualitative") |>
  mutate(log_interval_score = log(interval_score + 0.01))

# --- Model ---
# Formula
m.formula <- log_interval_score ~
  # -----------------------------
  # Method (3 levels*)
  Method +
  # Number of target countries (2 levels*)
  CountryTargets +
  # Affiliation same as target country
  # CountryTargetAffiliated +
  # -----------------------------
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

# Check output
summary(m.fit)
# Parametric terms
m.anova <- anova(m.fit)

# Check fit
m.aic <- glance(m.fit)$AIC
appraise(m.fit)

# Null model ---------------------------------------------------
# Formula
m_null.formula <- log_interval_score ~
  s(Incidence)  +
  Trend +
  Horizon +
  s(Model, bs = "re")
m_null.fit <- bam(formula = m_null.formula,
                   data = m.data,
                   family = gaussian(link = "identity"),
                   method = "REML")
summary(m_null.fit)
glance(m_null.fit)
m_null.anova <- anova(m_null.fit)

# Plot parametric coefficients --------
plot_coeffs <- function(m.anova) {
  coeffs_clean <- tibble(
    coeff = c("MethodStatistical",
              "MethodSemi-mechanistic",
              NA,
              "CountryTargetsMulti-country",
              NA),
    variable = c(rep("Method", 3),
                 rep("Location targets", 2)),
    level = c("Statistical", "Semi-mechanistic", "Mechanistic",
              "Multi-country", "Single-country")
  )

  coeffs <- as_tibble(x = m.anova[["p.table"]],
                      rownames = "coeff") |>
    janitor::clean_names() |>
    mutate(lower = estimate - std_error * qnorm(0.975),
           upper = estimate + std_error * qnorm(0.975))

  coeffs_table <- left_join(coeffs_clean, coeffs,
                            by = "coeff") |>
    replace_na(list(estimate=0, lower=0, upper=0)) |>
    mutate(level = fct_inorder(level),
           variable = fct_inorder(variable))

  plot_coeffs <- coeffs_table |>
    ggplot(aes(x = level, col = variable)) +
    geom_point(aes(y = estimate)) +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(y = "Partial effect", x = NULL, colour = NULL) +
    scale_colour_brewer(type = "qual", palette = 2) +
    coord_flip() +
    theme(legend.position = "bottom")
  return(plot_coeffs)
}
