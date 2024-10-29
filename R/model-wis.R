# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gratia) # devtools::install_github('gavinsimpson/gratia')
library(broom)
library(ggplot2)
library(broom)
library(gammit)
theme_set(theme_classic())
source(here("R", "prep-data.R"))

# --- Get data ---
metadata <- read_csv(here("data", "model-classification.csv"))
targets <- read_csv(here("data", "targets-by-model.csv"))

m.data <- prep_data(scoring_scale = "log") |>
  filter(!grepl("EuroCOVIDhub-", Model)) |>
  mutate(location = factor(location)) |>
  group_by(location) |>
  mutate(time = as.numeric(forecast_date - min(forecast_date)) / 7,
         Horizon = as.numeric(Horizon)) |>
  ungroup()

# --- Model ---
# Formula
m.formula <- log_interval_score ~
  # -----------------------------
  # Method (3 levels*)
  Method +
  # Number of target countries (2 levels*)
  CountryTargets +
  # Trend (3 levels)
  Trend +
  # Location (country)
  s(location, bs = "re") +
  # Affiliation same as target country
  # CountryTargetAffiliated +
  # -----------------------------
  # Observed incidence: interacting with trend; thin plate reg. spline (default)
  s(time, by = location) +
  # Horizon (4 levels, ordinal)
  s(Horizon, k = 3, by = Model) +
  # Individual model (35 levels*): random effect, nested within method
  s(Model, bs = "re")

# Fit GAMM with normal distribution
m.fit <- bam(formula = m.formula,
             data = m.data,
             family = gaussian())

# Check output
# summary(m.fit)
# Parametric terms
# m.anova <- anova(m.fit)

# Check fit
# m.aic <- glance(m.fit)$AIC
# appraise(m.fit)

plot_fixed_coeffs <- function(fit, effects = NULL) {
  fixed_effects <- attr(m.fit$pterms, "term.labels")
  fixed <- extract_fixed(m.fit) |>
    mutate(
      effect = NA_character_,
      variable = NA_character_
    )
  levels <- c()
  for (fixed_effect in fixed_effects) {
    fixed <- fixed |>
      mutate(
        effect = if_else(
          grepl(paste0("^", fixed_effect), term),
          fixed_effect,
          effect
        ),
        variable = if_else(
          effect == fixed_effect,
          sub(paste0("^", fixed_effect), "", term),
          variable
        )
      ) |>
      complete(
        variable = levels(m.data[[fixed_effect]]),
        fill = list(
          value = 0, lower_2.5 = 0, upper_97.5 = 0, effect = fixed_effect
        )
      )
    levels <- c(levels, levels(m.data[[fixed_effect]]))
  }
  fixed <- fixed |>
    filter(!is.na(variable)) |>
    select(effect, variable, value, lower_2.5, upper_97.5)
  max_effect <- max(abs(fixed$lower_2.5), abs(fixed$upper_97.5))

  fixed |>
    mutate(
      effect = factor(effect, levels = unique(effect)),
      variable = factor(variable, levels = rev(levels))
    )
  if (!is.null(effects)) {
    fixed <- fixed |>
      filter(effect %in% effects)
  }
  fixed <- fixed |>
    ggplot(aes(x = variable, y = value, colour = effect)) +
    geom_point() +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(y = "Partial effect", x = NULL, col = NULL) +
    scale_colour_brewer(type = "qual", palette = 2) +
    expand_limits(y = c(-max_effect, max_effect)) +
    coord_flip() +
    theme(legend.position = "bottom")
}

plot_models <- function(fit) {
  extract_ranef(m.fit) |>
    filter(group_var == "Model") |>
    left_join(metadata |> rename(group = model)) |>
    left_join(targets |> rename(group = model)) |>
    mutate(group = sub(".*-", "", group)) |> ## remove institution identifier
    select(-group_var) |>
    arrange(desc(value)) |>
    mutate(group = factor(group, levels = as.character(group))) |>
    ggplot(aes(x = group, col = classification, shape = target_type)) +
    geom_point(aes(y = value)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = 2) +
    coord_flip() +
    theme(legend.position = "bottom")
}
