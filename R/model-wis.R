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
  s(Method, bs = "re") +
  # Number of target countries (2 levels*)
  s(CountryTargets, bs = "re") +
  # Trend (3 levels)
  s(Trend, bs = "re") +
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

plot_models <- function(fit) {
  extract_ranef(m.fit) |>
    filter(group_var == "Model") |>
    left_join(metadata |> rename(group = model)) |>
    left_join(targets |> rename(group = model)) |>
    mutate(group = sub(".*-", "", group)) |> ## remove institution identifier
    select(-group_var) |>
    arrange(value) |>
    mutate(group = factor(group, levels = as.character(group))) |>
    ggplot(aes(x = group, col = classification, shape = target_type)) +
    geom_point(aes(y = value)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2) +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = 2) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
}

plot_effects <- function(fit) {
  extract_ranef(m.fit) |>
    filter(!(group_var %in% c("Model", "location"))) |>
    mutate(group = factor(group, levels = as.character(rev(group)))) |>
    ggplot(aes(x = group, col = group_var)) +
    geom_point(aes(y = value)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = "Dark2") +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}

p <- plot_models(m.fit)
ggsave("model_performance.png", p, width = 7.5, height = 3.5)

effects <- plot_effects(m.fit)
ggsave("model_effects.png", effects, width = 6, height = 3.5)
