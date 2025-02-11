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
data <- prep_data(scoring_scale = "log")
outcomes <- unique(data$outcome_target)
classification <- classify_models()
targets <- table_targets(data)

m.data <- data |>
  filter(!grepl("EuroCOVIDhub-", Model)) |>
  mutate(location = factor(location)) |>
  group_by(location) |>
  mutate(time = as.numeric(forecast_date - min(forecast_date)) / 7,
         Horizon = as.numeric(Horizon)) |>
  ungroup()

# --- Model ---
# Formula
m.formula <- log_wis ~
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
m.fits <- outcomes |>
  set_names() |>
  map(\(outcome) {
  bam(
    formula = m.formula,
    data = m.data |> filter(outcome_target == outcome),
    family = gaussian()
  )
})

plot_models <- function(fits, scores, x_labels = TRUE) {
  outcomes <- unique(scores$outcome_target)
  classification <- classify_models() |>
    rename(group = model)
  targets <- table_targets(scores) |>
    select(group = Model, CountryTargets) |>
    distinct()
  plots <- map(fits, function(fit) {
    plot <- extract_ranef(fit) |>
      filter(group_var == "Model") |>
      left_join(classification) |>
      left_join(targets) |>
      mutate(group = sub(".*-", "", group)) |> ## remove institution identifier
      select(-group_var) |>
      arrange(value) |>
      mutate(group = factor(group, levels = unique(as.character(group)))) |>
      ggplot(aes(x = group, col = classification, shape = CountryTargets)) +
      geom_point(aes(y = value)) +
      geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
      geom_hline(yintercept = 0, lty = 2) +
      labs(y = "Partial effect", x = "Model", colour = NULL, shape = NULL) +
      scale_colour_brewer(type = "qual", palette = 2) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      )
    if (!x_labels) {
      plot <- plot +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    }
    return(plot)
  })
  ## remove legends
  if (length(plots) > 1) {
    for (i in seq_len(length(plots) - 1)) {
      plots[[i]] <- plots[[i]] + theme(legend.position = "none")
    }
  }
  for (i in seq_along(plots)) {
    plots[[i]] <- plots[[i]] + ggtitle(outcome_targets[i])
  }
  Reduce(`+`, plots) + plot_layout(ncol = 1)
}

plot_effects <- function(fits, scores) {
  map(fits, extract_ranef) |>
    bind_rows(.id = "outcome_target") |>
    filter(!(group_var %in% c("Model", "location"))) |>
    mutate(group = factor(group, levels = unique(as.character(rev(group))))) |>
    ggplot(aes(x = group, col = group_var)) +
    geom_point(aes(y = value)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
    facet_wrap(~outcome_target, scales = "free_y") +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}
