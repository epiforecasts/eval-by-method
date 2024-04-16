# Aim: describe interval score in terms of model structure and country target type
# Load data:
# source(here("R", "prep-data.R"))
# scores <- prep_data(scoring_scale = "log")
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(patchwork)
library(janitor)

# Table summary --------------------
table_confint <- function(scores, group_var = NULL) {
  if (!is.null(group_var)) {
  scores <- scores |>
    group_by(.data[[group_var]])
    }
  table <- scores |>
    summarise(n_forecasts = n(),
              p_forecasts = round(n() / nrow(scores) * 100, 1),
              n_models = length(unique(Model)),
              p_models = round(n_models / 39 * 100, 1),
              mean = mean(interval_score),
              lower = t.test(interval_score)$conf.int[1],
              upper = t.test(interval_score)$conf.int[2],
              median = median(interval_score),
              lq = quantile(interval_score, 0.25),
              uq = quantile(interval_score, 0.75)
              ) |>
    mutate(across(c(mean, lower, upper,
                    median, lq, uq), ~ round(., 2)),
           Models = paste0(n_models, " (", p_models, "%)"),
           Forecasts = paste0(n_forecasts, " (", p_forecasts, "%)"),
           "Mean interval score (95% CI)" = paste0(mean, " (",
                                                   lower, "-", upper, ")"),
           "Median (IQR)" = paste0(median, " (", lq, "-", uq, ")"))

  if (!is.null(group_var)) {
      table <- table |>
        rename("Variable" = all_of(group_var)) |>
        mutate(group = group_var)
  }
  return(table)
}

create_table1 <- function(scores) {
  overall <- table_confint(scores, "scale") |>
    mutate(Variable = "Overall", group = "")
  method <- table_confint(scores, "Method")
  targets <- table_confint(scores, "CountryTargets")
  affiliated <- table_confint(scores, "CountryTargetAffiliated") |>
                        mutate(Variable = ifelse(Variable == "True",
                                "Affiliated to target country",
                                "Located elsewhere"),
                               Models = NA)
  horizon <- table_confint(scores, "Horizon")
  trend <- table_confint(scores, "Trend")
  table1 <- bind_rows(overall, method, targets, affiliated,
                      horizon, trend)
  return(table1)
}

# Plot over time --------------------
plot_iqr_over_time <- function(scores,
                           quantiles = c(0.25, 0.5, 0.75)) {
  q_over_time <- scores |>
    group_by(target_end_date) |>
    reframe(
      n = n(),
      value = quantile(interval_score, quantiles),
      quantile = paste0("q", quantiles)) |>
    pivot_wider(names_from = quantile)
  plot_over_time <- q_over_time |>
    ggplot(aes(x = target_end_date)) +
    geom_line(aes(y = q0.5), alpha = 0.5) +
    geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
                alpha = 0.3, col = NA) +
    scale_x_date(date_labels = "%b %Y") +
    labs(x = NULL, y = "Weighted interval score (IQR)")
  return(plot_over_time)
}

plot_means_over_time <- function(scores) {
  means_over_time <- scores |>
    group_by(target_end_date) |>
    summarise(
      n_forecasts = n(),
      n_models = length(unique(Model)),
      mean = mean(interval_score),
      lower95 = t.test(interval_score)$conf.int[1],
      upper95 = t.test(interval_score)$conf.int[2],
      lower99 = t.test(interval_score, conf.level = 0.99)$conf.int[1],
      upper99 = t.test(interval_score, conf.level = 0.99)$conf.int[2])
  means_plot_over_time <- means_over_time |>
    ggplot(aes(x = target_end_date)) +
    geom_line(aes(y = mean), alpha = 0.4) +
    geom_ribbon(aes(ymin = lower95, ymax = upper95),
                alpha = 0.3) +
    geom_ribbon(aes(ymin = lower99, ymax = upper99),
                alpha = 0.1) +
    scale_x_date(date_labels = "%b %Y") +
    labs(x = NULL, y = "Weighted interval score")
  return(means_plot_over_time)
}

# Density plot  --------------------
plot_density <- function(scores) {
  plot_conditional_density <- function(scores, group_var) {
    scores |>
      ggplot(aes(x = log_interval_score,
                 col = .data[[group_var]])) +
      geom_density() +
      labs(x = "Log of the weighted interval score")
  }

  method <- plot_conditional_density(scores, "Method")
  targets <- plot_conditional_density(scores, "CountryTargets")
  affiliated <- plot_conditional_density(scores, "CountryTargetAffiliated")

  plot_density_patchwork <- method +
    targets +
    affiliated +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "A",
                    caption = "NA represents Hub ensemble model")
  return(plot_density_patchwork)
}

# Linerange plot summary --------------------
plot_linerange <- function(group_var) {
  quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)
  plot <- scores |>
    group_by(.data[[group_var]], Horizon) |>
    reframe(
      n = n(),
      value = quantile(interval_score, quantiles),
      quantile = paste0("q", quantiles)) |>
    pivot_wider(names_from = quantile) |>
    # Plot
    ggplot(aes(y = .data[[group_var]], col = Horizon, fill = Horizon)) +
    geom_point(aes(x = q0.5), alpha = 0.8,
               position = position_dodge(width = 1)) +
    geom_linerange(aes(xmin = q0.25, xmax = q0.75), linewidth = 4,
                   alpha = 0.5, position = position_dodge(width = 1)) +
    geom_linerange(aes(xmin = q0.01, xmax = q0.99), linewidth = 4,
                   alpha = 0.2, position = position_dodge(width = 1)) +
    labs(y = NULL, x = NULL,
         col = "Week ahead forecast Horizon",
         fill = "Week ahead forecast Horizon") +
    scale_color_viridis_d(direction = 1) +
    theme(legend.position = "bottom")
  return(plot)
}

# Ridge plot by model --------------------
plot_ridges <- function(scores){
  scores |>
    group_by(Model) |>
    mutate(median_score = median(interval_score),
           lq = quantile(interval_score, 0.25),
           uq = quantile(interval_score, 0.75)) |>
    ungroup() |>
    mutate(Model = fct_reorder(Model, median_score)) |>
    filter(interval_score >= lq & interval_score <= uq) |>
    # Plot
    ggplot(aes(x = interval_score, y = Model, fill = stat(x))) +
    geom_density_ridges_gradient(scale = 1.5,
                                 rel_min_height = 0.01,
                                 quantile_lines = TRUE, quantiles = 2) +
    scale_fill_viridis_c(name = "Interval score",
                         option = "C", direction = -1) +
    theme_ridges() +
    labs(x = "Interval score IQR", y = "Model") +
    theme(legend.position = "none")
}

# Table of targets by model -------------
table_targets <- function(scores) {
  table_targets <- scores |>
    select(Model, forecast_date, location) |>
    distinct() |>
    group_by(Model, forecast_date) |>
    summarise(target_count = n()) |>
    ungroup() |>
    group_by(Model) |>
    summarise(CountryTargets = all(target_count <= 2),
              min_targets = min(target_count),
              max_targets = max(target_count),
              mean = mean(target_count),
              consistent = min_targets==max_targets) |>
    mutate(CountryTargets = factor(CountryTargets,
                                   levels = c(TRUE, FALSE),
                                   labels = c("Single-country",
                                              "Multi-country")))
  return(table_targets)
}
