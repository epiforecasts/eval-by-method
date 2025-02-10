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
  # affiliated <- table_confint(scores, "CountryTargetAffiliated") |>
  #                       mutate(Variable = ifelse(Variable == "True",
  #                               "Affiliated to target country",
  #                               "Located elsewhere"),
  #                              Models = NA)
  horizon <- table_confint(scores, "Horizon")
  trend <- table_confint(scores, "Trend")
  table1 <- bind_rows(overall, method, targets,
                      #affiliated,
                      horizon, trend)
  return(table1)
}

# Plot over time by explanatory variable ----------------------------------
plot_over_time <- function(scores, ensemble, add_plot){
  quantiles = c(0.25, 0.5, 0.75)

  plot_over_time_ensemble <- ensemble |>
    # Median & IQR
    group_by(target_end_date, Model) |>
    mutate(Model = "Hub ensemble model") |>
    reframe(
      n = n(),
      value = quantile(interval_score, quantiles),
      quantile = paste0("q", quantiles)) |>
    pivot_wider(names_from = quantile) |>
    # Plot
    ggplot(aes(x = target_end_date, col = Model, fill = Model)) +
    geom_line(aes(y = q0.5), alpha = 0.5) +
    geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
                alpha = 0.2, col = NA) +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_manual(values = c("Hub ensemble model" = "#1f78b4"),
                      aesthetics = c("col", "fill"),
                      ) +
    labs(x = NULL, y = "Interval score",
         fill = NULL, col = NULL) +
    theme(legend.position = "bottom")

  plot_over_time_target <- scores |>
    filter(!grepl("Qualitative", Method)) |>
    # Get median & IQR
    group_by(target_end_date, CountryTargets) |>
    reframe(
      n = n(),
      value = quantile(interval_score, quantiles),
      quantile = paste0("q", quantiles)) |>
    pivot_wider(names_from = quantile) |>
    # Plot
    ggplot(aes(x = target_end_date,
               col = CountryTargets,
               fill = CountryTargets)) +
    geom_line(aes(y = q0.5), alpha = 0.5) +
    geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
                alpha = 0.1, col = NA) +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_manual(values = c("Single-country" = "#e7298a",
                                 "Multi-country" = "#e6ab02"),
                                 aesthetics = c("col", "fill")) +
    labs(x = NULL, y = "Interval score",
         fill = NULL, col = NULL) +
    theme(legend.position = "bottom")

  plot_over_time_method <- scores |>
    filter(!grepl("Qualitative", Method)) |>
    # Get median & IQR
    group_by(target_end_date, Method) |>
    reframe(
      n = n(),
      value = quantile(interval_score, quantiles),
      quantile = paste0("q", quantiles)) |>
    pivot_wider(names_from = quantile) |>
    # Plot
    ggplot(aes(x = target_end_date, col = Method, fill = Method)) +
    geom_line(aes(y = q0.5), alpha = 0.5) +
    geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
                alpha = 0.1, col = NA) +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_brewer(aesthetics = c("col", "fill"),
                      type = "qual", palette = 2) +
    labs(x = NULL, y = "Interval score",
         fill = NULL, col = NULL) +
    theme(legend.position = "bottom")

  score_plot <- plot_over_time_ensemble +
    plot_over_time_method +
    plot_over_time_target

  if (!missing(add_plot)) {
    score_plot <- score_plot + add_plot
  }

  score_plot <- score_plot +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "A")

  return(score_plot)
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
  #affiliated <- plot_conditional_density(scores, "CountryTargetAffiliated")

  plot_density_patchwork <- method +
    targets +
    #affiliated +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "A",
                    caption = "NA represents Hub ensemble model")
  return(plot_density_patchwork)
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


# Metadata ----------------------------------------------------------------
table_metadata <- function(scores) {
  classification <- classify_models() |>
    select(Model = model, Method = classification)
  targets <- table_targets(scores) |>
    select(Model, CountryTargets)
  model_scores <- scores |>
    group_by(Model) |>
    table_confint() |>
    select(Model, Forecasts, median, `Median (IQR)`)
  metadata_table <- classification |>
    inner_join(targets |> rename("Country Targets" = CountryTargets)) |>
    left_join(model_scores) |>
    arrange(median, Method, `Country Targets`) |>
    mutate(Description = paste0("[Metadata](https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/model-metadata/", Model, ".yml)")) |>
    select(-median)
  return(metadata_table)
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

data_plot <- function(scores, log = FALSE, all = FALSE) {
  data <- scores |>
    select(location, target_end_date, Incidence) |>
    distinct()
  pop <- read_csv(paste0(
    "https://raw.githubusercontent.com/european-modelling-hubs/",
    "covid19-forecast-hub-europe/main/data-locations/locations_eu.csv"
  ), show_col_types = FALSE) |>
    select(location, population)
  data <- data |>
    left_join(pop, by = join_by(location)) |>
    mutate(
      rel_inc = Incidence / population * 1e5,
      log_inc = log(Incidence + 1)
    )
  total <- data |>
    group_by(target_end_date) |>
    summarise(
      Incidence = sum(Incidence),
      population = sum(population)
    ) |>
    mutate(
      rel_inc = Incidence / population * 1e5,
      log_inc = log(Incidence + 1),
      location = "Total"
    )
  var_name <- ifelse(log, "log_inc", "rel_inc")
  plot <- ggplot(mapping = aes(
    x = target_end_date, y = .data[[var_name]], group = location
  ))

  if (all) {
    plot <- plot + geom_line(data = data, alpha = 0.1)
  }

  plot <- plot +
    geom_line(data = total, linewidth = ifelse(all, 2, 1)) +
    xlab("")

  if (log) {
    plot <- plot + ylab("log(Deaths + 1)")
  } else {
    plot <- plot + ylab("Incidence per 100,000")
  }

  return(plot)
}

trends_plot <- function(scores) {
  trends <- scores |>
    select(location, target_end_date, Incidence, Trend) |>
    distinct()
  p <- ggplot(trends, aes(x = target_end_date, y = Incidence)) +
    geom_point(mapping = aes(colour = Trend), size = 3) +
    geom_line() +
    scale_colour_brewer(palette = "Set1", na.value = "grey") +
    theme(legend.position = "bottom") +
    facet_wrap(~ location, scales = "free_y") +
    xlab("")
  return(p)
}
