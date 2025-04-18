# Aim: describe interval score in terms of model structure and country target type
# Load data:
# source(here("R", "prep-data.R"))
# scores <- prep_data(scoring_scale = "log")
library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(patchwork)
library(janitor)
library(kableExtra)
library(stringr)
library(boot)

# Bootstrap CIs around the mean WIS
calc_ci <- function(x, R, ...) {
  mymean <- function(x, i, na.rm = FALSE) {
    return(mean(x[i], na.rm = na.rm))
  }

  bootstraps <- boot(x, mymean, R = R, parallel = "multicore", ...)
  ci <- boot.ci(bootstraps, type = "perc")
  list(data.frame(lboot = ci$perc[4], uboot = ci$perc[5]))
}

# Table summary --------------------
table_confint <- function(scores, group_var = NULL) {
  total_forecasts <- nrow(scores)
  total_models <- n_distinct(scores$Model)
  if (!is.null(group_var)) {
    scores <- scores |>
      group_by(.data[[group_var]])
  }

  table <- scores |>
    summarise(
      n_forecasts = format(n(), big.mark = ","),
      p_forecasts = round(n() / total_forecasts * 100, 1),
      n_models = n_distinct(Model),
      p_models = round(n_models / total_models * 100, 1),
      mean = mean(wis, na.rm = TRUE),
      sd = sd(wis, na.rm = TRUE)
    ) |>
    mutate(
      Models = paste0(n_models, " (", p_models, "%)"),
      Forecasts = paste0(n_forecasts, " (", p_forecasts, "%)"),
      "Mean WIS (SD)" = paste0(round(mean, 2), " (", round(sd, 2), ")")
    )

  if (!is.null(group_var)) {
    table <- table |>
      rename("Variable" = all_of(group_var)) |>
      mutate(group = group_var)
  }
  return(table)
}

create_raw_table1 <- function(scores, targets) {
  overall <- table_confint(scores, "scale") |>
    mutate(Variable = "Overall", group = "")
  method <- table_confint(scores, "Method")
  targets <- table_confint(scores, "CountryTargets")
  bind_rows(
    overall, method, targets
  )
}

print_table1 <- function(scores) {
  outcome_targets <- unique(scores$outcome_target)
  tables <- outcome_targets |>
    map(\(outcome) {
      scores <- scores |>
        filter(outcome_target == outcome)
      table <- create_raw_table1(scores)

      colnames(table)[!(colnames(table) %in% c("Variable", "group"))] <-
        paste(
          colnames(table)[!(colnames(table) %in% c("Variable", "group"))],
          outcome,
          sep = "_"
        )
      return(table)
    })

  ## merge all
  table1 <- tables[[1]]
  if (length(outcome_targets) > 1) {
    for (i in seq(2, length(outcome_targets))) {
      table1 <- inner_join(table1, tables[[i]], by = c("Variable", "group"))
    }
  }

  ## select columns
  table1 <- table1 |>
    select(
      Variable,
      starts_with("Models_"),
      starts_with("Forecasts_"),
      starts_with("Mean WIS (SD)_")
    )
  ## reorder
  for (outcome in rev(outcome_targets)) {
    table1 <- table1 |>
      relocate(ends_with(outcome), .after = Variable)
  }

  ## build extra headers
  headers_to_add <- c(" " = 1, vapply(
    outcome_targets, \(x) sum(grepl(paste0("_", x, "$"), colnames(table1))),
    1L
  ))

  table1 |>
    rename(" " = Variable) |>
    kable(
      caption = paste0(
        "Characteristics of forecasts sampled from ",
        "the European COVID-19 Forecast Hub, March 2021-2023. ",
        "Forecast performance was measured using the weighted ",
        "interval score (WIS), with a lower score indicating a more ",
        "accurate forecast."
      ),
      col.names = str_remove(colnames(table1), "_.*$"),
      align = c("l", rep("r", ncol(table1) - 1))
    ) |>
    pack_rows(index = c(
      " " = 1,
      "Method" = 5,
      "Number of country targets" = 2
    )) |>
    add_header_above(headers_to_add)
}

# Plot over time by explanatory variable ----------------------------------
plot_over_time <- function(scores, ensemble, add_plot, show_uncertainty = TRUE) {
  plot_over_time_target <- scores |>
    # Get mean & CIs
    group_by(target_end_date, outcome_target, CountryTargets) |>
    reframe(
      n = n(),
      mean = mean(wis, na.rm = TRUE),
      ci = calc_ci(wis, na.rm = TRUE, R = 1000)
    ) |>
    unnest(ci) |>
    # Plot
    ggplot(aes(
      x = target_end_date,
      col = CountryTargets,
      fill = CountryTargets
    )) +
    geom_line(aes(y = mean), alpha = 0.5)
  if (show_uncertainty) {
    plot_over_time_target <- plot_over_time_target +
      geom_ribbon(aes(ymin = lboot, ymax = uboot),
        alpha = 0.1, col = NA
      )
  }
  plot_over_time_target <- plot_over_time_target +
    facet_wrap(~outcome_target, scales = "free_y") +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_manual(
      values = c(
        "Single-country" = "#e7298a",
        "Multi-country" = "#e6ab02"
      ),
      aesthetics = c("col", "fill")
    ) +
    labs(
      x = NULL, y = "Mean WIS (log scale)",
      fill = NULL, col = NULL
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank()
    )

  plot_over_time_method <- scores |>
    # Get mean & CIs
    group_by(target_end_date, outcome_target, Method) |>
    reframe(
      n = n(),
      mean = mean(wis, na.rm = TRUE),
      ci = calc_ci(wis, na.rm = TRUE, R = 1000)
    ) |>
    unnest(ci) |>
    # Plot
    ggplot(aes(x = target_end_date, col = Method, fill = Method)) +
    geom_line(aes(y = mean), alpha = 0.5)
  if (show_uncertainty) {
    plot_over_time_method <- plot_over_time_method +
      geom_ribbon(aes(ymin = lboot, ymax = uboot),
        alpha = 0.1, col = NA
      )
  }
  plot_over_time_method <- plot_over_time_method +
    facet_wrap(~outcome_target, scales = "free_y") +
    scale_x_date(date_labels = "%b %Y") +
    scale_fill_brewer(
      aesthetics = c("col", "fill"),
      type = "qual", palette = 2
    ) +
    labs(
      x = NULL, y = "Mean WIS (log scale)",
      fill = NULL, col = NULL
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank()
    )

  score_plot <- plot_over_time_method +
    plot_over_time_target

  if (!missing(add_plot)) {
    score_plot <- score_plot + add_plot
  }

  score_plot <- score_plot +
    plot_layout(ncol = 1) +
    plot_annotation(tag_levels = "A")

  return(score_plot)
}

# Ridge plot by model --------------------
plot_ridges <- function(scores, target = "Deaths") {
  scores |>
    filter(outcome_target == target) |>
    group_by(Model) |>
    mutate(
      median_score = median(wis, na.rm = TRUE),
      lq = quantile(wis, 0.25, na.rm = TRUE),
      uq = quantile(wis, 0.75, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(Model = fct_reorder(Model, median_score)) |>
    filter(wis >= lq & wis <= uq) |>
    # Plot
    ggplot(aes(x = wis, y = Model, fill = stat(x))) +
    geom_density_ridges_gradient(
      scale = 1.5,
      rel_min_height = 0.01,
      quantile_lines = TRUE, quantiles = 2
    ) +
    scale_fill_viridis_c(
      name = "Interval score",
      option = "C", direction = -1
    ) +
    theme_ridges() +
    labs(x = "WIS (IQR)", y = "Model") +
    theme(legend.position = "none")
}

# Table of targets by model -------------
table_targets <- function(scores) {
  table_targets <- scores |>
    select(Model, outcome_target, forecast_date, location) |>
    distinct() |>
    group_by(Model, outcome_target, forecast_date) |>
    summarise(target_count = n(), .groups = "drop") |>
    ungroup() |>
    group_by(Model, outcome_target) |>
    summarise(
      CountryTargets = all(target_count <= 2),
      min_targets = min(target_count),
      max_targets = max(target_count),
      mean = mean(target_count),
      median = median(target_count),
      consistent = min_targets == max_targets
    ) |>
    mutate(CountryTargets = factor(CountryTargets,
      levels = c(TRUE, FALSE),
      labels = c(
        "Single-country",
        "Multi-country"
      )
    ))
  return(table_targets)
}

# Metadata ----------------------------------------------------------------
table_metadata <- function(scores) {
  classification <- classify_models() |>
    select(Model = model, Method = classification)
  model_scores <- scores |>
    group_by(Model, outcome_target) |>
    table_confint() |>
    select(Model, outcome_target, Forecasts)
  country_targets <- table_targets(scores) |>
    select(Model, outcome_target, CountryTargets)
  metadata_table <- classification |>
    left_join(model_scores) |>
    mutate(Description = paste0("[Metadata](https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/model-metadata/", Model, ".yml)")) |>
    inner_join(country_targets) |>
    mutate(
      outcome_target = sub("s$", " forecasts", outcome_target)
    ) |>
    pivot_wider(
      names_from = "outcome_target",
      values_from = "Forecasts",
      values_fill = ""
    ) |>
    rename("Country Targets" = CountryTargets) |>
    arrange(Model)
  return(metadata_table)
}

# Data --------------------
data_plot <- function(scores, log = FALSE, all = FALSE) {
  data <- scores |>
    select(location, outcome_target, target_end_date, Incidence) |>
    distinct()
  pop <- read_csv(here("data", "populations.csv"), show_col_types = FALSE)
  data <- data |>
    left_join(pop, by = join_by(location)) |>
    mutate(
      rel_inc = Incidence / population * 1e5,
      log_inc = log(Incidence + 1)
    )
  total <- data |>
    group_by(outcome_target, target_end_date) |>
    summarise(
      Incidence = sum(Incidence),
      population = sum(population),
      .groups = "drop"
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
    facet_wrap(~outcome_target, scales = "free") +
    xlab("")

  if (log) {
    plot <- plot + ylab(paste0("log(Incidence + 1)"))
  } else {
    plot <- plot + ylab("Incidence per 100,000")
  }
  plot <- plot +
    theme(strip.background = element_blank())

  return(plot)
}

trends_plot <- function(scores) {
  trends <- scores |>
    select(location, target_end_date, Incidence, Trend) |>
    distinct()
  p <- ggplot(trends, aes(x = target_end_date, y = Incidence)) +
    geom_point(mapping = aes(colour = Trend), size = 1) +
    geom_line() +
    scale_colour_brewer(palette = "Set2", na.value = "grey") +
    theme(legend.position = "bottom") +
    facet_wrap(~location, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab("")
  return(p)
}
