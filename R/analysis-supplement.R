# Scope: supplementary descriptive tables
# Load data:
# source(here("R", "process-data.R"))
# scores <- process_data(scoring_scale = "log")
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

# Detailed participation table ----
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
      p_models = round(n_models / total_models * 100, 1)
    ) |>
    mutate(
      Models = paste0(n_models, " (", p_models, "%)")
    )

  if (!is.null(group_var)) {
    table <- table |>
      rename("Variable" = all_of(group_var)) |>
      mutate(group = group_var)
  }
  return(table)
}

create_raw_table1 <- function(scores) {
  overall <- table_confint(scores, "scale") |>
    mutate(Variable = "Overall", group = "")
  method <- table_confint(scores, "Method")
  bind_rows(overall, method)
}

print_table1 <- function(scores, caption = NULL) {
  # Totals
  n_available_targets <- 104 * 2 * 4 * 32
  total_models <- n_distinct(scores$Model)

  # Participation: median, across models in each group
  # of each model's share of available forecast targets
  participation_by_model <- scores |>
    group_by(Model, Method) |>
    summarise(n_forecasts = n(), .groups = "drop") |>
    mutate(p_participation = n_forecasts / n_available_targets * 100)

  participation <- bind_rows(
    participation_by_model |>
      summarise(Participation = paste0(round(median(p_participation)), "%")) |>
      mutate(Variable = "Overall", group = ""),
    participation_by_model |>
      group_by(Method) |>
      summarise(Participation = paste0(round(median(p_participation)), "%"), .groups = "drop") |>
      rename(Variable = Method) |>
      mutate(group = "Method")
  ) |>
    select(Variable, group, Participation)

  # Overall Models column: distinct models per group across BOTH epi targets,
  # as a percentage of all distinct models in `scores`
  models_by_method <- scores |>
    select(Model, Method) |>
    distinct() |>
    count(Method, name = "n_models") |>
    mutate(
      p_models = round(n_models / total_models * 100, 1),
      Models_Overall = paste0(n_models, " (", p_models, "%)"),
      Variable = Method,
      group = "Method"
    )

  overall_models <- bind_rows(
    scores |>
      summarise(n_models = n_distinct(Model)) |>
      mutate(
        p_models = round(n_models / total_models * 100, 1),
        Models_Overall = paste0(n_models, " (", p_models, "%)"),
        Variable = "Overall",
        group = ""
      ),
    models_by_method
  ) |>
    select(Variable, group, Models_Overall)

  epi_targets <- unique(scores$epi_target)
  tables <- epi_targets |>
    map(\(outcome) {
      scores <- scores |>
        filter(epi_target == outcome)
      table <- create_raw_table1(scores)

      colnames(table)[!(colnames(table) %in% c("Variable", "group"))] <-
        paste(
          colnames(table)[!(colnames(table) %in% c("Variable", "group"))],
          outcome,
          sep = "_"
        )
      return(table)
    })

  ## merge all epi targets
  table1 <- tables[[1]]
  if (length(epi_targets) > 1) {
    for (i in seq(2, length(epi_targets))) {
      table1 <- inner_join(table1, tables[[i]], by = c("Variable", "group"))
    }
  }

  ## select columns — no WIS
  table1 <- table1 |>
    select(Variable, group, starts_with("Models_"))

  ## reorder epi target columns
  for (outcome in rev(epi_targets)) {
    table1 <- table1 |>
      relocate(ends_with(outcome), .after = Variable)
  }

  ## add the combined-outcome Models column and the Participation column,
  ## both computed once across both epi targets
  table1 <- table1 |>
    left_join(overall_models, by = c("Variable", "group")) |>
    left_join(participation, by = c("Variable", "group"))

  ## build spanning headers — only the per-outcome Models columns sit under
  ## the outcome span; the combined Models column is labelled in its own right
  headers_to_add <- c(" " = 1, vapply(
    epi_targets, \(x) sum(grepl(paste0("_", x, "$"), colnames(table1))),
    1L
  ), "Both outcomes" = 1, " " = 1)

  table1 |>
    select(-group) |>
    rename(" " = Variable) |>
    kable(
      col.names = c(" ", rep("Models (%)", length(epi_targets) + 1), "Participation (%)"),
      align = c("l", rep("r", length(epi_targets) + 1), "r")
    ) |>
    pack_rows(index = c(
      " " = 1,
      "Method" = 5
    )) |>
    add_header_above(headers_to_add)
}

# Table of targets by model -------------
table_targets <- function(scores) {
  table_targets <- scores |>
    select(Model, epi_target, forecast_date, Location) |>
    distinct() |>
    group_by(Model, epi_target, forecast_date) |>
    summarise(target_count = n(), .groups = "drop") |>
    ungroup() |>
    group_by(Model, epi_target) |>
    summarise(
      CountryTargets = all(target_count == 1),
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

# Composition of CountryTargets within each Method group
table_composition <- function(scores) {
  total_models <- n_distinct(scores$Model)
  scores |>
    select(Model, Method, CountryTargets) |>
    distinct() |>
    group_by(Method) |>
    summarise(
      n_single = sum(CountryTargets == "Single-country"),
      n_models = n(),
      .groups = "drop"
    ) |>
    mutate(
      Variable = paste0(n_single, "/", n_models, " (", round(n_single / n_models * 100), "%)"),
      group = "Method"
    ) |>
    rename("Method_var" = Method) |>
    select(Variable, group, Method_var)
}

# Components of the LWIS ----------
plot_wis_components <- function(scores) {
  wis_comp <- scores |>
    mutate(incidence_pk = Incidence / pop * 100000) |>
    select(target_end_date,
           epi_target, Horizon,
           incidence_pk,
           wis, dispersion, underprediction, overprediction) |>
    pivot_longer(cols = c(dispersion, underprediction, overprediction),
                 names_to = "component", values_to = "value") |>
    # per-target percentage of total WIS, so that the three components sum to 100% per target
    mutate(value_percent = value / wis * 100) |>
    group_by(target_end_date, epi_target, Horizon, incidence_pk, component) |>
    summarise(
      n = n(),
      value_percent = sum(value_percent, na.rm = TRUE) / n,
      .groups = "drop"
    )

  wis_comp |>
    filter(component == "dispersion" & Horizon == 1) |>
    ggplot(aes(x = target_end_date, y = incidence_pk)) +
    geom_tile() +
    scale_x_log10(labels = \(x) format(x, big.mark = ",", trim = TRUE,
                                       scientific = FALSE, drop0trailing = TRUE)) +
    scale_y_log10(labels = \(x) format(x, big.mark = ",", trim = TRUE,
                                       scientific = FALSE, drop0trailing = TRUE)) +
    facet_wrap(~epi_target, scales = "free")


  wis_comp |>
    ggplot(aes(x = incidence_pk, y = value_percent, colour = component)) +
    geom_point() +
    scale_x_log10(labels = \(x) format(x, big.mark = ",", trim = TRUE,
                                       scientific = FALSE, drop0trailing = TRUE)) +
    scale_y_continuous(labels = \(x) paste0(x, "%")) +
    facet_grid(cols = vars(epi_target), rows = vars(Horizon), scales = "free") +
    labs(
      x = "X",
      y = "Score component",
      colour = NULL
    ) +
    theme(legend.position = "bottom")
}
