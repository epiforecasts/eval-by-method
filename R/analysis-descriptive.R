# Aim: describe interval score in terms of model structure and country target type
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

create_raw_table1 <- function(scores) {
  overall <- table_confint(scores, "scale") |>
    mutate(Variable = "Overall", group = "")
  method <- table_confint(scores, "Method")
  targets <- table_confint(scores, "CountryTargets")
  bind_rows(overall, method, targets)
}

print_table1 <- function(scores) {
  # Cohort characteristics — no outcome data
  # Composition (single-country %) computed once across both epi targets
  composition <- scores |>
    select(Model, Method, CountryTargets) |>
    distinct() |>
    group_by(Method) |>
    summarise(
      n_single = sum(CountryTargets == "Single-country"),
      n_models = n(),
      .groups = "drop"
    ) |>
    mutate(
      `Single-country` = paste0(
        n_single, "/", n_models,
        " (", round(n_single / n_models * 100), "%)"
      ),
      Variable = Method,
      group = "Method"
    ) |>
    select(Variable, group, `Single-country`)

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

    ## add composition column for Method rows
    table1 <- table1 |>
      left_join(composition, by = c("Variable", "group")) |>
      relocate(`Single-country`, .after = last_col())

    ## reorder epi target columns
  for (outcome in rev(epi_targets)) {
    table1 <- table1 |>
      relocate(ends_with(outcome), .after = Variable)
  }

  ## build spanning headers
  headers_to_add <- c(" " = 1, vapply(
    epi_targets, \(x) sum(grepl(paste0("_", x, "$"), colnames(table1))),
    1L
  ), " " = 1)

  table1 |>
    select(-group) |>
    rename(" " = Variable) |>
    kable(
      caption = paste0(
        "Characteristics of models and forecasts sampled from ",
        "the European COVID-19 Forecast Hub, March 2021-2023. ",
        "Models (%) shows number of models and percentage of all included models. ",
        "Single-country shows models targeting one country as a fraction and percentage ",
        "of models in each method group."
      ),
      col.names = c(" ", rep(c("Models (%)"), length(epi_targets)), "Single-country (%)"),
      align = c("l", rep("r", length(epi_targets)), "r")
    ) |>
    pack_rows(index = c(
      " " = 1,
      "Method" = 5,
      "Geographic scope" <- 2
    )) |>
    add_header_above(headers_to_add)
}

# Table 2: unadjusted vs adjusted effects --------------------
print_table2 <- function(effects, show_ratio = TRUE) {
  effects |>
    filter(group_var %in% c("Method")) |>
    mutate(
      # Exponentiate point estimate and both CI bounds: multiplicative ratio
      # relative to the grand-mean WIS
      ratio = paste0(
        round(exp(value), 2),
        " (", round(exp(lower_2.5), 2), ", ", round(exp(upper_97.5), 2), ")"
      )
    ) |>
    select(group_var, group, model, ratio) |>
    pivot_wider(
      names_from = model,
      values_from = ratio
    ) |>
    arrange(group_var, group) |>
    mutate(
      group_var = factor(group_var)
    ) |>
    select(
      group_var, group,
      Unadjusted, Adjusted
    ) |>
    rename(
      "Variable" = group_var,
      "Group" = group,
      "Unadjusted ratio (95% CI)" = Unadjusted,
      "Adjusted ratio (95% CI)" = Adjusted
    ) |>
    kable(
      caption = paste0(
        "Partial effects of model structure on the performance of COVID-19 forecasts ",
        "(weighted interval score), ",
        "from univariate (unadjusted) and a joint (adjusted) generalised additive mixed model. ",
        "Effects represent deviations from the grand mean under a sum-to-zero constraint, ",
        "expressed as the exponentiated partial effect: a multiplicative ratio relative to the grand-mean WIS. ",
        "A ratio below 1 indicates better-than-average performance; 1 indicates the grand-mean WIS. ",
        "Raw partial effects on the log scale are reported in the Supplement. ",
        "95% CI = 95% confidence interval."
      ),
      align = c("l", "l", "r", "r")
    ) |>
    collapse_rows(columns = 1, valign = "top") |>
      kable_styling(full_width = FALSE)
}
