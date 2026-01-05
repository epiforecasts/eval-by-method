library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
source(here("R", "utils-variants.R"))

# Metadata ----------------------------------------------------------------
# Get classification of model types
classify_models <- function(file = here("data", "model-classification.csv"),
                            return_majority = TRUE) {
  methods <- read_csv(file) |>
    pivot_longer(
      -model,
      names_to = "classifier", values_to = "classification"
    ) |>
    filter(!(is.na(classification) | classification == "#N/A")) |>
    group_by(model) |>
    mutate(raters = n()) |>
    group_by(model, classification) |>
    mutate(votes = n(),
           agreement = votes == raters)

  if (return_majority) {
    methods <- methods |>
      group_by(model) |>
      slice_max(order_by = votes, with_ties = FALSE)
  }

  methods <- methods |>
    mutate(classification = ifelse(grepl("EpiExpert", model), "Judgement",
                                   classification),
           classification = factor(classification)) |>
    ungroup()

  return(methods)
}

# Prepare data for analysis -----------------------------
# Get scores for all forecasts; and add explanatory variables in a single dframe
process_data <- function(scoring_scale = "log") {
  # Get raw interval scores ----------------------------------------
  # scores data created in: R/process-score.r
  scores_files <- list.files(here("data"), pattern = "scores-raw-.*\\.csv")
  names(scores_files) <- sub("scores-raw-(.*)\\..*$", "\\1", scores_files)
  scores_raw <- scores_files |>
    map(\(file) {
      read_csv(here("data", file))
    }) |>
    bind_rows(.id = "outcome_target") |>
    filter(scale == scoring_scale)

  # Add variables of interest to scores dataframe ----------------------
  # Target type
  country_targets <- scores_raw |>
    select(model, forecast_date, location) |>
    distinct() |>
    group_by(model, forecast_date) |>
    summarise(target_count = n(), .groups = "drop") |>
    group_by(model) |>
    summarise(CountryTargets = all(target_count == 1), .groups = "drop") |>
    mutate(CountryTargets = factor(CountryTargets,
      levels = c(TRUE, FALSE),
      labels = c(
        "Single-country",
        "Multi-country"
      )
    ))

  # Method type
  methods <- classify_models() |>
    select(model, Method = classification)

  # Incidence level + trend (observed data from: R/utils-data.r)
  obs <- names(scores_files) |>
    set_names() |>
    map(~ read_csv(here("data", paste0("observed-", .x, ".csv")))) |>
    bind_rows(.id = "outcome_target") |>
    mutate(Trend = factor(trend,
                          levels = c("Stable", "Increasing", "Decreasing"))) |>
    rename(Incidence = observed) |>
    select(target_end_date, location, outcome_target, Trend, Incidence)

  # Variant phase
  variant_phase <- classify_variant_phases()

  # Combine all data -----------------------------------------------------
  data <- scores_raw |>
    left_join(obs, by = c("location", "target_end_date", "outcome_target")) |>
    left_join(variant_phase, by = c("location", "target_end_date")) |>
    left_join(country_targets, by = "model") |>
    left_join(methods, by = "model") |>
    # set to factors
    rename(Model = model, Horizon = horizon, Location = location) |>
    mutate(
      Horizon = ifelse(!Horizon %in% 1:4, NA_integer_, Horizon),
      Model = as.factor(Model),
      Location = as.factor(Location),
      outcome_target = paste0(str_to_title(outcome_target), "s"),
      wis = wis + 1e-7) |>
    filter(!is.na(Horizon)) ## horizon not in 1:4
  return(data)
}

