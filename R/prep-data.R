library("dplyr")
library("tidyr")
library("purrr")
library("readr")

# Read in metadata ans classify
classify_models <- function(file = here("data", "model-classification.csv")) {
  methods <- read_csv(file) |>
    pivot_longer(
      -model, names_to = "classifier", values_to = "classification"
    ) |>
    filter(!(is.na(classification) | classification == "#N/A")) |>
    group_by(model) |>
    summarise(
      agreement = (n_distinct(classification) == 1),
      classification = names(
        sort(table(classification), decreasing = TRUE)[1]
      ),
      .groups = "drop"
    ) |>
    mutate(classification = factor(
      classification,
      levels = c(
        "Agent-based", "Mechanistic",
        "Semi-mechanistic", "Statistical",
        "Machine learning", "Qualitative"
      )
    ))
  return(methods)
}

# Prepare scores data with explanatory variables
prep_data <- function(scoring_scale = "log") {
  scores_files <- list.files(here("output"), pattern = "scores-raw-.*\\.csv")
  names(scores_files) <- sub("scores-raw-(.*)\\..*$", "\\1", scores_files)
  # Get raw interval score
  scores_raw <- scores_files |>
    map(\(file) {
      read_csv(here("output", file))
    }) |>
    bind_rows(.id = "outcome_target") |>
    filter(scale == scoring_scale)

  # Extra explanatory vars ------
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
                                labels = c("Single-country",
                                           "Multi-country")))

  # Method type
  methods <- classify_models() |>
    select(model, Method = classification, agreement)

  # Incidence level + trend (see: R/import-data.r)
  obs <- names(scores_files) |>
    set_names() |>
    map(~ read_csv(here("data", paste0("observed-", .x, ".csv")))) |>
    bind_rows(.id = "outcome_target") |>
    mutate(Trend = factor(trend, levels = c("Stable", "Increasing", "Decreasing"))) |>
    rename(Incidence = observed) |>
    select(target_end_date, location, outcome_target, Trend, Incidence)

  data <- scores_raw |>
    left_join(obs, by = c("location", "target_end_date", "outcome_target")) |>
    left_join(country_targets, by = "model") |>
    left_join(methods, by = "model") |>
    rename(Model = model, Horizon = horizon) |>
    mutate(Model = as.factor(Model),
           outcome_target = paste0(str_to_title(outcome_target), "s"),
           Horizon = ordered(Horizon,
                             levels = 1:4, labels = 1:4),
           log_wis = log(wis + 0.01)) |>
    filter(!is.na(Horizon)) ## horizon not in 1:4
  return(data)
}

# Prediction data  ------------------------------------------------------
get_forecasts <- function(data_type = "death") {
  forecasts <- arrow::read_parquet(here("data",
                                        "covid19-forecast-hub-europe.parquet")) |>
    filter(grepl(data_type, target))

  forecasts <- forecasts |>
    separate(target, into = c("horizon", "target_variable"),
             sep = " wk ahead ") |>
    # set forecast date to corresponding submission date
    mutate(
      horizon = as.numeric(horizon),
      forecast_date = target_end_date - weeks(horizon) + days(1)) |>
    rename(prediction = value) |>
    select(location, forecast_date,
           horizon, target_end_date,
           model, quantile, prediction)

  # Exclusions
  # dates should be between start of hub and until end of JHU data
  forecasts <- forecasts |>
    filter(forecast_date >= as.Date("2021-03-07") &
             target_end_date <= as.Date("2023-03-10"))
  # only keep forecasts up to 4 weeks ahead
  forecasts <- filter(forecasts, horizon <= 4)

  # only include predictions from models with all quantiles
  rm_quantiles <- forecasts |>
    group_by(model, forecast_date, location) |>
    summarise(q = length(unique(quantile))) |>
    filter(q < 23)
  forecasts <- anti_join(forecasts, rm_quantiles,
                    by = c("model", "forecast_date", "location"))
  forecasts <- filter(forecasts, !is.na(quantile)) # remove "median"

  # remove duplicates
  forecasts <- forecasts |>
    group_by_all() |>
    mutate(duplicate = row_number()) |>
    ungroup() |>
    filter(duplicate == 1) |>
    select(-duplicate)

  return(forecasts)
}
