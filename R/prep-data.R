library("dplyr")
library("tidyr")

# Read in metadata ans classify
classify_models <- function(file = here("data", "model-classification.csv")) {
  methods <- read_csv(file) |>
    pivot_longer(
      -model, names_to = "classifier", values_to = "classification"
    ) |>
    group_by(model) |>
    summarise(
      classification = names(
        sort(table(classification), decreasing = TRUE)[1]
      ), .groups = "drop"
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
  scores_files <- list.files(here("data"), pattern = "scores-raw-.*\\.csv")
  names(scores_files) <- sub("scores-raw-(.*)\\..*$", "\\1", scores_files)
  # Get raw interval score
  scores_raw <- scores_files |>
    map(\(file) {
      read_csv(here("data", file))
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
    select(model, Method = classification)

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

# Match affiliated location to scored forecast location
# CountryTargetAffiliated = factor(
#   location == country_affiliation,
#   levels = c(TRUE, FALSE),
#   labels = c("True", "False")),
