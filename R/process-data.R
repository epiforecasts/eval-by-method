library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
source(here("R", "utils-variants.R"))
source(here("R", "utils-data.R"))

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

# Weekly covariates per target series ----------------------------------
# One row per outcome, location and week, from the observed counts written by
# download_obs() (R/utils-data.R):
#   - Incidence: weekly count; NA where download_obs() removed a negative count
#   - Trend: classify_trend() of those counts
#   - Growth: average weekly change in log(count + 1) over the past
#     `growth_weeks` weeks, set per outcome. Two weeks for both: of windows of
#     1 to 4 weeks, it best predicted the growth that followed over the
#     forecast horizons across both outcomes (one week did better for cases
#     alone), see report/quarto/supplement/_covariates.qmd
#   - VariantPhase: dominant variant phase
# Missing weeks stay missing: Trend and Growth are NA wherever the weeks they
# read include one. Joined to each forecast by process_data() (#187, #201).
observed_covariates <- function(growth_weeks = c(Cases = 2, Deaths = 2)) {
  stopifnot(setequal(names(growth_weeks), c("Cases", "Deaths")))
  obs <- c(Cases = "case", Deaths = "death") |>
    map(\(x) {
      read_csv(here("data", paste0("observed-", x, ".csv")),
               show_col_types = FALSE)
    }) |>
    list_rbind(names_to = "epi_target") |>
    arrange(epi_target, location, target_end_date) |>
    group_by(epi_target, location) |>
    mutate(Trend = classify_trend(observed)) |>
    ungroup()
  # Recomputed trend matches the stored one, so the stored file is current
  stopifnot(identical(obs$Trend, as.character(obs$trend)))

  # Join on date rather than row position, so a missing week cannot shift it
  lagged <- transmute(obs, epi_target, location,
                      target_end_date = target_end_date +
                        weeks(unname(growth_weeks[epi_target])),
                      observed_lag = observed)
  obs |>
    left_join(lagged, by = c("epi_target", "location", "target_end_date")) |>
    left_join(ungroup(classify_variant_phases()),
              by = c("location", "target_end_date")) |>
    transmute(
      epi_target, location, target_end_date,
      Incidence = observed,
      Trend = factor(Trend, levels = c("Stable", "Increasing", "Decreasing")),
      Growth = (log(observed + 1) - log(observed_lag + 1)) /
        unname(growth_weeks[epi_target]),
      VariantPhase
    )
}

# Prepare data for analysis -----------------------------
# Get scores for all forecasts; and add explanatory variables in a single dframe
# covariate_date: which week the target-specific covariates (incidence, trend,
#   variant phase) describe.
#   - "target": the target week being forecast (uses the observed outcome)
#   - "forecast": a week before the forecast date, so covariates use only
#     information available to the forecaster. See issue #187 (forecaster's
#     dilemma).
# covariate_lag_weeks: with "forecast", how many weeks before the week ending
#   the day before the forecast date. That week was at best partly reported
#   when the forecast was made, so the model uses 1: the week ending 8 days
#   before the forecast date (target_end_date - (horizon + 1) weeks). 0 gives
#   the week ending the day before. Ignored with "target".
# growth_weeks: weeks over which growth is averaged, per outcome; passed to
#   observed_covariates().
# The default here is "target", which describes the target week, for the
# descriptive tables and figures; model_wis() sets its own default.
process_data <- function(scoring_scale = "log",
                         covariate_date = c("target", "forecast"),
                         covariate_lag_weeks = 1,
                         growth_weeks = c(Cases = 2, Deaths = 2)) {
  covariate_date <- match.arg(covariate_date)
  # Get raw interval scores ----------------------------------------
  # scores data created in: R/process-score.r
  scores_files <- list.files(here("data"), pattern = "scores-raw-.*\\.csv")
  names(scores_files) <- sub("scores-raw-(.*)\\..*$", "\\1", scores_files)
  scores_raw <- scores_files |>
    map(\(file) {
      read_csv(here("data", file))
    }) |>
    bind_rows(.id = "epi_target") |>
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
    select(model, Method = classification, agreement)

  # Incidence, trend, growth and variant phase per outcome, location and week
  covariates <- observed_covariates(growth_weeks)

  # Combine all data -----------------------------------------------------
  # Date at which to join target-specific covariates
  scores_raw <- scores_raw |>
    mutate(
      epi_target = paste0(str_to_title(epi_target), "s"),
      covariate_end_date = if (covariate_date == "target") {
        target_end_date
      } else {
        target_end_date - weeks(horizon + covariate_lag_weeks)
      }
    )

  data <- scores_raw |>
    left_join(covariates, by = c("epi_target", "location",
                                 "covariate_end_date" = "target_end_date")) |>
    select(-covariate_end_date) |>
    left_join(country_targets, by = "model") |>
    left_join(methods, by = "model") |>
    # set to factors
    rename(Model = model, Horizon = horizon, Location = location) |>
    mutate(
      Horizon = ifelse(!Horizon %in% 1:4, NA_integer_, Horizon),
      Model = as.factor(Model),
      Location = as.factor(Location)) |>
    # Scores of exactly 0 are kept as they are: the Tweedie family used in
    # analysis-model.R has a point mass at zero, so no constant is needed to
    # make them representable on a log link.
    filter(!is.na(Horizon)) ## horizon not in 1:4
  return(data)
}

