# Prepare scores data with explanatory variables
prep_data <- function(scoring_scale = "log") {
  # Get raw interval score
  scores_raw <- read_csv(here("data", "scores-raw.csv")) |>
    filter(scale == scoring_scale)

  # Extra explanatory vars ------
  # Target type
  targets <- scores_raw |>
    select(model, forecast_date, location) |>
    distinct() |>
    group_by(model, forecast_date) |>
    summarise(target_count = n()) |>
    ungroup() |>
    group_by(model) |>
    summarise(CountryTargets = all(target_count <= 2)) |>
    mutate(CountryTargets = factor(CountryTargets,
                                levels = c(TRUE, FALSE),
                                labels = c("Single-country",
                                           "Multi-country")))

  # Method type
  methods <- read_csv(here("data", "model-classification.csv")) |>
    mutate(Method = factor(classification,
                           levels = c("Mechanistic", "Semi-mechanistic",
                                      "Statistical", "Qualitative"))) |>
    select(model, Method)

  # Incidence level + trend (see: R/import-data.r)
  obs <- read_csv(here("data", "observed.csv")) |>
    mutate(Trend = factor(trend, levels = c("Stable", "Increasing", "Decreasing"))) |>
    rename(Incidence = observed) |>
    select(target_end_date, location, Trend, Incidence)

  data <- scores_raw |>
    left_join(obs, by = c("location", "target_end_date")) |>
    left_join(targets, by = "model") |>
    left_join(methods, by = "model") |>
    rename(Model = model, Horizon = horizon) |>
    mutate(Model = as.factor(Model),
           Horizon = ordered(Horizon,
                             levels = 1:4, labels = 1:4),
           log_interval_score = log(interval_score + 0.01))
  return(data)
}

# Match affiliated location to scored forecast location
# CountryTargetAffiliated = factor(
#   location == country_affiliation,
#   levels = c(TRUE, FALSE),
#   labels = c("True", "False")),
