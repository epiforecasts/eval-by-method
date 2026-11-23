library(here)
library(dplyr)
library(purrr)
library(scoringutils)
source(here("R", "utils-data.R"))
source(here("R", "process-data.R"))

walk(c("case", "death"), \(target) {
  # Get forecasts & observations -----
  forecasts_raw <- get_forecasts(data_type = target)

  # Observed data
  # Keep only what scoring needs: score() drops rows with an NA in any column
  # (scoringutils na.omit), so extra columns such as trend would silently drop
  # forecasts that have an observed value
  obs <- read_csv(here("data", paste0("observed-", target, ".csv"))) |>
    select(location, target_end_date, observed)
  pop <- read_csv(here("data", "populations.csv")) |>
    rename(pop = population)
  obs <- left_join(obs, pop, by = "location")
  forecasts <- left_join(
    forecasts_raw, obs,
    by = c("location", "target_end_date")
  )

  # Population normalisation
  forecasts <- forecasts |>
    mutate(
      observed = observed / pop * 100000,
      prediction = prediction / pop * 100000
    )

  # Score forecasts on natural and log scales -----
  # +1 offset maps zeros to log(1) = 0. Values are non-negative: observed <0
  # is set to NA upstream (utils-data.R), and predictions have no negatives.
  log_forecasts <- forecasts |>
    mutate(
      scale = "log",
      observed = log(observed + 1),
      prediction = log(prediction + 1)
    )

  scores <- forecasts |>
    mutate(scale = "natural") |>
    # add version for the log transformations
    rbind(log_forecasts) |>
    mutate(quantile = round(quantile, 3)) |>
    as_forecast_quantile(
      predicted = "prediction", quantile_level = "quantile"
    ) |>
    score()

  # Every forecast with an observed value is scored, on both scales
  n_scorable <- forecasts |>
    filter(!is.na(observed)) |>
    distinct(model, location, forecast_date, horizon, target_end_date) |>
    nrow()
  stopifnot(nrow(scores) == 2 * n_scorable)

  write_csv(scores, here("data", paste0("scores-raw-", target, ".csv")))
})
