library(here)
library(dplyr)
library(purrr)
library(scoringutils)
source(here("R", "import-data.R"))

walk(c("case", "death"), \(target) {

  # Get forecasts & observations -----
  # Get forecasts (note this is slow)
  forecasts_raw <- get_forecasts(data_type = target)

  # Observed data
  obs <- get_observed(data_type = target)
  write_csv(obs, here("data", paste0("observed-", target, ".csv")))
  forecasts <- left_join(
    forecasts_raw, obs,
    by = c("location", "target_end_date")
  )

  # Score forecasts on natural and log scales -----
  log_forecasts <- forecasts |>
    mutate(
      scale = "log",
      observed = log(observed + 1),
      prediction = log(pmax(prediction, 0) + 1)
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

  write_csv(scores, here("data", paste0("scores-raw-", target, ".csv")))
})

pop <- get_pop()
write_csv(pop, here("data", paste0("populations.csv")))
