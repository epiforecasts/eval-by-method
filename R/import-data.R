library(here)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(tidyr)

# Import and save data
# obs <- import_obs()
# write_csv(obs, here("data", "observed.csv"))
# forecasts <- import_forecasts()
# arrow::write_parquet(data, here("data", "forecasts.parquet"))

# Prediction data  ------------------------------------------------------
import_forecasts <- function() {
  forecasts <- arrow::read_parquet(here("data",
                                        "covid19-forecast-hub-europe.parquet")) |>
    filter(grepl("death", target))

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

# Observed data ---------------------------------
import_obs <- function() {
  obs <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") |>
    # aggregate to weekly incidence
    mutate(year = epiyear(date),
           week = epiweek(date)) |>
    group_by(location, location_name, year, week) |>
    summarise(target_end_date = max(date),
              true_value = sum(value, na.rm = TRUE)) |>
    ungroup() |>
    select(-year, -week)
  return(obs)
}

# Join forecasts to observations, remove anomalies
join_obs <- function(forecasts, remove_anomalies = TRUE) {
  obs <- import_obs()
  forecasts <- left_join(forecasts, obs,
                    by = c("location", "target_end_date"))
  if (remove_anomalies) {
    # remove anomalies (detected as of March 4th 2023, pre-data change)
    anomalies <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/5a2a8d48e018888f652e981c95de0bf05a838135/data-truth/anomalies/anomalies.csv") |>
      filter(target_variable == "inc death")
    forecasts <- anti_join(forecasts, anomalies,
                      by = c("target_end_date", "location"))
    # remove any true values <0
    forecasts <- filter(forecasts, true_value >= 0)
    }
return(forecasts)
}
