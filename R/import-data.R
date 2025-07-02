library(here)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
# functions to clean forecasts, and download observed incidence and population data
# example:
# walk(c("case", "death"), \(target) {download_obs(data_type = target)}

# Prediction data  ------------------------------------------------------
get_forecasts <- function(data_type = "death") {
  # Get all forecasts from saved parquet, clean, and apply inclusion criteria
  forecasts <- arrow::read_parquet(here(
    "data",
    "covid19-forecast-hub-europe.parquet"
  )) |>
    filter(grepl(data_type, target))

  forecasts <- forecasts |>
    separate(target,
             into = c("horizon", "target_variable"),
             sep = " wk ahead "
    ) |>
    # set forecast date to corresponding submission date
    mutate(
      horizon = as.numeric(horizon),
      forecast_date = target_end_date - weeks(horizon) + days(1)
    ) |>
    rename(prediction = value) |>
    select(
      location, forecast_date,
      horizon, target_end_date,
      model, quantile, prediction
    )

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
                         by = c("model", "forecast_date", "location")
  )
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

# Observed data ---------------------------------------------------------
download_obs <- function(data_type = "death") {
  file_name <- paste0("truth_JHU-Incident%20", str_to_title(data_type), "s.csv")
  obs <- read_csv(paste0(
    "https://raw.githubusercontent.com/covid19-forecast-hub-europe/",
    "covid19-forecast-hub-europe/main/data-truth/JHU/", file_name
  )) |>
    # aggregate to weekly incidence
    mutate(
      year = epiyear(date),
      week = epiweek(date)
    ) |>
    group_by(location, location_name, year, week) |>
    summarise(
      target_end_date = max(date),
      observed = sum(value, na.rm = TRUE)
    ) |>
    ungroup() |>
    select(-year, -week)

  # Remove <0 values
  obs <- obs |>
    group_by(location) |>
    arrange(target_end_date) |>
    mutate(observed = ifelse(observed < 0, NA, observed))

  # Add "trend" as change in 3-week moving average
  obs <- obs |>
    mutate(
      ma = zoo::rollmean(observed,
        align = "right", k = 3, fill = NA
      ),
      trend = ma / lag(ma, n = 1),
      trend = as.factor(ifelse(is.nan(trend), "Stable",
        ifelse(trend >= 1.05, "Increasing",
          ifelse(trend <= 0.95, "Decreasing",
            "Stable"
          )
        )
      ))
    )
  obs <- obs |>
    select(location, target_end_date, observed, trend)
  write_csv(obs, here("data", paste0("observed-", data_type, ".csv")))
}

# Population data ---------------------------------------------------------
download_pop <- function() {
  read_csv(paste0(
    "https://raw.githubusercontent.com/european-modelling-hubs/",
    "covid19-forecast-hub-europe/main/data-locations/locations_eu.csv"
  ), show_col_types = FALSE) |>
    select(location, population)
  write_csv(pop, here("data", paste0("populations.csv")))
}
