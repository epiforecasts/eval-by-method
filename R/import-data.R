# Functions to import and save data for predictions and observations
# Examples:
# forecasts <- get_forecasts()
# obs <- get_observed()
# forecasts <- left_join(forecasts, obs,
#                        by = c("location", "target_end_date"))
# anomalies <- get_anomalies()
# forecasts <- anti_join(forecasts, anomalies,
#                        by = c("target_end_date", "location"))

library(here)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal())

# Prediction data  ------------------------------------------------------
get_forecasts <- function() {
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

# Observed data ---------------------------------------------------------
# Get raw values
get_observed <- function() {
  obs <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") |>
    # aggregate to weekly incidence
    mutate(year = epiyear(date),
           week = epiweek(date)) |>
    group_by(location, location_name, year, week) |>
    summarise(target_end_date = max(date),
              observed = sum(value, na.rm = TRUE)) |>
    ungroup() |>
    select(-year, -week)

  # Remove <0 values
  obs <- obs |>
    group_by(location) |>
    arrange(target_end_date) |>
    mutate(observed = ifelse(observed < 0, NA, observed))

  # Add "trend" as change in 3-week moving average
  obs <- obs |>
    mutate(ma = zoo::rollmean(observed,
                              align = "right", k = 3, fill = NA),
           trend = ma / lag(ma, n=1),
           trend = as.factor(ifelse(is.nan(trend), "Stable",
                                    ifelse(trend >= 1.05, "Increasing",
                                           ifelse(trend <= 0.95, "Decreasing",
                                                  "Stable")))))
  obs <- obs |>
    select(location, target_end_date, observed, trend)
  return(obs)
}

# Plot observed data and trend classification
plot_observed <- function() {
  obs <- import_observed()
  obs |>
    ggplot(aes(x = target_end_date, y = log(observed))) +
    geom_point(col = trend) +
    geom_line(alpha = 0.3) +
    scale_x_date() +
    labs(x = NULL, y = "Log observed", col = "Trend",
         caption = "Trend (coloured points) of weekly change in 3-week moving average") +
    theme(legend.position = "bottom", ) +
    facet_wrap(facets = "location", ncol = 1,
               strip.position = "left")

  ggsave(filename = here("output/fig-trends.pdf"),
         height = 50, width = 15, limitsize = FALSE)
}

# Anomalies
get_anomalies <- function() {
  read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/5a2a8d48e018888f652e981c95de0bf05a838135/data-truth/anomalies/anomalies.csv") |>
    filter(target_variable == "inc death") |>
    select(-target_variable) |>
    mutate(anomaly = TRUE)
}

# Plot anomalies
plot_anomalies <- function() {
  obs <- get_observed()
  anomalies <- get_anomalies()

  obs <- left_join(obs, anomalies) |>
    group_by(location) |>
    mutate(anomaly = replace_na(anomaly, FALSE))

  obs |>
    ggplot(aes(x = target_end_date,
               y = log(observed),
               col = anomaly)) +
    geom_line() +
    geom_point(size = 0.3) +
    scale_x_date() +
    labs(x = NULL, y = "Log observed") +
    theme(legend.position = "bottom", ) +
    facet_wrap(facets = "location", ncol = 1,
               strip.position = "left")

  ggsave(filename = here("output/fig-anomalies.pdf"),
         height = 50, width = 15, limitsize = FALSE)
}
