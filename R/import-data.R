library(here)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(tidyr)

# Observed data
obs <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") |>
  # aggregate to weekly incidence
  mutate(target_variable = "inc death",
         year = epiyear(date),
         week = epiweek(date)) |>
  group_by(location, location_name, target_variable,
           year, week) |>
  summarise(target_end_date = max(date),
            true_value = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  select(-year, -week)
write_csv(obs, here("data", "observed.csv"))

# Prediction data
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
         target_variable, horizon, target_end_date,
         model, quantile, prediction)

data <- left_join(forecasts, obs,
                  by = c("location", "target_end_date", "target_variable"))

# Exclusions ------------------------------------------------------
# dates should be between start of hub and until end of JHU data
data <- data |>
  filter(forecast_date >= as.Date("2021-03-07") &
           target_end_date <= as.Date("2023-03-10"))
# only keep forecasts up to 4 weeks ahead
data <- filter(data, horizon <= 4)
# remove anomalies (detected as of March 4th 2023, pre-data change)
rm_anomalies <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/5a2a8d48e018888f652e981c95de0bf05a838135/data-truth/anomalies/anomalies.csv")
data <- anti_join(data, rm_anomalies,
                        by = c("target_end_date",
                               "target_variable", "location"))
# remove any true values <0
data <- filter(data, true_value >= 0)
# only include predictions from models with all quantiles
rm_quantiles <- data |>
  group_by(model, forecast_date, location, target_variable) |>
  summarise(q = length(unique(quantile))) |>
  filter(q < 23)
data <- anti_join(data, rm_quantiles,
                        by = c("model", "forecast_date",
                               "target_variable", "location"))
data <- filter(data, !is.na(quantile)) # remove "median"
# remove duplicates
data <- data |>
  group_by_all() |>
  mutate(duplicate = row_number()) |>
  ungroup() |>
  filter(duplicate == 1) |>
  select(-duplicate)

# Save as parquet (avoid csv file size ~220MB) ----------------------------
arrow::write_parquet(data, here("data", "forecasts.parquet"))
