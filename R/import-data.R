library(here)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

# Observed data ---------------------------------------------------------
walk(c("case", "death"), \(data_type) {
  file_name <- paste0("truth_JHU-Incident%20", str_to_title(data_type), "s.csv")
  obs <- read_csv(paste0(
    "https://raw.githubusercontent.com/covid19-forecast-hub-europe/",
    "covid19-forecast-hub-europe/main/data-truth/JHU/", file_name
  )) |>
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
  write_csv(obs, here("data", paste0("observed-", data_type, ".csv")))
}

# Population data ---------------------------------------------------------
pop <- read_csv(paste0(
  "https://raw.githubusercontent.com/european-modelling-hubs/",
  "covid19-forecast-hub-europe/main/data-locations/locations_eu.csv"
), show_col_types = FALSE) |>
  select(location, population)
write_csv(pop, here("data", paste0("populations.csv")))
