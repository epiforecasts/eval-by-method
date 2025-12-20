# Display model inclusion criteria in the style of participant flow diagram
# Examples
#   flow <- create_model_flow()
#   flow |> fc_merge() |> fc_draw()

library(here)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(flowchart)

create_model_flow <- function() {
  forecasts <- arrow::read_parquet(here(
    "data",
    "covid19-forecast-hub-europe.parquet"
  ))
  fc_clean <- forecasts
  # Data cleaning
  fc_clean$horizon = as.numeric(substr(fc_clean$target, 1,2))
  fc_clean$target_variable = str_extract(fc_clean$target, "case|death|hosp")
  # Set forecast date to corresponding submission date
  fc_clean$forecast_date = fc_clean$target_end_date - weeks(fc_clean$horizon) + days(1)

  fc_clean <- fc_clean[c("model", "target_variable", "location", "forecast_date",
                         "horizon", "target_end_date", "quantile", "value")]
  # Study period: between start of hub and until end of JHU data
  fc_clean <- fc_clean[fc_clean$forecast_date >= as.Date("2021-03-07") &
                         fc_clean$target_end_date <= as.Date("2023-03-10"),]
  models0 <- distinct(fc_clean, target_variable, model)

  # Exclusions -----
  # (1) Only include predictions from models with all quantiles
  rm_quantiles <- fc_clean |>
    group_by(model, target_variable, forecast_date, location) |>
    summarise(q = length(unique(quantile))) |>
    filter(q < 23)
  fc_clean <- anti_join(fc_clean, rm_quantiles,
                        by = c("model", "target_variable",
                               "forecast_date", "location")
  )
  models1 <- distinct(fc_clean, target_variable, model) |>
    mutate(inc_quantile = TRUE)

  # (3) Only forecasts up to 4 weeks ahead
  fc_clean <- filter(fc_clean, horizon <= 4)
  models2 <- distinct(fc_clean, target_variable, model) |>
    mutate(inc_horizon = TRUE)

  # (2) Only forecasts for cases and deaths
  fc_clean <- filter(fc_clean, target_variable %in% c("case", "death"))
  models3 <- distinct(fc_clean, target_variable, model) |>
    mutate(inc_target = TRUE)

  # (4) Exclude Hub-created models
  fc_clean <- filter(fc_clean, !grepl("EuroCOVIDhub-", model))
  models4 <- distinct(fc_clean, target_variable, model) |>
    mutate(inc_xhub = TRUE)

  # Count models at each processing step
  models <- left_join(models0, models1) |>
    left_join(models2) |>
    left_join(models3) |>
    left_join(models4) |>
    mutate(across(starts_with("inc_"), ~ if_else(is.na(.), FALSE, .))) |>
    filter(target_variable != "hosp")

  flow <- imap(c("case", "death"),
               ~ models |>
                 filter(target_variable == .x) |>
                 as_fc(label = paste0("Models forecasting ",
                                      .x, "s"),
                       text_pattern = "{label}\n") |>
                 fc_filter(inc_quantile,
                           label = "Provided 23 quantiles",
                           show_exc = TRUE) |>
                 fc_filter(inc_horizon,
                           label = "Provided 1:4 week predictions",
                           show_exc = TRUE) |>
                 fc_filter(inc_xhub,
                           label = "Not created by Hub",
                           show_exc = TRUE) |>
                 fc_draw()
  )

  flow_chart <- flow |>
    fc_merge() |>
    fc_draw() |>
    fc_export(filename = "flowchart.png", path = here("plots"),
              width = 3000, height = 3000, res = 500)
}
