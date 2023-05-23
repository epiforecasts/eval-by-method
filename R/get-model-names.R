# Count teams contributing to European Forecast hub over time
library(gh)
library(purrr)
library(dplyr)
library(lubridate)
library(readr)
library(here)

# Get model names / dates from github ---------------------------
model_names <- gh(paste0("/repos/covid19-forecast-hub-europe/covid19-",
                         "forecast-hub-europe/contents/data-processed/?recursive=1"))
model_names <- transpose(model_names)
model_names <- model_names[["name"]]
model_files <- map(model_names,
                   ~ gh(paste0("/repos/covid19-forecast-hub-europe/covid19-",
                               "forecast-hub-europe/contents/data-processed/",
                               .x, "?recursive=1")))
model_names_dates <- unlist(model_files)
model_names_dates <- model_names_dates[names(model_names_dates) == "name"]

models <- tibble("file" = model_names_dates) |>
  mutate(model = substr(file, 12, nchar(file)-4),
         date = floor_date(as.Date(substr(file, 1, 10)),
                           unit="week")) |>
  select(-file)

# keep models submitting between 8 March 2021 to 4 weeks before the 10 March 2023 (when data ends)

models <- models |>
  filter(between(date,
                 as.Date("2021-03-07"),
                 as.Date("2023-03-10") - weeks(4)))

# N unique weekly submissions:
nrow(models |> filter(!grepl("EuroCOVIDhub", model)))

# unique models to classify:
models_distinct <- distinct(models, model)

write_csv(models_distinct, here("data", "models.csv"))
