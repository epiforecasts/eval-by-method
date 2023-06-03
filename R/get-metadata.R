# Teams contributing to European Forecast hub
library(gh)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(here)
library(yaml)
library(googlesheets4)

# get metadata raw as submitted by teams
get_metadata_raw <- function(local = TRUE) {

  if (local) {
    metadata <- read_csv(here("data", "metadata-raw.csv"))

  } else {
    # Get model names / dates from github ---
    # get names of all models available
    cat("Fetching metadata from Github")
    all_models <- gh(paste0("/repos/covid19-forecast-hub-europe/covid19-",
                            "forecast-hub-europe/contents/data-processed/?recursive=1"))
    all_models <- transpose(all_models)
    all_models <- all_models[["name"]]

    # get dates of forecasts within each model's submissions folder
    model_names_dates <- map(all_models,
                             ~ gh(paste0("/repos/covid19-forecast-hub-europe/covid19-",
                                         "forecast-hub-europe/contents/data-processed/",
                                         .x,
                                         "?recursive=1")))
    model_names_dates <- unlist(model_names_dates)
    model_names_dates <- model_names_dates[names(model_names_dates) == "name"]
    model_names_dates <- tibble("file" = model_names_dates) |>
      mutate(model = substr(file, 12, nchar(file)-4),
             date = floor_date(as.Date(substr(file, 1, 10)),
                               unit="week")) |>
      select(-file)

    # inclusion criteria:
    # keep models submitting between 8 March 2021 to 4 weeks before the 10 March 2023
    model_names_dates <- model_names_dates |>
      filter(between(date,
                     as.Date("2021-03-07"),
                     as.Date("2023-03-10") - weeks(4)))

    # Get metadata ------------------------------------------------------------
    metadata_raw <- map_dfr(unique(model_names_dates$model),
                        ~ read_yaml(paste0("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/model-metadata/",
                                           .x, ".yml")))

    ## TODO check: inclusion criteria: model not designated secondary or other
    ## using this would exclude:
    ## --- "Secondary": 8 models (including epiforecasts epinow2, weekly growth)
    ## --- "Other": 3 models (1 arima variation, 2 EpiExpert variations)
    # metadata <- metadata_raw |>
    #   filter(!team_model_designation %in% c("secondary", "other"))

    # TODO pull out contributor details separately to get institution location
    ctb <- metadata_raw |>
      select(model_abbr, model_contributors)
    ctb <- unnest_wider(ctb, col = c("model_contributors"))

    # tidy
    metadata <- metadata_raw |>
      select(model_abbr,
             website_url, repo_url, citation,
             methods, methods_long, data_inputs) |>
      distinct()

    # write data ---------------------------------------------------------------
    # write to google sheet for qualitative coding
    gs4_auth() # write access is with katharine.sherratt@lshtm.ac.uk
    cat("Writing to google sheet")
    sheet_url <- "https://docs.google.com/spreadsheets/d/1XgXLYBCpdtjztJFhWDJz6G7A_Uw92dnnr-WFqGAFGn4/edit#gid=0"
    write_sheet(metadata, ss = sheet_url, sheet = "metadata-raw")
    # save local copy
    cat("Writing csv to local /data")
    write_csv(metadata, here("data", "metadata-raw.csv"))
  }

  return(metadata)
}

# get metadata with qualitative coding of model type/methods
get_metadata_processed <- function(local = TRUE) {
  if (local) {
    metadata <- read_csv(here("data", "metadata-processed.csv"))
  } else {
    sheet_url <- "https://docs.google.com/spreadsheets/d/1XgXLYBCpdtjztJFhWDJz6G7A_Uw92dnnr-WFqGAFGn4/edit#gid=0"
    metadata <- read_sheet(sheet_url, sheet = "metadata-processed")
    # save local copy
    write_csv(metadata, here("data", "metadata-processed.csv"))
  }
  return(metadata)
}
