# Functions to identify Hub data and fetch/write metadata
library(gh)
library(purrr)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(here)
library(yaml)
library(googlesheets4)

# Get names of all models in a hub repo --------------------------------
# get_model_names()
# args
# repo: name of github repo (format: "org/repo")
get_model_names <- function(repo) {
  # Get all filepaths in top level data-processed directory
  data_processed <- gh(paste0(
    "/repos/", repo,
    "/contents/data-processed/?recursive=1"
  ))
  # Get names of all models
  model_abbr <- transpose(data_processed)
  model_abbr <- unlist(model_abbr[["name"]])
  return(model_abbr)
}

# Get model names and dates of forecast submissions ------------------
# get_model_submissions()
# args
# repo: name of github repo (format: "org/repo")
# model_abbr: optional vector of model names to get content for
# min_date, max_date: set to NULL to include all dates
get_model_submissions <- function(repo = "covid19-forecast-hub-europe/covid19-forecast-hub-europe",
                                  model_abbr = NULL,
                                  min_date = NULL,
                                  max_date = NULL) {
  if (is.null(model_abbr)) {
    model_abbr <- get_model_names(repo)
  }
  # Get contents of individual models' directories
  model_content <- map(
    model_abbr,
    ~ gh(paste0(
      "/repos/", repo,
      "/contents/data-processed/",
      .x,
      "?recursive=1"
    ))
  )
  # Get forecasts within each model's submissions folder
  model_content <- unlist(model_content)
  model_dates <- model_content[names(model_content) == "name"]
  model_submissions <- tibble("date" = model_dates) |>
    mutate(
      model = substr(date, 12, nchar(date) - 4),
      date = floor_date(as.Date(substr(date, 1, 10)),
        unit = "week"
      )
    )
  # Inclusion by date
  if (is.null(min_date)) {
    min_date <- min(model_submissions$date)
  }
  if (is.null(max_date)) {
    min_date <- max(model_submissions$date)
  }
  model_submissions <- model_submissions |>
    filter(between(
      date,
      as.Date(min_date),
      as.Date(max_date)
    ))

  return(model_submissions)
}

# Get raw metadata -------------------------------------
# get_metadata_raw()
# args
# model_abbr: vector of model abbreviations used in data-processed
# repo: name of github repo (format: "org/repo")
get_metadata_raw <- function(model_abbr, repo) {
  # - read metadata files from github
  # -- try a single filepath first to check which formatting style in use
  try_read <- try(read_yaml(paste0(
    "https://raw.githubusercontent.com/",
    repo, "/main/model-metadata/",
    model_abbr[1],
    ".yml"
  )))
  # (US hub uses old formatting style: txt on master)
  if ("try-error" %in% class(try_read)) {
    metadata_raw <- map_dfr(
      model_abbr,
      ~ read_yaml(
        paste0(
          "https://raw.githubusercontent.com/",
          repo, "/master",
          "/data-processed/", .x,
          "/metadata-", .x, ".txt"
        )
      )
    )
  } else # (euro hub uses yml on main/model-metadata)
  {
    metadata_raw <- tryCatch(map_dfr(
      model_abbr,
      ~ read_yaml(paste0(
        "https://raw.githubusercontent.com/",
        repo, "/main/model-metadata/",
        .x, ".yml"
      ))
    ))
  }

  # add name of hub
  metadata_raw <- mutate(metadata_raw,
    hub = repo
  )
  return(metadata_raw)
}

# Get contributors from model metadata (helper for EU hub with nested cols) ----
# get_contributors()
# args
# metadata_raw: raw metadata from get_metadata_raw()
get_contributors <- function(metadata_raw) {
  ctb <- metadata_raw |>
    select(model_abbr, model_contributors, team_funding) |>
    unnest_wider(col = c("model_contributors"))
  return(ctb)
}

# Write data to google sheet for qualitative coding ------------------------
# write_metadata()
# - writes metadata to google sheet
# - write access is with katharine.sherratt@lshtm.ac.uk
# args
# sheet_name: google sheet tab name
# write_local: optionally write to local csv file
write_metadata <- function(metadata_raw,
                           sheet_name = NULL,
                           write_local = TRUE) {
  # remove list-col of contributors and duplicate rows per model contributor
  metadata <- metadata_raw |>
    select(-model_contributors) |>
    distinct()

  # Write to google sheet
  gs4_auth()
  cat("Writing to google sheet")
  sheet_url <- "https://docs.google.com/spreadsheets/d/1XgXLYBCpdtjztJFhWDJz6G7A_Uw92dnnr-WFqGAFGn4/edit#gid=0"
  write_sheet(metadata, ss = sheet_url, sheet = sheet_name)

  # optionally save local copy
  if (write_local) {
    cat("Writing csv to local /data")
    write_csv(metadata, here("data", paste0("metadata-raw", ".csv")))
  }
}

# Fetch metadata from google sheet ---------------------------------------
# get_metadata_processed()
# args
# sheet_name: google sheet tab name
# write_local: optionally write to local csv file
get_metadata_processed <- function(sheet_name = "model-classification",
                                   write_local = TRUE) {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1XgXLYBCpdtjztJFhWDJz6G7A_Uw92dnnr-WFqGAFGn4/edit#gid=0"
  metadata <- read_sheet(sheet_url, sheet = sheet_name) |>
    select(model, KS, RB, SF, JM)

  if (write_local) {
    cat("Writing csv to local /data")
    write_csv(metadata, here("data", paste0(sheet_name, ".csv")))
  }
  return(metadata)
}
