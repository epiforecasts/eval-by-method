library("dplyr")
library("tidyr")
library("purrr")
library("readr")
library("lubridate")
library(stringr)

# Metadata ----------------------------------------------------------------
# Classification of model types
classify_models <- function(file = here("data", "model-classification.csv")) {
  methods <- read_csv(file) |>
    pivot_longer(
      -model,
      names_to = "classifier", values_to = "classification"
    ) |>
    filter(!(is.na(classification) | classification == "#N/A")) |>
    group_by(model) |>
    summarise(
      agreement = (n_distinct(classification) == 1),
      classification = names(
        sort(table(classification), decreasing = TRUE)[1]
      ),
      .groups = "drop"
    ) |>
    mutate(classification = factor(
      classification,
      levels = c(
        "Agent-based", "Mechanistic",
        "Semi-mechanistic", "Statistical",
        "Machine learning", "Other"
      )
    ))
  return(methods)
}

# Variant phases
classify_variant_phases <- function() {
  # step 1: set up data ----------------------------------------
  variant_names <- c("Other" = "Other",
                     "Unknown" = "UNK",
                     "Other" = "SGTF",
                     "Alpha" = "B.1.1.7",
                     "Alpha" = "B.1.1.7+E484K",
                     "Other" = "B.1.351", # Beta
                     "Other" = "P.1", # Gamma
                     "Other" = "P.3", # Gamma
                     "Delta" = "B.1.617.1",
                     "Delta" = "B.1.617.2",
                     "Delta" = "B.1.617.3",
                     "Delta" = "AY.4.2",
                     "Other" = "B.1.427", # Epsilon
                     "Other" = "B.1.429", # Epsilon
                     "Other" = "B.1.427/B.1.429",
                     "Other" = "B.1.526", # Kappa
                     "Other" = "B.1.525", # Eta
                     "Other" = "C.37", # Lambda
                     "Other" = "B.1.620",
                     "Other" = "B.1.621", # Mu
                     "Omicron-BA.1" = "B.1.1.529", # Omicron
                     "Omicron-BA.1" = "BA.1",
                     "Omicron-BA.2" = "BA.2",
                     "Omicron-BA.2" = "BA.2+L452X",
                     "Omicron-BA.2" = "BA.2.75",
                     "Omicron-BA.2" = "BA.2.86",
                     "Other" = "BA.3",
                     "Omicron-BA.4/5" = "BA.4/BA.5",
                     "Omicron-BA.4/5" = "BA.4",
                     "Omicron-BA.4/5" = "BA.5",
                     "Omicron-BQ/XBB" = "BQ.1",
                     "Omicron-BQ/XBB" = "XBB",
                     "Omicron-BQ/XBB" = "XBB.1.5-like",
                     "Omicron-BQ/XBB" = "XBB.1.5-like+F456L")
   # download data
  variants_raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv") |>
    mutate(country_code = ifelse(country == "Greece", "GR", country_code))

  # complete grid of dates and locations in ecdc year-weeks
  date_location <- tibble(
    daily_date = seq.Date(from = as.Date("2020-01-04"), to = Sys.Date(), by = 7),
    year = isoyear(daily_date),
    week = isoweek(daily_date)) |>
    group_by(year, week) |>
    filter(daily_date == min(daily_date)) |>
    filter(between(daily_date,
                   as.Date("2021-03-01"), as.Date("2023-03-17"))) |>
    expand_grid(country_code = unique(variants_raw$country_code))

  # filter variant data to relevant dates and variants
  variants <- variants_raw |>
    # remove variant identification when number sequenced <3
    mutate(across(c(variant, percent_variant),
                  ~ if_else(number_sequenced < 3, NA, .x))) |>
    # set dates as a complete grid of location-weeks
    mutate(year = as.numeric(substr(year_week, 1, 4)),
           week = as.numeric(substr(year_week, 6, 8))) |>
    left_join(date_location, by = c("year", "week", "country_code")) |>
    filter(!is.na(daily_date)) |>
    select(country_code, year_week, daily_date, source,
           percent_cases_sequenced, number_sequenced,
           variant, percent_variant) |>
    # group variants into named categories
    mutate(variant_name = forcats::fct_recode(variant, !!!variant_names)) |>
    # remove "unknown", as counted in Other
    filter(!variant %in% c("UNK")) |>
    # sum variants by week
    group_by(country_code, year_week, daily_date,
             source, number_sequenced, percent_cases_sequenced,
             variant_name) |>
    summarise(percent_variant = sum(percent_variant),
              .groups = "drop")

  # step 2: identify dominant variant in each week by source ------
  dominant <- variants |>
    group_by(country_code, year_week, daily_date,
             source, number_sequenced) |>
    slice_max(order_by = percent_variant, with_ties = FALSE, n = 1) |>
    mutate(variant_name = as.character(variant_name)) |>
    ungroup() |>
    # widen to have GISAID and TESSy side-by-side
    pivot_wider(names_from = source,
                values_from = c(number_sequenced,
                                variant_name, percent_variant),
                names_glue = "{source}_{.value}",
                values_fill = NA, id_cols = c(country_code,
                                              year_week, daily_date))

  # select single dominant variant each week
  # (rule 1) Prefer GISAID for known reliability: For the first choice, prefer the GISAID first-ranked variant.
  # (rule 2) Avoiding "Other"/NA category: If GISAID is "Other"/NA and TESSy has a named variant, take TESSy
  dominant <- dominant |>
    # clean and match between GISAID and TESSy
    mutate(dominant_variant = case_when(
      # prefer GISAID if TESSy missing
      is.na(TESSy_variant_name) ~
        paste("GISAID", GISAID_variant_name),
      # prefer TESSy if GISAID is missing or "Other"
      GISAID_variant_name %in% c("Other", NA) ~
        paste("TESSy", TESSy_variant_name),
      TRUE ~ paste("GISAID", GISAID_variant_name)))

  # add dominant % and separate source and name
  dominant <- dominant |>
    mutate(dominant_percent = case_when(
      dominant_variant %in% paste("GISAID", GISAID_variant_name) ~
        GISAID_percent_variant,
      dominant_variant %in% paste("TESSy", TESSy_variant_name) ~
        TESSy_percent_variant,
      TRUE ~ NA_real_)) |>
    separate(dominant_variant,
             into = c("source", "dominant_name"), sep = " ") |>
    mutate(dominant_name = na_if(dominant_name, "NA"))

  # step 3: use single sequential phases for each location -------
  dominant_phases <- dominant |>
    mutate(dominant_name = ifelse(dominant_name == "Other", NA, dominant_name)) |>
    filter(!is.na(dominant_name)) |>
    group_by(country_code) |>
    arrange(daily_date) |>
    group_by(dominant_name, .add = TRUE) |>
    summarise(daily_date = first(daily_date), .groups = "drop") |>
    right_join(date_location,
               by = c("country_code", "daily_date")) |>
    group_by(country_code) |>
    arrange(daily_date) |>
    fill(dominant_name, .direction = "downup") |>
    mutate(VariantPhase = factor(dominant_name)) |>
    select(location = country_code, target_end_date = daily_date, VariantPhase)

  return(dominant_phases)
}

# Scores data: add explanatory variables -----------------------------
# Get scores for all forecasts and add explanatory variables used:
#   number of country targets, method classification, trend of observed incidence
prep_data <- function(scoring_scale = "log") {
  scores_files <- list.files(here("output"), pattern = "scores-raw-.*\\.csv")
  names(scores_files) <- sub("scores-raw-(.*)\\..*$", "\\1", scores_files)
  # Get raw interval score
  scores_raw <- scores_files |>
    map(\(file) {
      read_csv(here("output", file))
    }) |>
    bind_rows(.id = "outcome_target") |>
    filter(scale == scoring_scale)

  # Target type
  country_targets <- scores_raw |>
    select(model, forecast_date, location) |>
    distinct() |>
    group_by(model, forecast_date) |>
    summarise(target_count = n(), .groups = "drop") |>
    group_by(model) |>
    summarise(CountryTargets = all(target_count == 1), .groups = "drop") |>
    mutate(CountryTargets = factor(CountryTargets,
      levels = c(TRUE, FALSE),
      labels = c(
        "Single-country",
        "Multi-country"
      )
    ))

  # Method type
  methods <- classify_models() |>
    select(model, Method = classification, agreement)

  # Incidence level + trend (see: R/import-data.r)
  obs <- names(scores_files) |>
    set_names() |>
    map(~ read_csv(here("data", paste0("observed-", .x, ".csv")))) |>
    bind_rows(.id = "outcome_target") |>
    mutate(Trend = factor(trend,
                          levels = c("Stable", "Increasing", "Decreasing"))) |>
    rename(Incidence = observed) |>
    select(target_end_date, location, outcome_target, Trend, Incidence)

  # variant phase
  variant_phases <- classify_variant_phases()

  # final data join -------------------
  data <- scores_raw |>
    left_join(obs, by = c("location", "target_end_date", "outcome_target")) |>
    left_join(variant_phases, by = c("location", "target_end_date")) |>
    left_join(country_targets, by = "model") |>
    left_join(methods, by = "model") |>
    rename(Model = model, Horizon = horizon) |>
    mutate(
      Model = as.factor(Model),
      outcome_target = paste0(str_to_title(outcome_target), "s"),
      Horizon = ordered(Horizon,
        levels = 1:4, labels = 1:4
      ),
      log_wis = log(wis + 0.01)
    ) |>
    filter(!is.na(Horizon)) ## horizon not in 1:4
  return(data)
}

