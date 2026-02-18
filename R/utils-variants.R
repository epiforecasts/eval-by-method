# Variant phases
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)
library(forcats)
library(stringr)
library(purrr)

# # example:
# # load variant data for 32 countries from all sources
# variant_data <- bind_rows(get_variants_ecdc(date_grid, variant_names),
#                       get_variants_uk(date_grid, variant_names),
#                       get_variants_ch(date_location, variant_names))
# # classify each week into a variant phase
# variant_phases <- set_variant_phases(variant_data, date_location)

# Main wrapper function to classify variant phases -------------------------
classify_variant_phases <- function() {
  # set up data dictionary for standardised labelling of variants
  variant_names <- c("Unknown" = "UNK",
                     "Other" = "Other",
                     "Other" = "other",
                     "Other" = "other_lineages",
                     "Other" = "other_lineage",
                     "Other" = "SGTF",
                     # Alpha
                     "Alpha" = "Alpha",
                     "Alpha" = "B.1.1.7",
                     "Alpha" = "B.1.1.7+E484K",
                     "Other" = "B.1.1.318",
                     "Other" = "B.1.351", # Beta
                     "Other" = "P.1", # Gamma
                     "Other" = "P.2",
                     "Other" = "P.3",
                     # Delta
                     "Delta" = "Delta",
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
                     # Omicron BA 1
                     "Omicron-BA.1" = "B.1.1.529",
                     "Omicron-BA.1" = "BA.1",
                     "Omicron-BA.1" = "Omicron BA.1",
                     # BA 2
                     "Omicron-BA.2" = "BA.2",
                     "Omicron-BA.2" = "BA.2+L452X",
                     "Omicron-BA.2" = "BA.2.75",
                     "Omicron-BA.2" = "BA.2.86",
                     "Omicron-BA.2" = "JN.1",
                     "Other" = "BA.3",
                     # BA 4/5
                     "Omicron-BA.4/5" = "BA.4/BA.5",
                     "Omicron-BA.4/5" = "BA.4",
                     "Omicron-BA.4/5" = "BA.5",
                     "Omicron-BA.4/5" = "Omicron BA.5",
                     # BQ/XBB
                     "Omicron-BQ/XBB" = "BQ.1",
                     "Omicron-BQ/XBB" = "XBB",
                     "Omicron-BQ/XBB" = "XBB.1.5-like",
                     "Omicron-BQ/XBB" = "XBB.1.5-like+F456L")

  # set a complete grid of dates and locations in ecdc year-weeks
  date_grid <- tibble(
    target_end_date = seq.Date(from = as.Date("2020-01-04"),
                               to = as.Date("2023-03-17"), by = 7),
    year = isoyear(target_end_date),
    week = isoweek(target_end_date)) |>
    group_by(year, week) |>
    filter(target_end_date == min(target_end_date)) |>
    filter(between(target_end_date,
                   as.Date("2021-03-01"), as.Date("2023-03-17"))) |>
    mutate(year_week = paste0(year, "-",
                              str_pad(week, width = 2, side = "left", pad = "0")))
  locs <- unique(read_csv(here("data", "populations.csv"))$location)
  date_location <- date_grid |>
    expand_grid(location = locs)

  # load and clean variant data from each source
  variant_data <- bind_rows(get_variants_ecdc(date_location, variant_names),
                            get_variants_uk(date_grid, variant_names),
                            get_variants_ch(date_location, variant_names))

  # identify dominant variant phases
  variant_phases <- set_variant_phases(variant_data, date_location)

  return(variant_phases)
}


# utility functions -------------------------------------------------------

# Identify dominant variant in each week and location
set_variant_phases <- function(variant_data, date_location) {
  # Aggregate variant percentages across sources per location-week,
  # then identify the dominant variant
  dominant <- variant_data |>
    filter(!is.na(variant_percent)) |>
    group_by(location, target_end_date, variant_name) |>
    summarise(variant_percent = mean(variant_percent), .groups = "drop") |>
    group_by(location, target_end_date) |>
    slice_max(order_by = variant_percent, with_ties = FALSE, n = 1) |>
    mutate(variant_name = as.character(variant_name)) |>
    ungroup()

  # Expected chronological order of variant phases
  variant_order <- c("Alpha", "Delta", "Omicron-BA.1", "Omicron-BA.2",
                     "Omicron-BA.4/5", "Omicron-BQ/XBB")

  # Use single sequential phases for each location
  # Find first date each named variant was dominant (>50%) in each location
  phase_starts <- dominant |>
    mutate(dominant_name = ifelse(variant_name == "Other", NA,
                                  variant_name)) |>
    filter(!is.na(dominant_name), variant_percent > 50) |>
    group_by(location) |>
    arrange(target_end_date) |>
    group_by(dominant_name, .add = TRUE) |>
    summarise(target_end_date = first(target_end_date), .groups = "drop") |>
    # Enforce chronological ordering: remove out-of-sequence phases
    mutate(variant_rank = match(dominant_name, variant_order)) |>
    filter(!is.na(variant_rank)) |>
    group_by(location) |>
    arrange(target_end_date) |>
    filter(variant_rank == cummax(variant_rank)) |>
    select(-variant_rank)

  # Expand out to all weeks
  dominant_phases <- phase_starts |>
    right_join(date_location,
               by = c("location", "target_end_date")) |>
    group_by(location) |>
    arrange(target_end_date) |>
    fill(dominant_name, .direction = "downup") |>
    mutate(VariantPhase = factor(dominant_name)) |>
    select(location, target_end_date, VariantPhase)

  return(dominant_phases)

}

# Load and standardise variant data from source
# Switzerland
get_variants_ch <- function(date_grid, variant_names) {
  # 2020-2022: daily data from wastewater sampling
  # https://covid19.admin.ch/api/data/20231206-0sxi4s4a/sources/COVID19Variants_wgs.csv
  # - use rolling 7 day mean % at weekly intervals
  variants_ch_wgs <- read_csv(here("data", "variants", "ch-wgs.csv")) |>
    select(variant = variant_type,
           target_end_date = date,
           variant_percent = prct_mean7d) |>
    filter(target_end_date %in% date_grid$target_end_date)

  # 2022-2023: weekly data from hospital sampling
  # https://covid19.admin.ch/api/data/20231206-0sxi4s4a/sources/COVID19Variants_hosp_w.csv
  variants_ch_hosp <- read_csv(here("data", "variants", "ch-hosp.csv")) |>
    mutate(year_week = paste0(substr(date, 1, 4), "-", substr(date, 5, 6))) |>
    select(variant = variant_type,
           year_week,
           variant_percent = prct) |>
    left_join(distinct(date_grid, year_week, .keep_all = TRUE),
              by = c("year_week")) |>
    filter(year_week %in% date_grid$year_week &
             !target_end_date %in% variants_ch_wgs$target_end_date)

  ch <- bind_rows(variants_ch_wgs, variants_ch_hosp) |>
    mutate(location = "CH",
           source = "National") |>
    filter(variant != "all_sequenced") |>
    mutate(variant_name = as.character(fct_recode(variant, !!!variant_names))) |>
    select(location, source, target_end_date, variant_name, variant_percent)
}

# UK
get_variants_uk <- function(date_grid, variant_names) {
  # https://ukhsa-dashboard.data.gov.uk/respiratory-viruses/covid-19
  # Downloaded: October 2025
  variants_uk <- read_csv(here("data", "variants", "uk-ukhsa.csv")) |>
    select(variant_name = stratum,
           target_end_date = date,
           variant_percent = metric_value) |>
    # match dates to epi weeks
    mutate(target_end_date = dmy(target_end_date) - days(2),
           variant_name = gsub(x = variant_name, pattern = "Omicron ",
                               replacement = ""),
           location = "GB",
           source = "National",
           variant_name = as.character(fct_recode(variant_name,
                                                  !!!variant_names))) |>
    filter(target_end_date %in% unique(date_grid$target_end_date))
  return(variants_uk)
}

# ECDC
get_variants_ecdc <- function(date_location, variant_names) {
  # download data
  # https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv
  #
  variants_ecdc <- read_csv(here("data", "variants", "eu-ecdc.csv")) |>
    # match location codes to Hub standard
    mutate(location = ifelse(country == "Greece", "GR", country_code)) |>
    # filter variant data to relevant dates and variants
    # set dates as a complete grid of location-weeks
    left_join(date_location, by = c("year_week", "location")) |>
    filter(!is.na(target_end_date)) |>
    select(location, target_end_date, source,
           variant, variant_percent = percent_variant) |>
    # group variants into named categories
    mutate(variant_name = fct_recode(variant, !!!variant_names)) |>
    # remove "unknown", as counted in Other
    filter(!variant %in% c("UNK")) |>
    # sum variants by week
    group_by(location, target_end_date, source, variant_name) |>
    summarise(variant_percent = sum(variant_percent),
              .groups = "drop")

  return(variants_ecdc)
}
