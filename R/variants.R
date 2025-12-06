# Identifying dominant variant phases across 30 European countries

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(plotly)

# Process variants data
variants_raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv",
                         progress = FALSE, show_col_types = FALSE)
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
                   "Omicron/BA.1" = "B.1.1.529", # Omicron
                   "Omicron/BA.1" = "BA.1",
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

# complete grid of dates and locations in ecdc year-weeks
date_location <- tibble(
  daily_date = seq.Date(from = as.Date("2020-01-04"), to = Sys.Date(), by = 7),
  year = isoyear(daily_date),
  week = isoweek(daily_date)) |>
  group_by(year, week) |>
  filter(daily_date == min(daily_date)) |>
  filter(between(daily_date,
                 as.Date("2021-03-01"), as.Date("2023-03-17"))) |>
  expand_grid(country = unique(variants_raw$country))

# filter to relevant dates
variants <- variants_raw |>
  # set dates as a complete grid of location-weeks
  mutate(year = as.numeric(substr(year_week, 1, 4)),
         week = as.numeric(substr(year_week, 6, 8))) |>
  left_join(date_location, by = c("year", "week", "country")) |>
  filter(!is.na(daily_date)) |>
  select(country, year_week, daily_date, source,
         number_sequenced, variant, percent_variant)

# group variants into named categories
variants <- variants |>
  mutate(variant_name = forcats::fct_recode(variant, !!!variant_names)) |>
  # remove "unknown" as counted in Other
  filter(!variant %in% c("UNK")) |>
  # sum variants by week
  group_by(country, year_week, daily_date,
           source, number_sequenced, variant_name) |>
  summarise(percent_variant = sum(percent_variant),
            .groups = "drop")


# Identify dominant variant phases -------------------------------------------
dominant_2 <- variants |>
  group_by(country, year_week, daily_date, source, number_sequenced) |>
  slice_max(order_by = percent_variant, with_ties = FALSE, n = 2) |>
  mutate(variant_name = as.character(variant_name),
         rank = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = source,
              values_from = c(number_sequenced,
                              variant_name, percent_variant),
              names_glue = "{source}_{.value}",
              values_fill = NA, id_cols = c(country, year_week, daily_date, rank))

# Apply rules to identify dominant variant phase, using the top 2 variants each week
# (1) Prefer GISAID for known reliability: For the first choice, prefer the GISAID first-ranked variant.
# (2) Avoiding "Other" category: If GISAID is "Other" and TESSy has a named variant, take TESSy. If the second-ranked variant is the same as the week before, and the first-ranked variant is "Other", take the second-ranked variant.
# (3) Temporal consistency: If the first-ranked variant is different to the week after and the week before, and the second-ranked variant appears in any source in both the week after and the week before, take the second-ranked variant from that source.
# TODO
# Apply these rules in order.
# Add new columns with: dominant variant name, dominant variant percentage, number sequenced, source, rule used to identify dominant variant.
