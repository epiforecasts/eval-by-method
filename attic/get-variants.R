# Gets variants of concern dataset from ECDC
# For each variant/country finds the first period
#  with variant % greater than 5% and up to 50%
#  before the first peak for that variant
# Note ECDC data are in weeks, dates are indexed to Saturdays to match target end dates
#
# Example
#   variant_names <- c("B.1.617.2" = "Delta", "B.1.1.529" = "Omicron")
#   forecast_variants <- download_variant_introduction(introduction_percent = 5,
#                                                   country_names = "Germany",
#                                                   variant_codes = names(variant_names))
library(here)
library(dplyr)
library(readr)
library(lubridate)

download_variant_introduction <- function(country_names = NULL,
                                          variant_codes = NULL,
                                          introduction_percent = 5,
                                          peak_percent = 99) {
  # date lookup table for ecdc year-weeks
  date_range <- tibble(
    daily_date = seq.Date(from = as.Date("2020-01-04"), to = Sys.Date(), by = 7),
    year = isoyear(daily_date),
    week = isoweek(daily_date)) %>%
    group_by(year, week) %>%
    filter(daily_date == min(daily_date))

  # get data
  variants <- read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv", progress = FALSE, show_col_types = FALSE)
  if (is.null(variant_codes)) {variant_codes <- unique(variants$variant)}
  if (is.null(country_names)) {country_names <- unique(variants$country)}

  # filter as needed and set dates
  variants <- variants %>%
    filter(variant %in% variant_codes &
             country %in% country_names) %>%
    mutate(year = as.numeric(substr(year_week, 1, 4)),
           week = as.numeric(substr(year_week, 6, 8))) %>%
    left_join(date_range, by = c("year", "week")) %>%
    select(country, daily_date, variant, starts_with("number"), starts_with("percent")) |>
    mutate(percent_seq_ma = zoo::rollmedian(percent_variant, 3, align = "right", fill = NA))

  # get the first maxima of the variant
  introduction_period <- variants %>%
    group_by(variant, country) %>%
    arrange(daily_date) %>%
    filter(percent_variant <= peak_percent) %>%
    slice_max(percent_variant, with_ties = FALSE) %>%
    select(variant, country, variant_peak_date = daily_date)

  # get the period before the first maxima where the variant is e.g. >5<50%
  var_data <- variants %>%
    left_join(introduction_period, by = c("variant", "country")) %>%
    filter(daily_date <= variant_peak_date &
             percent_variant >= introduction_percent &
             percent_variant <= 50) %>%
    group_by(country, variant) %>%
    summarise(date_introduction = min(daily_date),
              date_dominant = max(daily_date)+7,
              date_peak = min(variant_peak_date))

  return(var_data)
}

summarise_variants <- function(introduction_percent) {
  variant_names <- c("B.1.351" = "Beta",
                     "B.1.617.2" = "Delta",
                     "B.1.1.529" = "Omicron")
  variant_data <- download_variant_introduction(variant_codes = names(variant_names),
                                                introduction_percent = introduction_percent)
  variant_medians <- variant_data |>
    group_by(variant) |>
    summarise(across(starts_with("date"),
                     ~ median(.x, na.rm=T)))
  return(variant_medians)
}
