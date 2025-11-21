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

variant_names <- c("Other" = "Other",
                   "Unknown" = "UNK",
                   "Alpha" = "B.1.1.7",
                   "Delta" = "B.1.617.2",
                   "Omicron" = "B.1.1.529",
                   "Omicron-BA1" = "BA.1")

  # date lookup table for ecdc year-weeks
  date_range <- tibble(
    daily_date = seq.Date(from = as.Date("2020-01-04"), to = Sys.Date(), by = 7),
    year = isoyear(daily_date),
    week = isoweek(daily_date)) |>
    group_by(year, week) |>
    filter(daily_date == min(daily_date)) |>
    mutate(forecast_period = between(daily_date,
                                     as.Date("2021-03-01"),
                                     as.Date("2023-03-17")))

  # get data
  variants_raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv",
                       progress = FALSE, show_col_types = FALSE)

  # filter as needed and set dates
  variants <- variants_raw |>
    filter(source == "GISAID") |>
    mutate(year = as.numeric(substr(year_week, 1, 4)),
           week = as.numeric(substr(year_week, 6, 8))) |>
    left_join(date_range, by = c("year", "week")) |>
    filter(forecast_period) |>
    select(country, daily_date, variant, percent_variant) |>
    mutate(dominant = percent_variant >= 50,
           variant_name = factor(variant,
                                 levels = variant_names,
                                 labels = names(variant_names)))

  variants |>
    ggplot(aes(x = daily_date, y = percent_variant, col = variant_name)) +
    geom_line() +
    facet_wrap(~ country, ncol = 1) +
    theme(legend.position = "bottom")
  ggsave("variants.pdf", height = 20)



  # get the first maxima of the variant
  introduction_period <- variants |>
    group_by(variant, country) |>
    arrange(daily_date) |>
    filter(percent_variant <= 99) |>
    slice_max(percent_variant, with_ties = FALSE) |>
    select(variant, country, variant_peak_date = daily_date)

  # get the period before the first maxima where the variant is e.g. >5<50%
  var_data <- variants |>
    left_join(introduction_period, by = c("variant", "country")) |>
    filter(daily_date <= variant_peak_date &
             percent_variant >= introduction_percent &
             percent_variant <= dominant_percent) |>
    group_by(country, variant) |>
    summarise(date_introduction = min(daily_date),
              date_dominant = max(daily_date) + 7,
              date_peak = min(variant_peak_date))


# Score by variant phase -----------------------------------------


delta <- variant_data |>
  filter(variant == "B.1.617.2") |>
  select(location_name = country,
         delta_intro = date_introduction,
         delta_dominant = date_dominant)

omicron <- variant_data |>
  filter(variant == "BA.1") |>
  select(location_name = country,
         omicron_intro = date_introduction,
         omicron_dominant = date_dominant)

locations <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
  select(location_name, location)

scores_raw_var <- scores_raw |>
  filter(scale == "log") |>
  # distinct(location, target_end_date) |>
  left_join(locations, by = "location") |>
  left_join(delta, by = "location_name") |>
  left_join(omicron, by = "location_name") |>
  mutate(variant = case_when(
    target_end_date <= delta_intro ~ "pre-delta",
    target_end_date >= delta_intro & target_end_date <= delta_dominant ~ "delta intro",
    target_end_date >= delta_dominant & target_end_date <= omicron_intro ~ "delta dominant",
    target_end_date >= omicron_intro & target_end_date <= omicron_dominant ~ "omicron intro",
    target_end_date >= omicron_dominant ~ "omicron dominant"
  )) |>
  filter(!is.na(variant))  # exclude countries with no variant data (UK, Swiss)
