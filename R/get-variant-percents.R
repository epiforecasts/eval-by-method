rt <- read_csv(here("data", "rt-epiforecasts-covid.csv")) |>
  rename(target_end_date = date, location_name = country) |>
  mutate(epiyearweek = paste0(lubridate::epiyear(target_end_date), "-",
                              lubridate::epiweek(target_end_date))) |>
  filter(between(target_end_date,
                 min(scores_raw$target_end_date),
                 min(scores_raw$target_end_date)+365))

# variant_data <- read_csv("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv")
source("https://gist.githubusercontent.com/kathsherratt/a534bff397f0824403a2e81ba83ddbb9/raw/7dd2f9f96f0edd8e55af2ff9a35b7900f4be8055/download_variant_introduction.R")
variant_names <- c("B.1.617.2" = "Delta",
                   "B.1.1.529" = "Omicron",
                   "BA.1" = "Omicron-BA1")
variant_data <- download_variant_introduction(variant_codes = names(variant_names))
variant_select <- variant_data |>
  group_by(country, variant) |>
  pivot_longer(starts_with("date"),
               names_to = "variant_type", values_to = "variant_date") |>
  filter(variant %in% names(variant_names))

# Plot all Rt estimates & variants
plot_rt <- rt |>
  left_join(variant_select,
            by = c("location_name" = "country",
                   "target_end_date" = "variant_date")) |>
  mutate(variant = as.factor(variant),
         variant_plot = ifelse(!is.na(variant), 1, NA)) |>
  ggplot(aes(x = target_end_date)) +
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20), alpha = 0.8, col = NA) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.5, col = NA) +
  geom_point(aes(y = variant_plot, col = variant)) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = NULL) +
  facet_grid(rows = vars(location_name), scales = "free") +
  theme(legend.position = "bottom")

ggsave(filename = here("output", "rt.pdf"),
       plot = plot_rt,
       height = 30, width = 5)


# -------------------------------------------------------------------------
# Get scores
scores_raw <- read_csv(here("data", "scores-raw.csv"))

# Get variant introduction dates: 5-50%
source("https://gist.githubusercontent.com/kathsherratt/a534bff397f0824403a2e81ba83ddbb9/raw/7dd2f9f96f0edd8e55af2ff9a35b7900f4be8055/download_variant_introduction.R")
variant_names <- c("B.1.617.2" = "Delta",
                   "B.1.1.529" = "Omicron",
                   "BA.1" = "Omicron-BA1")
variant_data <- download_variant_introduction(variant_codes = names(variant_names))

locations <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
  select(location_name, location)

variant_dates <- scores_raw |>
  # distinct(location, target_end_date) |>
  left_join(locations, by = "location") |>
  left_join(variant_data,
            by = c("location_name" = "country")) |>
  mutate(variant_intro = ifelse(between(target_end_date,
                                        date_introduction, date_dominant),
                                "Intro", "Other")) |>
  select(location, target_end_date, variant_intro)
# 420 weeks' data points to cover 2 variant introductions across 30 countries

scores <- scores_raw |>
  filter(location %in% unique(variant_dates$location) &
           scale == "log")

variant_score <- variant_dates |>
  left_join(scores, by = c("target_end_date", "location"))
