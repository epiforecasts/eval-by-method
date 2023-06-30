locations <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
  select(location_name, location)


# Get Rt estimates
# from: https://github.com/lin-lab/COVID19-Viz/tree/master/clean_data_pois
# download.file("https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/rt_table_export.csv.zip",
#                        destfile = here("data", "rt_lin.csv.zip"))

rt_lin <- read_csv(here("data", "rt_lin.csv.zip")) |>
  filter(resolution == "country") |>
  select(target_end_date = date_lag, location_name = dispID,
         rt_lower, rt, rt_upper) |>
  left_join()

# categorise Rt
rt_lin <- rt_lin |>
  mutate(rt_cat = case_when(rt_lower < 1 & rt_upper > 1 ~ "Cross",
                            rt_lower >= 1 & rt_upper > 1 ~ "Over",
                            rt_lower <= 1 & rt_upper <= 1 ~ "Under"))

# Epinow2 estimates -------------------------------------------------------
source(here("R", "get-rt.R"))

# Rt
# median_estimates <- get_covid19_nowcasts(dataset = "national/deaths", variable = "rt",
#                           earliest_date = "2021-03-07")
# median_estimates <- median_estimates |>
#   filter(country %in% locations$location_name)
# write_csv(median_estimates, here("data", "rt-epiforecasts-covid.csv"))
median_estimates <- read_csv(here("data", "rt-epiforecasts-covid.csv"))
median_estimates <- median_estimates |>
  rename(target_date = date, region = country) |>
  mutate(source = "covid.io")


# Growth rate
growth_rate <- get_covid19_nowcasts(dataset = "national/deaths",
                                    variable = "growth_rate",
                                    earliest_date = "2021-03-07")
growth_rate <- growth_rate |>
  left_join(locations, by = c("country" = "location_name")) |>
  filter(country %in% locations$location_name)
write_csv(growth_rate, here("data", "gr-epiforecasts-covid.csv"))

# Estimates made for European Hub Submissions ---------------------------------
scores_raw <- read_csv(here("data", "scores-raw.csv"))
dates <- gh(paste0("/repos/epiforecasts/europe-covid-forecast/contents/",
                   "rt-forecast/data/summary/cases?recursive=1"))
dates <- transpose(dates)
dates <- unlist(dates[["name"]])
rt <- map_dfr(dates,
              ~ read_csv(paste0("https://raw.githubusercontent.com/epiforecasts",
                                "/europe-covid-forecast/master/rt-forecast/data/summary/cases/",
                                .x, "/rt.csv")) |>
                mutate(reference_date = .x))
rt <- rt |>
  filter(between(date, min(scores_raw$forecast_date), max(scores_raw$forecast_date)))

# take median across Rt estimates
hub <- rt |>
  filter(type == "estimate") |>
  rename(target_date = date) |>
  group_by(target_date, region) |>
  summarise(
    n = n(),
    lower_90 = median(lower_90),
    lower_20 = median(lower_20),
    median = median(median),
    upper_20 = median(upper_20),
    upper_90 = median(upper_90),
    reference_date = max(reference_date),
    .groups = "drop") |>
  mutate(source = "hub-submissions")

# Compare ----------------------------------------------------
rt_source <- bind_rows(median_estimates, hub)

rt_source |>
  ggplot(aes(x = target_date, col = source, fill = source)) +
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20), alpha = 0.8, col = NA) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.5, col = NA) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = NULL) +
  facet_grid(rows = vars(region), scales = "free") +
  theme(legend.position = "bottom")
ggsave("rt-by-source.pdf", height = 30, width = 5)
