locations <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
  select(location_name, location)
scores_raw <- read_csv(here("data", "scores-raw.csv"))

# Epinow2 estimates -------------------------------------------------------
# Rt
source(here("R", "get-rt.R"))
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

# Estimates made for European Hub Submissions
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

# Compare
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

# Rt phase -------
# categorise Rt
rt <- rt |>
  mutate(rt_cat = case_when(rt_lower < 1 & rt_upper > 1 ~ "Cross",
                            rt_lower >= 1 & rt_upper > 1 ~ "Over",
                            rt_lower <= 1 & rt_upper <= 1 ~ "Under"))

# Variants --------------------------------------------------------------
source("https://gist.githubusercontent.com/kathsherratt/a534bff397f0824403a2e81ba83ddbb9/raw/7dd2f9f96f0edd8e55af2ff9a35b7900f4be8055/download_variant_introduction.R")
variant_names <- c("B.1.617.2" = "Delta", "B.1.1.529" = "Omicron")

variants <- download_variant_introduction(country_names = NULL,
                                          variant_codes = NULL)



rt <- read_csv(here("data", "rt-epiforecasts-covid.csv")) |>
  rename(target_end_date = date, location_name = country) |>
  mutate(epiyearweek = paste0(lubridate::epiyear(target_end_date), "-",
                              lubridate::epiweek(target_end_date))) |>
  filter(between(target_end_date,
                 min(scores_raw$target_end_date),
                 min(scores_raw$target_end_date)+365)) |>
  group_by(location_name, epiyearweek) |>
  summarise(across(lower_20:upper_90, median),
            target_end_date = max(target_end_date),
            n = n(),
            .groups = "drop") |>
  filter(n == 7)







# -------------------------------------------------------------------------


# Plot all Rt estimates
plot_rt <- rt |>
  mutate(change = ifelse(change, 1, NA)) |>
  ggplot(aes(x = target_end_date)) +
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20), alpha = 0.8, col = NA) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.5, col = NA) +
  geom_point(aes(y = change)) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = NULL) +
  facet_grid(rows = vars(location_name), scales = "free") +
  theme(legend.position = "bottom")
ggsave(filename = here("output", "rt.pdf"),
       plot = plot_rt,
       height = 30, width = 5)



# Scores By Rt >1< --------------------------------------------------------

rt <- read_csv(here("data", "rt-epiforecasts-covid.csv")) |>
  rename(target_end_date = date, location_name = country) |>
  mutate(epiyearweek = paste0(lubridate::epiyear(target_end_date), "-",
                              lubridate::epiweek(target_end_date))) |>
  filter(between(target_end_date,
                 min(scores_raw$target_end_date),
                 min(scores_raw$target_end_date)+365))

# categorise Rt
rt <- rt |>
  group_by(location_name, epiyearweek) |>
  summarise(across(lower_20:upper_90, median),
            target_end_date = max(target_end_date)) |>
  mutate(rt_cat = case_when(lower_20 < 1 & upper_20 > 1 ~ "Cross",
                            lower_20 >= 1 & upper_20 > 1 ~ "Over",
                            lower_20 <= 1 & lower_20 <= 1 ~ "Under")) |>
  select(location_name, target_end_date, rt_cat) |>
  left_join(read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
              select(location_name, location))

# Get scores
scores_raw_rt <- scores_raw |>
  left_join(rt, by = c("target_end_date", "location")) |>
  filter(scale == "log" &
           !is.na(rt_cat))

# Score all forecasts relative to each other (pairwise)
score_by <- c("rt_cat", "model")

scores_pairwise_rt <- scores_raw_rt |>
  select(model, horizon, location, forecast_date,
         interval_score, rt_cat) |>
  pairwise_comparison(
    metric = "interval_score",
    baseline = "EuroCOVIDhub-ensemble",
    by = score_by)

scores_pairwise_rt <- scores_pairwise_rt |>
  filter(compare_against == "EuroCOVIDhub-ensemble") |>
  select(model, all_of(score_by),
         rel_wis = scaled_rel_skill) |>
  mutate(rt_cat = factor(rt_cat, levels = c("Under", "Cross", "Over")))

write_csv(scores_pairwise_rt, here("data", "scores-pairwise-rt.csv"))

# -------------------------------------------------------------------------



# visualise by method
metadata <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification)


by_method_rt <- scores_pairwise_rt |>
  left_join(metadata) |>
  filter(!grepl("Other", method_type) & !is.na(method_type)) |>
  left_join(targets_by_model, by = "model")

plot_method_rt <- by_method_rt |>
  group_by(target_type, rt_cat) |>
  summarise(
    n = n(),
    value = quantile(rel_wis, quantiles),
    quantile = paste0("q", quantiles),
    .groups = "drop") |>
  pivot_wider(names_from = quantile) |>
  ggplot(aes(y = target_type, col = rt_cat, fill = rt_cat)) +
  geom_point(aes(x = q0.5), alpha = 0.8, position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.25, xmax = q0.75), linewidth = 4,
                 alpha = 0.5, position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.01, xmax = q0.99), linewidth = 4,
                 alpha = 0.2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1, lty = 2) +
  labs(y = NULL, x = NULL,
       caption = paste0(min(rt$target_end_date), " to ", max(rt$target_end_date)),
       col = "Weekly Rt estimate",
       fill = "Weekly Rt estimate") +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

plot_method_rt
ggsave(here("output", "target-by-rt.jpg"),
       width = 5, height = 4)
