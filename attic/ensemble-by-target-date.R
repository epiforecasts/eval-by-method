"""
Evolving ensembles that draw on all forecasts available, rather than only single latest set of forecasts
(rationale: if more is more, use all available for a single target; follows scenario hub trajectory work. We know that >1wk forecasts are worse, but including more even bad models seems to help performance)
"""

library(here)
library(ggplot2)
source(here("R", "import-data.R"))
source(here("attic", "create-ensembles.R"))

# Get forecasts -----
# Get forecasts (note this is slow)
forecasts <- get_forecasts()

# check how many weeks included in each forecast
horizons <- forecasts |>
  filter(quantile == 0.5) |>
  group_by(forecast_date, location, model) |>
  summarise(n = n())
# keep only forecasts with 1:4 weeks
forecasts_4wk <- filter(horizons, n == 4) |>
  select(-n) |>
  left_join(forecasts)

forecasts_4wk <- forecasts_4wk |>
  filter(model != "EuroCOVIDhub-ensemble") |>
  mutate(quantile = as.factor(round(quantile, 2)))

# Create each ensemble ------------------
median_datewise <- forecasts_4wk |>
  group_by(forecast_date, target_end_date,
           location, quantile) |>
  summarise(prediction = median(prediction, na.rm = TRUE),
            n = n(),
            ensemble_type = "forecast_date",
            .groups = "drop")
median_median <- median_datewise |>
  group_by(target_end_date,
           location, quantile) |>
  summarise(prediction = median(prediction, na.rm = TRUE),
            n = n(),
            ensemble_type = "forecast_date_median",
            .groups = "drop")
median_alltime <- forecasts_4wk |>
  group_by(target_end_date,
           location, quantile) |>
  summarise(prediction = median(prediction, na.rm = TRUE),
            n = n(),
            ensemble_type = "all_time",
            .groups = "drop")

median <- bind_rows(median_datewise, median_median, median_alltime) |>
  mutate(ensemble_type = factor(ensemble_type,
                                levels = c("forecast_date",
                                           "forecast_date_median",
                                           "all_time")))

# example target ----------------------------------------------
median |>
  filter(location == "DE" &
           quantile %in% c(0.01,0.25,0.5,0.75,0.99)) |>
  pivot_wider(names_from = quantile, names_prefix = "q",
              values_from = prediction) |>
  mutate(forecast_date = ifelse(is.na(forecast_date),
                                      ensemble_type,
                                      forecast_date),
         forecast_date = as.factor(forecast_date)) |>
  # Plot
  ggplot(aes(x = target_end_date)) +
  #geom_point(aes(y = observed)) +
  #geom_line(aes(y = observed)) +
  geom_line(aes(y = q0.5,
                col = forecast_date),
            alpha = 0.6) +
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75,
                  fill = forecast_date),
              alpha = 0.4) +
  geom_ribbon(aes(ymin = q0.01, ymax = q0.99,
                  fill = forecast_date),
              alpha = 0.2) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  # Use a single colour for successive weeks' forecasts
  scale_fill_hue(h = c(50,50)) +
  scale_colour_hue(h = c(50,50)) +
  #
  labs(x = NULL, y = "Weekly deaths in Germany") +
  facet_wrap(~ ensemble_type, scales = "free") +
  theme(legend.position = "none",
        strip.background = element_blank())

# Score -----------------------------------------------------------
# Observed data
obs <- get_observed()
forecasts <- left_join(forecasts, obs,
                       by = c("location", "target_end_date")) |>
  rename(true_value = observed)

# Score forecasts on natural and log scales -----
scores <- median |>
  mutate(scale = "natural") |>
  # add version for the log transformations
  rbind(median |>
          mutate(
            scale = "log",
            true_value = log(true_value + 1),
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("ensemble_type", "location",
                          "target_end_date",
                          "forecast_date",
                          "horizon", "scale"),
                   na.rm = TRUE)
