library(here)
library(dplyr)
library(scoringutils)
source(here("R", "import-data.R"))

# Get forecasts & observations -----
# Get forecasts (note this is slow)
forecasts_raw <- get_forecasts()

# Observed data
# TODO: score without anomaly removal; anomaly detection not useful, see plot
obs <- get_observed()
forecasts <- left_join(forecasts_raw, obs,
                       by = c("location", "target_end_date")) |>
  rename(true_value = observed)
# remove anomalies (as of March 4th 2023, pre-data change)
anomalies <- get_anomalies()
forecasts <- anti_join(forecasts, anomalies,
                        by = c("target_end_date", "location"))

# Score forecasts on natural and log scales -----
scores <- forecasts |>
  mutate(scale = "natural") |>
  # add version for the log transformations
  rbind(forecasts |>
          mutate(
            scale = "log",
            true_value = log(true_value + 1),
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("model", "location",
                          "target_end_date", "forecast_date",
                          "horizon", "scale"),
                   na.rm = TRUE)

write_csv(scores, here("data", "scores-raw.csv"))

# Score all forecasts relative to each other (pairwise) -----
# set variables to group scores within
score_pairwise <- function(scores,
                           score_by = c("model", "scale")) {
  scores_pairwise <- scores |>
    pairwise_comparison(
      metric = "interval_score",
      baseline = "EuroCOVIDhub-ensemble",
      by = score_by)
  scores_pairwise <- scores_pairwise |>
    filter(compare_against == "EuroCOVIDhub-ensemble") |>
    select(model, all_of(score_by),
           rel_wis = scaled_rel_skill)
  return(scores_pairwise)
}

# All time / all location / all horizon
scores_pairwise <- score_pairwise(scores,
                                  score_by = c("model", "scale"))
write_csv(scores_pairwise, here("data", "scores-pw.csv"))

# All time / all location; by horizon
scores_pairwise_horizon <- score_pairwise(scores,
                                  score_by = c("horizon",
                                               "model", "scale"))
write_csv(scores_pairwise_horizon, here("data", "scores-pw-horizon.csv"))

# All horizons / all location; by forecast target date
scores_pairwise_target <- score_pairwise(scores,
                                  score_by = c("target_end_date",
                                               "model", "scale"))
write_csv(scores_pairwise_target, here("data", "scores-pw-target-date.csv"))

# All horizons / all location; by forecast creation date
scores_pairwise_origin <- score_pairwise(scores,
                                  score_by = c("forecast_date",
                                               "model", "scale"))
write_csv(scores_pairwise_origin, here("data", "scores-pw-forecast-date.csv"))

# All tine / all horizons; by location;
scores_pairwise_location <- score_pairwise(scores,
                                         score_by = c("location",
                                                      "model", "scale"))
write_csv(scores_pairwise_origin, here("data", "scores-pw-location.csv"))

