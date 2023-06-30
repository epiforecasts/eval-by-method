library(here)
library(dplyr)
library(scoringutils)

# Get forecasts & observations -----
# source(here("R", "import-data.R")) # refresh/save data
data <- arrow::read_parquet(here("data", "forecasts.parquet"))

# Score forecasts on natural and log scales -----
scores <- data |>
  mutate(scale = "natural") |>
  # add version for the log transformations
  rbind(data |>
          mutate(
            scale = "log",
            true_value = log(true_value + 1),
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("model", "location",
                          "target_end_date", "forecast_date",
                          "horizon", "target_variable", "scale"),
                   na.rm = TRUE)

write_csv(scores, here("data", "scores-raw.csv"))

# Score all forecasts relative to each other (pairwise) -----
# set variables to group scores within [Add: target_end_date, epidemic phase]

score_pairwise <- function(scores,
                           score_by = c("model", "target_variable", "scale")) {
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
                                  score_by = c("model", "target_variable", "scale"))
write_csv(scores_pairwise, here("data", "scores-pw.csv"))

# All time / all location; by horizon
scores_pairwise_horizon <- score_pairwise(scores,
                                  score_by = c("horizon",
                                               "model", "target_variable", "scale"))
write_csv(scores_pairwise_horizon, here("data", "scores-pw-horizon.csv"))

# All horizons / all location; by forecast target date
scores_pairwise_target <- score_pairwise(scores,
                                  score_by = c("target_end_date",
                                               "model", "target_variable", "scale"))
write_csv(scores_pairwise_target, here("data", "scores-pw-target-date.csv"))

# All horizons / all location; by forecast creation date
scores_pairwise_origin <- score_pairwise(scores,
                                  score_by = c("forecast_date",
                                               "model", "target_variable", "scale"))
write_csv(scores_pairwise_origin, here("data", "scores-pw-forecast-date.csv"))


