# simple scoring

library(here)
library(ggplot2)
source(here("R", "get-metadata.R"))
source(here("R", "evaluation.R"))

metadata <- get_metadata_processed()
metadata <- metadata |>
  select(model_abbr, model_main)
scores_raw <- get_scores() |>
  # replace 0 counts with 1 for interval score
  mutate(wis = ifelse(wis == 0, 1, wis))

baseline_model <- "EuroCOVIDhub-ensemble"
baseline_scores <- scores_raw |>
  filter(model == baseline_model) |>
  select(target_variable, forecast_date, target_end_date, horizon, location,
         baseline_wis = wis)

baseline_scores |>
  filter(horizon == 2 & location %in% c("AT", "GB", "DE", "FR")) |>
  ggplot(aes(x = target_end_date, y = baseline_wis,
             col = location)) +
  geom_line() +
  geom_point(alpha = 0.5)


 scores <- scores_raw |>
  left_join(metadata, by = c("model" = "model_abbr")) |>
  left_join(baseline_scores) |>
  mutate(wis_ratio = wis / baseline_wis)

scores |>
  filter(wis_ratio <= 5) |>
  ggplot(aes(x = wis_ratio)) +
  geom_histogram(binwidth = 0.1)


# Score from scratch ------------------------------------------------------


