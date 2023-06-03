# scoring from the hub eval -------------------
library(here)
library(dplyr)
library(readr)
library(lubridate)
library(scoringutils)

# get scores from before data change march 10
get_scores <- function(local = TRUE) {
  if (local) {
    scores <- read_csv(here("data", "scores.csv"))
    } else {
      scores <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/3ad05ecb51ba27a2493c93ab1b12489158500654/evaluation/scores.csv")
      write_csv(scores, here("data", "scores.csv"))
    }

  # keep models that fit inclusion criteria - see "R/get-metadata.R"
  models <- read_csv(here("data", "metadata-raw.csv"))
  scores <- filter(scores, model %in% c("EuroCOVIDhub-baseline", models$model_abbr))

  # keep forecasts of deaths, <= 4wk
  scores <- filter(scores, target_variable == "inc death") |>
    filter(horizon <= 4)

  return(scores)
}

pairwise_scoring <- function(scores = NULL,
                             baseline_model = "EuroCOVIDhub-baseline",
                             by_variables = "horizon") {
  # pairwise comparisons against ensemble
  # see: https://github.com/covid19-forecast-hub-europe/EuroForecastHub/blob/main/R/summarise_scores.R
  pairwise <- scores |>
    filter(n_quantiles == 23) |>
    select(model, horizon, location, location_name,
           forecast_date, interval_score = wis,
           all_of(by_variables)) |>
    pairwise_comparison(
      metric = "interval_score",
      baseline = baseline_model,
      by = by_variables
    )

  rel_wis <- pairwise |>
    filter(compare_against == baseline_model) |>
    select(rel_wis = scaled_rel_skill,
           all_of(by_variables))

  return(rel_wis)
}

# # plot
# library(ggplot2)
# library(forcats)
#
# rel_wis_plot <- rel_wis_baseline |>
#   filter(rel_wis <= 5) |>
#   mutate(horizon = factor(horizon, levels = 1:4, ordered = TRUE))
#
# rel_wis_plot |>
#   ggplot(aes(x = horizon, y = rel_wis)) +
#   geom_boxplot() +
#   geom_point(position = position_jitter(width = 0.1)) +
#   geom_hline(yintercept = 1, lty = 2)
