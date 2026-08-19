# Lagged ensemble
#
# Exploratory side-analysis, not part of the manuscript pipeline.
#
# For a target week T, the Hub ensemble has already predicted T up to four
# times, at horizons 1-4, from four successive forecast dates. This builds a
# "lagged ensemble": the quantile-wise median across those repeated
# predictions, and compares its accuracy against the real-time ensemble.
#
# get_forecasts() sets forecast_date = target_end_date - weeks(horizon) + 1 day,
# so for a fixed target_end_date the horizon *is* the lag. The components of the
# nominal-horizon-h lagged ensemble are simply the forecasts of the same target
# at horizons h ... h + max_lag (capped at 4, the largest horizon retained
# upstream). So horizon 1 has 4 components, horizon 2 has 3, horizon 3 has 2,
# and horizon 4 has 1 -- at horizon 4 the lagged ensemble is by construction
# identical to the real-time ensemble, which is a useful check.

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scoringutils)
source(here("R", "utils-data.R"))

max_horizon <- 4
hub_ensemble <- "EuroCOVIDhub-ensemble"

# Build -------------------------------------------------------------------
build_lagged_ensemble <- function(ens,
                                  max_lag = 3,
                                  model_name = "Lagged-ensemble") {
  # Quantile-wise median (Vincentization) over the same target predicted at
  # successively longer horizons. The median is monotone-preserving, so this
  # cannot introduce quantile crossing.
  ens |>
    rename(component_horizon = horizon) |>
    cross_join(tibble(horizon = 1:max_horizon)) |>
    filter(
      component_horizon >= horizon,
      component_horizon <= horizon + max_lag
    ) |>
    group_by(location, target_end_date, horizon, quantile) |>
    summarise(
      prediction = median(prediction),
      n_components = n_distinct(component_horizon),
      .groups = "drop"
    ) |>
    mutate(
      model = model_name,
      forecast_date = target_end_date - weeks(horizon) + days(1)
    ) |>
    select(
      location, forecast_date, horizon, target_end_date,
      model, quantile, prediction, n_components
    )
}

# Score -------------------------------------------------------------------
score_lagged_ensemble <- function(data_type = "death", max_lag = 3) {
  # Mirrors R/process-score.R: population-normalise, score on both scales.
  ens <- get_forecasts(data_type = data_type) |>
    filter(model == hub_ensemble)

  lagged <- build_lagged_ensemble(ens, max_lag = max_lag)

  # Score both models through an identical path so they are comparable
  forecasts <- ens |>
    mutate(n_components = 1L) |>
    bind_rows(lagged)

  obs <- read_csv(here("data", paste0("observed-", data_type, ".csv")),
    show_col_types = FALSE
  )
  pop <- read_csv(here("data", "populations.csv"), show_col_types = FALSE) |>
    rename(pop = population)
  obs <- left_join(obs, pop, by = "location")
  forecasts <- left_join(forecasts, obs,
    by = c("location", "target_end_date")
  )

  forecasts <- forecasts |>
    mutate(
      observed = observed / pop * 100000,
      prediction = prediction / pop * 100000
    )

  log_forecasts <- forecasts |>
    mutate(
      scale = "log",
      observed = log(observed + 1),
      prediction = log(prediction + 1)
    )

  scores <- forecasts |>
    mutate(scale = "natural") |>
    rbind(log_forecasts) |>
    mutate(quantile = round(quantile, 3)) |>
    as_forecast_quantile(
      predicted = "prediction", quantile_level = "quantile"
    ) |>
    score()

  write_csv(scores, here(
    "data",
    paste0("scores-lagged-ensemble-", data_type, ".csv")
  ))
  return(scores)
}

# Compare -----------------------------------------------------------------
compare_lagged_ensemble <- function(scores, max_lag = 3) {
  # Keep only forecast weeks where the lagged ensemble has its full complement
  # of components, so the contrast is not driven by uneven composition.
  full_only <- scores |>
    mutate(n_full = pmin(max_lag + 1, max_horizon - horizon + 1)) |>
    filter(n_components == n_full | model == hub_ensemble)

  # ... and only targets where both models are present
  complete_targets <- full_only |>
    distinct(target, location, target_end_date, horizon, scale, model) |>
    count(target, location, target_end_date, horizon, scale) |>
    filter(n == 2) |>
    select(-n)

  full_only |>
    inner_join(complete_targets,
      by = c("target", "location", "target_end_date", "horizon", "scale")
    ) |>
    group_by(target, scale, model, horizon) |>
    summarise(wis = mean(wis), n = n(), .groups = "drop")
}

plot_lagged_ensemble <- function(comparison, scoring_scale = "log") {
  comparison |>
    filter(scale == scoring_scale) |>
    ggplot(aes(x = horizon, y = wis, colour = model)) +
    geom_line() +
    geom_point() +
    facet_wrap(~target, scales = "free_y") +
    labs(
      x = "Horizon (weeks)",
      y = paste0("Mean WIS (", scoring_scale, " scale)"),
      colour = NULL
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
}

# Run ---------------------------------------------------------------------
scores <- map(c("case", "death"), \(target) {
  score_lagged_ensemble(data_type = target) |>
    mutate(target = target)
}) |>
  bind_rows()

comparison <- compare_lagged_ensemble(scores)
print(comparison, n = 40)

ggsave(here("attic", "ensemble-lag-wis.png"),
  plot_lagged_ensemble(comparison, scoring_scale = "log"),
  width = 6, height = 3.5, dpi = 300
)
