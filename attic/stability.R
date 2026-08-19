# Forecast stability (temporal coherence)
#
# Exploratory side-analysis, outside the manuscript pipeline.
#
# A model predicts the same target week up to four times, at horizons 4, 3, 2,
# 1. Stability asks how much it changes its mind between those successive
# forecasts. It is measured entirely from the forecasts, without reference to
# the observation, so it is a property of the model's behaviour rather than of
# its accuracy.
#
# Following Brockhaus et al. (2023, PLoS Comput Biol 19:e1011653), who define
# temporal coherence for real-time Rt estimates as the requirement that
# "estimates issued at various times should not differ more than implied by the
# respective uncertainty intervals". They compare each real-time estimate with a
# consolidated estimate published 70 days later. The forecast analogue has no
# consolidated version, so we compare each forecast with the next one issued for
# the same target week.
#
# Metrics, all computed per revision step (a pair of forecasts of the same
# target week by the same model, one week apart):
#
#   cd            Cramer distance between the two predictive distributions.
#                 Primary measure. The divergence underlying the CRPS/WIS, so
#                 it is on the same footing as the accuracy outcome, and it uses
#                 the whole distribution rather than the median alone.
#   revision      Signed change in the median (Brockhaus MSD analogue).
#   abs_revision  Absolute change in the median (their MAD analogue).
#   coherent      Does the earlier forecast's 95% interval contain the later
#                 forecast's median? (their coverage analogue). Incoherent
#                 revisions are ones the model's own stated uncertainty did not
#                 allow for.
#   width_ratio   95% interval width, later / earlier. Should be below 1: a
#                 forecast made closer to the target ought to be sharper.
#   flip          Does the implied direction of change into the target week
#                 reverse between the two forecasts? (their R = 1 crossing
#                 analogue). Direction is the forecast median for the target
#                 week against the same forecast's estimate of the preceding
#                 week: its own horizon h-1 prediction, or the last observation
#                 for horizon 1.
#
# Everything is computed on log population-normalised incidence, matching the
# scale of the primary WIS analysis, so revisions are comparable across
# countries and between cases and deaths.
#
# Run: source(here::here("attic", "stability.R")); stability <- get_stability()

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
source(here("R", "utils-data.R"))

# Cramer distance ---------------------------------------------------------
# Approximates each predictive distribution by its 23 quantiles treated as an
# equally weighted sample, the standard approximation for hub quantile
# forecasts. Then CD(F, G) = E/2 for the energy distance E, i.e.
#   CD = mean|x - y| - mean|x - x'|/2 - mean|y - y'|/2
# over all pairs. Computed in row blocks to bound memory: the cross term needs
# K^2 = 529 differences per row.
cramer_distance <- function(x, y, block = 20000) {
  stopifnot(ncol(x) == ncol(y), nrow(x) == nrow(y))
  k <- ncol(x)
  i <- rep(seq_len(k), each = k)
  j <- rep(seq_len(k), times = k)

  starts <- seq(1, nrow(x), by = block)
  map(starts, \(s) {
    rows <- s:min(s + block - 1, nrow(x))
    xb <- x[rows, , drop = FALSE]
    yb <- y[rows, , drop = FALSE]
    cross <- rowMeans(abs(xb[, i, drop = FALSE] - yb[, j, drop = FALSE]))
    selfx <- rowMeans(abs(xb[, i, drop = FALSE] - xb[, j, drop = FALSE]))
    selfy <- rowMeans(abs(yb[, i, drop = FALSE] - yb[, j, drop = FALSE]))
    cross - selfx / 2 - selfy / 2
  }) |>
    unlist(use.names = FALSE)
}

# Forecasts in wide (one row per forecast, one column per quantile) form ----
widen_forecasts <- function(data_type) {
  pop <- read_csv(here("data", "populations.csv"), show_col_types = FALSE) |>
    rename(pop = population)

  get_forecasts(data_type = data_type) |>
    inner_join(pop, by = "location") |>
    mutate(
      quantile = round(quantile, 3),
      prediction = log(prediction / pop * 100000 + 1)
    ) |>
    select(model, location, target_end_date, forecast_date, horizon,
           quantile, prediction) |>
    pivot_wider(names_from = quantile, values_from = prediction,
                names_prefix = "q") |>
    drop_na()
}

# Direction of change implied by each forecast ----------------------------
# Anchor for the week before the target: the same forecast's own prediction one
# horizon shorter, or, at horizon 1, the last observation (available by then,
# since a horizon-1 forecast is issued the week after the anchor week ends).
add_direction <- function(wide, data_type) {
  pop <- read_csv(here("data", "populations.csv"), show_col_types = FALSE) |>
    rename(pop = population)
  obs_anchor <- read_csv(here("data", paste0("observed-", data_type, ".csv")),
                         show_col_types = FALSE) |>
    inner_join(pop, by = "location") |>
    transmute(location,
              target_end_date = target_end_date + days(7),
              horizon = 1,
              anchor_obs = log(observed / pop * 100000 + 1))

  # a forecast of week T - 7 at horizon h - 1 shares a forecast date with the
  # forecast of week T at horizon h, so it is that forecast's own view of the
  # preceding week
  anchor_own <- wide |>
    transmute(model, location,
              target_end_date = target_end_date + days(7),
              horizon = horizon + 1,
              anchor_own = q0.5)

  wide |>
    left_join(anchor_own, by = c("model", "location", "target_end_date",
                                 "horizon")) |>
    left_join(obs_anchor, by = c("location", "target_end_date", "horizon")) |>
    mutate(
      anchor = if_else(horizon == 1, anchor_obs, anchor_own),
      direction = sign(q0.5 - anchor)
    ) |>
    select(-anchor_own, -anchor_obs)
}

# Revision steps ----------------------------------------------------------
# One row per (model, location, target week, revision step). A step is labelled
# by the horizon it revises *to*: step 1 is the last revision before the target.
compute_stability <- function(data_type) {
  wide <- widen_forecasts(data_type) |>
    add_direction(data_type = data_type)
  qn <- grep("^q", names(wide), value = TRUE)

  to <- wide |>
    filter(horizon <= 3) |>
    mutate(join_h = horizon + 1)
  from <- wide |>
    rename(join_h = horizon)

  pairs <- inner_join(to, from,
    by = c("model", "location", "target_end_date", "join_h"),
    suffix = c("_to", "_from")
  )

  x <- as.matrix(pairs[, paste0(qn, "_to")])
  y <- as.matrix(pairs[, paste0(qn, "_from")])

  pairs |>
    transmute(
      epi_target = data_type,
      model, location, target_end_date,
      horizon_to = horizon, horizon_from = join_h,
      forecast_date = forecast_date_to,
      cd = cramer_distance(x, y),
      revision = q0.5_to - q0.5_from,
      abs_revision = abs(revision),
      coherent = q0.5_to >= q0.025_from & q0.5_to <= q0.975_from,
      width_ratio = (q0.975_to - q0.025_to) / (q0.975_from - q0.025_from),
      flip = !is.na(direction_to) & !is.na(direction_from) &
        direction_to != direction_from & direction_to != 0 &
        direction_from != 0,
      flip = if_else(is.na(direction_to) | is.na(direction_from), NA, flip)
    )
}

# Cached entry point ------------------------------------------------------
get_stability <- function(refresh = FALSE) {
  file <- here("data", "stability.csv")
  if (!refresh && file.exists(file)) {
    return(read_csv(file, show_col_types = FALSE))
  }
  stability <- map(c("case", "death"), compute_stability) |>
    bind_rows()
  write_csv(stability, file)
  return(stability)
}
