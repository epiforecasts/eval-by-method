# Diagnostic: within-model temporal autocorrelation of forecast scores.
#
# The primary GAMM (R/analysis-model.R) carries no temporal correlation
# structure, so it treats observations as conditionally independent given the
# random effects. Forecast scores are a weekly repeated-measures series (by
# forecast_date) at horizons 1-4, so likely residual autocorrelation, particularly
# across the 1:4-week horizon block sharing one forecast origin, which could bias
# standard errors.
#
# This script quantifies that autocorrelation. It does NOT add an AR structure
# to the production model; it is a diagnostic to justify (or qualify) the
# independence assumption.
#
# Residuals come from the LOG-RESPONSE arm (gaussian(identity) on log(wis),
# model-logresp.R), whose plain residuals() are link-scale and near-symmetric
# (skew ~= -0.8) -- the clean substrate for an ACF. The primary
# gaussian(link="log") fit's residuals (deviance = response = Pearson) live on
# the raw, strongly right-skewed WIS scale, where heteroskedasticity would
# masquerade as autocorrelation, so they are deliberately avoided. The raw
# series (A) uses log(wis) so it sits on the same scale as the residuals (B).
#
# Three diagnostics:
#   A. lag-1..4 ACF per (Model, Location, Horizon, epi_target) series on log(wis)
#   B. the same ACF on the GAMM residuals (the key independence test)
#   C. cross-horizon (1:4) correlation at a fixed forecast origin
#
# Run: source(here::here("R", "sensitivity", "check-autocorrelation.R"))

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(mgcv)
source(here("R", "sensitivity", "model-logresp.R")) # model_wis_logresp(), m.formula_joint

# Minimum series length (weekly points) to estimate an ACF from.
.min_series_n <- 10L
# Lags reported (weeks).
.acf_lags <- 1:4
# Number of forecast origins sampled for the spaghetti plot.
.n_origins_plot <- 300L

check_autocorrelation <- function() {
  # --- Fit the log-response model and recover the aligned data ---
  # Reconstruct m.data with the SAME filters/mutate as model-logresp.R:29-37 so
  # rows align 1:1 with fit$y (verified by stopifnot below).
  m.data <- process_data(scoring_scale = "log") |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    filter(!is.na(wis)) |>
    mutate(
      Epi_target = as.factor(epi_target),
      Incidence = log(Incidence + 1),
      wis_log = log(pmax(wis, 1e-4))
    )

  fit <- model_wis_logresp()
  stopifnot(nrow(m.data) == length(fit$y))
  m.data$resid <- residuals(fit)

  # ---------------------------------------------------------------------------
  # A & B: per-series ACF on log(wis) and on residuals
  # ---------------------------------------------------------------------------
  # series = one weekly sequence at a fixed lead time
  series_acf <- function(values, dates) {
    # order in time, require enough points, drop series with gaps handled by
    # acf() on the ordered vector (treats rows as equally spaced weekly steps)
    o <- order(dates)
    v <- values[o]
    d <- as.Date(dates[o])
    if (length(v) < .min_series_n || anyNA(v)) return(NULL)
    if (any(as.integer(diff(d)) != 7L)) return(NULL)
    a <- acf(v, lag.max = max(.acf_lags), plot = FALSE, demean = TRUE)$acf[, 1, 1]
    tibble(lag = .acf_lags, acf = a[.acf_lags + 1L])
  }

  acf_by_series <- m.data |>
    group_by(epi_target, Model, Location, Horizon) |>
    summarise(
      raw = list(series_acf(wis_log, forecast_date)),
      resid = list(series_acf(resid, forecast_date)),
      .groups = "drop"
    ) |>
    # keep series that produced an ACF (>= .min_series_n points)
    filter(map_lgl(raw, ~ !is.null(.x)))

  n_series <- nrow(acf_by_series)

  summarise_acf <- function(col, label) {
    acf_by_series |>
      select(all_of(col)) |>
      unnest(cols = all_of(col)) |>
      group_by(lag) |>
      summarise(
        median = median(acf, na.rm = TRUE),
        q25 = quantile(acf, 0.25, na.rm = TRUE),
        q75 = quantile(acf, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(series = label, .before = 1)
  }

  acf_summary <- bind_rows(
    summarise_acf("raw", "log(WIS)"),
    summarise_acf("resid", "GAMM residual")
  ) |>
    arrange(series, lag)

  # ---------------------------------------------------------------------------
  # C: cross-horizon correlation at a fixed forecast origin (the 1:4 concern)
  # ---------------------------------------------------------------------------
  # For each (Model, Location, epi_target, forecast_date) the four horizons share
  # one origin. Correlate horizons across origins, separately per scale.
  cross_horizon_cor <- function(value_col) {
    wide <- m.data |>
      select(epi_target, Model, Location, forecast_date, Horizon,
             value = all_of(value_col)) |>
      filter(Horizon %in% 1:4) |>
      pivot_wider(names_from = Horizon, values_from = value,
                  names_prefix = "h", values_fn = mean) |>
      select(h1, h2, h3, h4)
    cor(wide, use = "pairwise.complete.obs")
  }

  cor_raw <- cross_horizon_cor("wis_log")
  cor_resid <- cross_horizon_cor("resid")

  # ---------------------------------------------------------------------------
  # Spaghetti plot: residual across horizons 1-4, one faint line per origin.
  # Within-origin correlation shows up as lines that move together (parallel /
  # consistently sloped) rather than crossing at random.
  # ---------------------------------------------------------------------------
  origins <- m.data |>
    filter(Horizon %in% 1:4) |>
    distinct(epi_target, Model, Location, forecast_date) |>
    mutate(origin_id = row_number())
  set.seed(1)
  sampled <- origins |> slice_sample(n = min(.n_origins_plot, nrow(origins)))
  spaghetti_data <- m.data |>
    filter(Horizon %in% 1:4) |>
    inner_join(sampled, by = c("epi_target", "Model", "Location", "forecast_date"))

  spaghetti_plot <- ggplot(
    spaghetti_data,
    aes(x = Horizon, y = resid, group = origin_id)
  ) +
    geom_line(alpha = 0.08, colour = "black") +
    stat_summary(aes(group = 1), fun = mean, geom = "line",
                 colour = "red", linewidth = 1) +
    facet_wrap(~epi_target) +
    labs(
      x = "Forecast horizon (weeks ahead)",
      y = "Model residual (log WIS scale)",
      title = NULL
    ) +
    theme_minimal()

  # --- Report ---
  message("--- Within-model temporal autocorrelation diagnostic ---")
  message("Series (Model x Location x Horizon x epi_target, n >= ",
          .min_series_n, " weekly points): ", n_series)
  message("\nA/B. Per-series ACF, median [IQR] across series, by lag (weeks):")
  print(as.data.frame(acf_summary), digits = 3, row.names = FALSE)

  message("\nC. Cross-horizon correlation at fixed forecast origin -- log(WIS):")
  print(round(cor_raw, 3))
  message("\nC. Cross-horizon correlation at fixed forecast origin -- GAMM residual:")
  print(round(cor_resid, 3))

  # One-line verdict
  resid_lag1 <- acf_summary |>
    filter(series == "GAMM residual", lag == 1) |> pull(median)
  raw_lag1 <- acf_summary |>
    filter(series == "log(WIS)", lag == 1) |> pull(median)
  resid_xh <- mean(cor_resid[upper.tri(cor_resid)])
  message("\nVerdict: median lag-1 ACF ", round(raw_lag1, 2), " (log WIS) -> ",
          round(resid_lag1, 2), " (residual); ",
          "mean cross-horizon residual r = ", round(resid_xh, 2), ".")

  invisible(list(
    acf_by_series = acf_by_series,
    acf_summary = acf_summary,
    cor_raw = cor_raw,
    cor_resid = cor_resid,
    n_series = n_series,
    spaghetti_plot = spaghetti_plot
  ))
}

# Auto-run when sourced directly (CLI). Set CHECK_AUTOCORR_NORUN=1 to source the
# function only -- e.g. from the supplement, which calls it once and renders the
# returned tables, to avoid re-fitting the model twice.
if (!nzchar(Sys.getenv("CHECK_AUTOCORR_NORUN"))) {
  check_autocorrelation()
}
