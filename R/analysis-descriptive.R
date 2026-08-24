# Aim: describe interval score in terms of model structure and country target type
# Load data:
# source(here("R", "process-data.R"))
# scores <- process_data(scoring_scale = "log")
library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(patchwork)
library(janitor)
library(kableExtra)
library(stringr)

# Figure: forecast error vs observed incidence ------------------------
# Shared prep: incidence per 100k, plus a log-spaced incidence bin mid-point
# (numeric, via findInterval -- not a formatted-label parse, which silently
# drops rows if the label text doesn't round-trip through as.numeric()).
prep_error_vs_obs <- function(scores, n_bins = 20) {
  plot_data <- scores |>
    mutate(incidence_pk = Incidence / pop * 100000) |>
    filter(!is.na(wis), !is.na(incidence_pk), incidence_pk > 0)

  log_inc <- log10(plot_data$incidence_pk)
  breaks <- seq(min(log_inc), max(log_inc), length.out = n_bins + 1)
  bin_idx <- findInterval(log_inc, breaks, all.inside = TRUE)
  plot_data$bin_mid <- 10^((breaks[bin_idx] + breaks[bin_idx + 1]) / 2)
  plot_data
}

incidence_scale_x <- function() {
  scale_x_log10(labels = \(x) format(x, big.mark = ",", trim = TRUE,
                                     scientific = FALSE, drop0trailing = TRUE))
}

# WIS against observed incidence: 5-25% and 25-75% quantile bands per
# horizon, no median line, so the full spread (not just central tendency)
# is visible. Bands nest per horizon (coloured), faceted by epi_target.
plot_error_vs_obs_bands <- function(scores, n_bins = 20, min_n = 20) {
  binned <- prep_error_vs_obs(scores, n_bins = n_bins) |>
    group_by(epi_target, Horizon, bin_mid) |>
    summarise(
      q05 = quantile(wis, 0.05),
      q25 = quantile(wis, 0.25),
      q75 = quantile(wis, 0.75),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= min_n)

  ggplot(binned, aes(x = bin_mid, fill = factor(Horizon), group = Horizon)) +
    geom_ribbon(aes(ymin = q05, ymax = q25), alpha = 0.15) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.35) +
    facet_wrap(~epi_target, scales = "free") +
    incidence_scale_x() +
    scale_y_log10() +
    scale_fill_viridis_d("Horizon (weeks)", end = 0.85) +
    labs(x = "Observed incidence per 100,000", y = "Performance (LWIS)") +
    theme_classic() +
    theme(legend.position = "bottom", strip.background = element_blank())
}

# Hex-binned density of WIS against observed incidence, by horizon
plot_error_vs_obs_hex <- function(scores, bins = 40) {
  plot_data <- prep_error_vs_obs(scores)

  ggplot(plot_data, aes(x = incidence_pk, y = wis)) +
    geom_hex(bins = bins) +
    facet_grid(rows = vars(epi_target), cols = vars(Horizon), scales = "free") +
    incidence_scale_x() +
    scale_y_log10() +
    scale_fill_viridis_c("Forecasts", trans = "log10", labels = scales::label_comma()) +
    labs(x = "Observed incidence per 100,000", y = "Performance (LWIS)") +
    theme_classic() +
    theme(legend.position = "bottom", strip.background = element_blank())
}

plot_wis_over_time <- function(scores) {
  panels <- c("Observed per 100k", paste("Horizon", 1:4))

  obs <- scores |>
    distinct(Location, epi_target, target_end_date, Incidence, pop) |>
    mutate(
      value = Incidence / pop * 1e5,
      panel = factor("Observed per 100k", panels)
    )

  wis <- scores |>
    filter(!is.na(wis)) |>
    mutate(
      value = wis,
      panel = factor(paste("Horizon", Horizon), panels)
    )

  ggplot(mapping = aes(x = target_end_date, y = value)) +
    geom_line(
      data = obs, aes(group = Location),
      alpha = 0.25, linewidth = 0.3
    ) +
    geom_point(
      data = wis,
      alpha = 0.02, size = 0.3, stroke = 0
    ) +
    facet_grid(
      rows = vars(panel), cols = vars(epi_target),
      scales = "free_y", switch = "y"
    ) +
    labs(x = NULL, y = NULL) +
    theme(strip.placement = "outside")
}

# Table: structure effects by epidemiological target ---------
# One row per model structure, one column per target, from the
# s(Method, Epi_target) cells.
print_table_method_target <- function(method_by_target) {
  method_by_target |>
    mutate(
      ratio = paste0(
        round(exp(value), 2),
        " (", round(exp(lower_2.5), 2), ", ", round(exp(upper_97.5), 2), ")"
      )
    ) |>
    select(Method, Epi_target, ratio) |>
    pivot_wider(names_from = Epi_target, values_from = ratio) |>
    arrange(Method) |>
    rename("Model structure" = Method) |>
    kable(
      align = c("l", "r", "r")
    ) |>
    kable_styling(full_width = FALSE)
}

# Table 2: unadjusted vs adjusted effects --------------------
print_table2 <- function(effects, show_ratio = TRUE) {
  effects |>
    filter(group_var %in% c("Method")) |>
    mutate(
      # Exponentiate point estimate and both CI bounds: multiplicative ratio
      # relative to the grand-mean LWIS
      ratio = paste0(
        round(exp(value), 2),
        " (", round(exp(lower_2.5), 2), ", ", round(exp(upper_97.5), 2), ")"
      )
    ) |>
    select(group_var, group, model, ratio) |>
    pivot_wider(
      names_from = model,
      values_from = ratio
    ) |>
    arrange(group_var, group) |>
    mutate(
      group_var = factor(group_var)
    ) |>
    select(
      group_var, group,
      Unadjusted, Adjusted
    ) |>
    rename(
      "Variable" = group_var,
      "Group" = group,
      "Unadjusted ratio (95% CI)" = Unadjusted,
      "Adjusted ratio (95% CI)" = Adjusted
    ) |>
    kable(
      align = c("l", "l", "r", "r")
    ) |>
    collapse_rows(columns = 1, valign = "top") |>
      kable_styling(full_width = FALSE)
}
