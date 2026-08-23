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
plot_error_vs_obs <- function(scores) {
  plot_data <- scores |>
    mutate(incidence_pk = Incidence / pop * 100000) |>
    filter(!is.na(wis),
           !is.na(Incidence)
    )

  ggplot(plot_data, aes(x = incidence_pk, y = wis)) +
    geom_point(alpha = 0.03, size = 0.4, stroke = 0) +
    facet_grid(rows = vars(epi_target), cols = vars(Horizon), scales = "free") +
    scale_x_log10(labels = \(x) format(x, big.mark = ",", trim = TRUE,
                                       scientific = FALSE, drop0trailing = TRUE)) +
    scale_y_continuous() +
    labs(
      x = "Observed incidence", y = "Performance (LWIS)",
      colour = NULL, fill = NULL
    ) +
    theme(legend.position = "bottom")

  plot_data |>
    filter(Horizon==1) |>
    mutate(bincidence_pk = as.factor(signif(incidence_pk, 4))) |>
    ggplot(aes(x = bincidence_pk, y = wis)) +
    geom_boxplot() +
    facet_wrap(~epi_target, scales = "free") +
    labs(x = "", y = "") +
    theme(legend.position = "bottom")
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
#
# scores |>
#   filter(!is.na(wis)) |>
#   ggplot(aes(x = target_end_date, y = wis)) +
#   geom_hex(bins = 50) +
#   scale_fill_viridis_c(trans = "log10", name = "Forecasts") +
#   facet_grid(rows = vars(epi_target), cols = vars(Horizon), scales = "free_y") +
#   labs(x = NULL, y = "LWIS")
#
# scores |>
#   filter(!is.na(wis), !is.na(Incidence)) |>
#   mutate(incidence_pk = Incidence / pop * 1e5) |>
#   ggplot(aes(x = target_end_date, y = incidence_pk, z = wis)) +
#   stat_summary_hex(bins = 40, fun = median) +
#   scale_y_log10() +
#   scale_fill_viridis_c(name = "Median LWIS") +
#   facet_grid(rows = vars(epi_target), cols = vars(Horizon), scales = "free_y")

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
