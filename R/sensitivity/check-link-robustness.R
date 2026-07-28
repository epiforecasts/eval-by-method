# Robustness check: log vs identity link for the log-scale WIS model.
#
# The response `wis` is already on the log-incidence scale. The production model
# (analysis-model.R) adds gaussian(link = "log"), a second log. Because log-scale
# WIS values are small and roughly symmetric, a gaussian(link = "identity") fit
# should give near-identical Method / CountryTargets partial effects. This script
# fits both and prints a side-by-side comparison
#
# Run: source(here::here("R", "sensitivity", "check-link-robustness.R"))

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
source(here("R", "process-data.R"))

check_link_robustness <- function() {
  # --- Data: mirror model_wis(scoring_scale = "log") ---
  m.data <- process_data(scoring_scale = "log") |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    mutate(Incidence = log(Incidence + 1))

  m.formula_joint <- wis ~
  Epi_target +
    s(Method, bs = "re") +
    s(CountryTargets, bs = "re") +
    s(Incidence) +
    s(Trend, bs = "re") +
    s(Location, bs = "re") +
    s(VariantPhase, bs = "re") +
    s(Horizon, by = Model, k = 3, bs = "sz") +
    s(Model, bs = "re")

  fit_one <- function(family) {
    bam(
      formula = m.formula_joint,
      data = m.data,
      family = family,
      method = "fREML",
      discrete = TRUE
    )
  }

  links <- list(log = gaussian(link = "log"),
                identity = gaussian(link = "identity"))

  # Fit every outcome under both links, extract focal random effects.
  effects <- imap(links, \(fam, link_name) {
    outcomes |>
      set_names() |>
      extract_ranef(fit_one(fam)) |>
      filter(group_var %in% c("Method", "CountryTargets")) |>
      mutate(link = link_name)
  }) |>
    list_rbind()

  # Side-by-side comparison of point estimates and CIs.
  comparison <- effects |>
    select(epi_target, group_var, group, link, value, lower_2.5, upper_97.5) |>
    pivot_wider(
      names_from = link,
      values_from = c(value, lower_2.5, upper_97.5)
    ) |>
    mutate(
      diff = value_identity - value_log,
      # CI overlap: do the two intervals intersect?
      ci_overlap = pmax(lower_2.5_log, lower_2.5_identity) <=
        pmin(upper_97.5_log, upper_97.5_identity)
    ) |>
    arrange(epi_target, group_var, group)

  message("--- log vs identity link: Method / CountryTargets partial effects ---")
  print(as.data.frame(comparison), digits = 3)
  message("\nMax |identity - log| difference in point estimate: ",
          round(max(abs(comparison$diff)), 4))
  message("All CIs overlap: ", all(comparison$ci_overlap))

  invisible(comparison)
}

check_link_robustness()
