# Sensitivity: covariates before the forecast date vs at the target date (#187)
#
# The primary model adjusts for incidence, trend and variant phase in the week
# ending 8 days before the forecast date (target_end_date - (horizon + 1)
# weeks; see process_data()). That uses only information the forecaster could
# have had: the week ending the day before the forecast date was at best partly
# reported when the forecast was made.
#
# The earlier primary model joined the same covariates at the target week,
# which uses the observed outcome. That conditions the evaluation on
# information the forecaster did not have (a "forecaster's dilemma", Lerch et
# al. 2017), and with a Tweedie log-link GAM it weights each score by a function
# of the outcome, which breaks propriety. It is kept here as a sensitivity
# analysis: it adjusts for realised rather than predictable difficulty.
#
# This script:
#   1. describes how far the two sets of covariates agree, by horizon
#   2. fits the joint model with target-date covariates (both scales); only
#      needed if the archived fit in output/sensitivity/covariate-date/target/
#      is missing
#   3. compares adjusted effects of the primary fit in output/<scale>/ against
#      the target-date fit
#
# Outputs: output/sensitivity/covariate-date/
#   - covariate-agreement.csv
#   - target/<scale>/results.rds, fit_obs.rds  (target-date fit)
#   - effects-comparison-<scale>.csv, effects-comparison-<scale>.png
#
# Run: source(here::here("R", "sensitivity", "check-covariate-date.R"))
#      check_covariate_agreement()
#      fit_target_date()             # slow; skip while the archive exists
#      compare_covariate_date("log"); compare_covariate_date("natural")

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
source(here("R", "process-data.R"))

sens_dir <- here("output", "sensitivity", "covariate-date")

# 1. Agreement between target-date and primary covariates ---------------------
check_covariate_agreement <- function(scoring_scale = "log") {
  dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)
  target <- process_data(scoring_scale, covariate_date = "target")
  primary <- process_data(scoring_scale, covariate_date = "forecast",
                          covariate_lag_weeks = 1)
  stopifnot(nrow(target) == nrow(primary))

  # Rows align (same scores, only covariates differ), so compare row-wise
  agreement <- tibble(
    Epi_target = target$epi_target,
    Horizon = target$Horizon,
    trend_target = target$Trend,
    trend_primary = primary$Trend,
    variant_target = target$VariantPhase,
    variant_primary = primary$VariantPhase,
    log_inc_target = log(target$Incidence + 1),
    log_inc_primary = log(primary$Incidence + 1)
  ) |>
    group_by(Epi_target, Horizon) |>
    summarise(
      n = n(),
      trend_agree = mean(trend_target == trend_primary, na.rm = TRUE),
      variant_agree = mean(variant_target == variant_primary, na.rm = TRUE),
      log_incidence_cor = cor(log_inc_target, log_inc_primary,
                              use = "complete.obs"),
      primary_covariate_missing = sum(is.na(trend_primary) |
                                        is.na(log_inc_primary) |
                                        is.na(variant_primary)),
      .groups = "drop"
    )

  write_csv(agreement, file.path(sens_dir, "covariate-agreement.csv"))
  agreement
}

# 2. Fit with target-date covariates ------------------------------------------
fit_target_date <- function(scales = c("log", "natural")) {
  source(here("R", "analysis-model.R"))
  walk(scales, \(scale) {
    model_wis(
      scoring_scale = scale,
      output_dir = file.path(sens_dir, "target", scale),
      spec_label = "covariate-target-date",
      covariate_date = "target"
    )
  })
}

# 3. Compare adjusted effects --------------------------------------------------
compare_covariate_date <- function(scale = "log",
                                   primary_dir = here("output", scale),
                                   target_dir = file.path(sens_dir, "target",
                                                          scale)) {
  read_effects <- function(dir) {
    results <- readRDS(file.path(dir, "results.rds"))
    results$effects |>
      filter(model == "Adjusted") |>
      bind_rows(results$method_by_target |>
                  mutate(group_var = "Method by outcome")) |>
      select(group_var, group, value, lower_2.5, upper_97.5)
  }

  comparison <- bind_rows(
    target = read_effects(target_dir),
    primary = read_effects(primary_dir),
    .id = "covariate_date"
  )

  wide <- comparison |>
    pivot_wider(names_from = covariate_date,
                values_from = c(value, lower_2.5, upper_97.5)) |>
    mutate(difference = value_primary - value_target)
  write_csv(wide, file.path(sens_dir, paste0("effects-comparison-", scale, ".csv")))

  # Plot effects on the ratio scale (exp of log-link effect), excluding the
  # many individual model and location effects (summarised by correlation)
  focal <- c("Method by outcome", "Method", "Trend", "VariantPhase",
             "CountryTargets", "Epi_target")
  plot <- comparison |>
    filter(group_var %in% focal) |>
    mutate(group_var = factor(group_var, levels = focal),
           covariate_date = factor(covariate_date,
                                   levels = c("target", "primary"),
                                   labels = c("Target week",
                                              "Week before forecast date"))) |>
    ggplot(aes(x = exp(value), y = group, colour = covariate_date)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = exp(lower_2.5), xmax = exp(upper_97.5)),
                    position = position_dodge(width = 0.5), size = 0.3) +
    facet_grid(group_var ~ ., scales = "free_y", space = "free_y") +
    scale_colour_manual(values = c("Target week" = "#b2182b",
                                   "Week before forecast date" = "#2166ac")) +
    labs(x = "Ratio of expected LWIS (adjusted)", y = NULL,
         colour = "Covariates measured at") +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text.y = element_text(angle = 0))
  ggsave(file.path(sens_dir, paste0("effects-comparison-", scale, ".png")),
         plot, width = 7, height = 9, dpi = 300, bg = "white")

  # Rank agreement of individual model effects across the two fits
  model_cor <- wide |>
    filter(group_var == "Model") |>
    summarise(spearman = cor(value_target, value_primary, method = "spearman"),
              max_abs_difference = max(abs(difference)))

  list(comparison = wide, model_rank_correlation = model_cor, plot = plot)
}
