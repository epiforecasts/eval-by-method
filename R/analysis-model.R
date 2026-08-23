# Aim: estimate the effect of model structure on log WIS,
# jointly accounting for other forecaster- and target- specific effects
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
library(gratia)
library(ggplot2)
source(here("R", "process-data.R"))
source(here("R", "utils-effects.R"))

# --- Specification ---
# Shared joint-model RHS, setup for reuse by the sensitivity scripts
# Response: WIS, modelled with a Tweedie family* and log link on both scales.
m.formula_joint <- wis ~
  # Method x epidemiological target interaction^
  # model structure (mechanistic, statistical, etc.) x epidemiological outcome
  s(Method, Epi_target, bs = "re") +
  # Epidemiological outcome (cases, deaths)^
  Epi_target +
  # CountryTargets: model predicts for single- vs multi-country
  s(CountryTargets, bs = "re") +
  # Incidence: log of current incidence level (smooth)
  s(Incidence) +
  # Trend: epidemic trend (stable, increasing, decreasing)
  s(Trend, bs = "re") +
  # Location: country location (random effect)
  s(Location, bs = "re") +
  # VariantPhase: dominant variant phase (random effect)
  s(VariantPhase, bs = "re") +
  # Horizon: forecast horizon (smooth, by model)
  s(Horizon, by = Model, k = 3, bs = "sz") +
  # Model: individual model (random effect)
  s(Model, bs = "re")

# * See R/sensitivity/check-family.R
# ^ Note on the interaction term:
# s(Method, Epi_target) lets each model structure differ in predicting
# cases versus deaths. The pooled per-structure effect is recovered
# as a contrast across cells (method_pooled_effects()).
# Epi_target stays as an unpenalised fixed effect, to take the component that's shared by all model structures into a single coefficient

# --- Functional model ---
# Function: fits the joint model and univariate models, extracts random effects,
# and saves results. Takes a spec_label to archive diagnostics for comparison
model_wis <- function(
  scoring_scale = "log",
  family_link = "log",
  output_dir = "output",
  spec_label = NULL
) {
  # --- Data handling
  m.data <- process_data(scoring_scale = scoring_scale)
  m.data <- m.data |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    filter(!is.na(wis)) |> # drop unscored forecasts explicitly
    mutate(Epi_target = as.factor(epi_target))

  # Settings for log or natural scale
  if (scoring_scale == "log") {
    # log-transform incidence if scoring on log scale
    m.data <- m.data |>
      mutate(Incidence = log(Incidence + 1))
  } else if (scoring_scale != "natural") {
    stop("scoring_scale must be either 'log' or 'natural'")
  }
  # tw() deparses its `link` argument, so use do.call to pass on the value
  m.family <- do.call(tw, list(link = family_link))

  # --- Model fitting
  # univariate per effect
  message("--------fitting univariate models")
  m.formulas_uni <- list(
    method = wis ~ s(Method, bs = "re"),
    epi_target = wis ~ Epi_target,
    target = wis ~ s(CountryTargets, bs = "re"),
    incidence = wis ~ s(Incidence),
    trend = wis ~ s(Trend, bs = "re"),
    location = wis ~ s(Location, bs = "re"),
    variant = wis ~ s(VariantPhase, bs = "re"),
    horizon = wis ~ s(Horizon, by = Model, k = 3, bs = "sz"),
    model = wis ~ s(Model, bs = "re")
  )
  m.fit <- function(m.formula) {
    bam(
      formula = m.formula,
      data = m.data,
      family = m.family,
      method = "fREML",
      control = gam.control(trace = TRUE),
      discrete = TRUE
    )
  }
  m.fits_uni <- map(m.formulas_uni, m.fit)

  # Joint; shared RHS defined at file scope
  message("--------fitting joint model")
  m.fits_joint <- m.fit(m.formula_joint)

  # --- Output handling
  # Epi_target is a fixed parametric term, so not returned by extract_ranef.
  # Pull the Deaths-vs-Cases contrast from the parametric table and shape to
  # match the random-effects columns, so downstream plotting/tables treat as
  # pseudo group_var = "Epi_target".
  extract_target_effect <- function(fit, model_label) {
    fe <- gammit::extract_fixed(fit)
    ci_cols <- grep("^lower_|^upper_", names(fe), value = TRUE)
    fe |>
      filter(term == "Epi_targetDeaths") |>
      transmute(
        group_var = "Epi_target",
        group = "Deaths",
        value,
        se,
        lower_2.5 = .data[[ci_cols[grepl("^lower", ci_cols)]]],
        upper_97.5 = .data[[ci_cols[grepl("^upper", ci_cols)]]],
        model = model_label
      )
  }

  # Univariate random effects (exclude smooth-only and the fixed target fit)
  random_effects_uni <- m.fits_uni[
    !grepl("horizon|incidence|epi_target", names(m.fits_uni))
  ] |>
    map(extract_ranef_terms) |>
    list_rbind() |>
    mutate(model = "Unadjusted") |>
    bind_rows(extract_target_effect(m.fits_uni$epi_target, "Unadjusted"))

  # Drop the raw interaction cells from `effects`: reported per target
  # via `method_by_target`
  random_effects_joint <- extract_ranef_terms(m.fits_joint) |>
    filter(group_var != "Method:Epi_target") |>
    bind_rows(method_pooled_effects(m.fits_joint)) |>
    mutate(model = "Adjusted") |>
    bind_rows(extract_target_effect(m.fits_joint, "Adjusted"))

  random_effects <- random_effects_joint |>
    bind_rows(random_effects_uni)

  # Per-target structure effects, one row per Method x Epi_target cell.
  # Kept out of `effects` so tables that print every group_var do not double-count
  # against the pooled effect derived from the same cells.
  method_by_target <- method_target_effects(m.fits_joint) |>
    mutate(model = "Adjusted")

  # Extract model checks
  checks <- k.check(m.fits_joint)
  formula <- m.fits_joint$formula
  results <- list(
    data = m.data,
    effects = random_effects,
    method_by_target = method_by_target,
    checks = checks,
    formula = formula
  )

  dir.create(here(output_dir, "plots"),
             recursive = TRUE, showWarnings = FALSE)

  saveRDS(results, here(output_dir, "results.rds"))

  # --- Diagnostics ---
  # Observed vs fitted
  stopifnot(nrow(m.data) == length(m.fits_joint$y))
  fit_obs <- tibble::tibble(
    observed = m.fits_joint$y,
    fitted = fitted(m.fits_joint),
    epi_target = m.data$epi_target
  )
  saveRDS(fit_obs, here(output_dir, "fit_obs.rds"))

  # appraise() plots
  p <- appraise(m.fits_joint)
  ggsave(here(output_dir, "plots", "check_joint.png"), p, dpi = 300)

  # Keep a labelled copy plus summary statistics, so this fit stays comparable
  if (!is.null(spec_label)) {
    archive_diagnostics(m.fits_joint, spec_label, scoring_scale, p)
  }
}

# --- More diagnostics ---
# Archive under a stable label to compare successive model specs
# Appends to output/diagnostics/fit-summary.csv
archive_diagnostics <- function(fit, spec_label, scoring_scale, plot,
                                dir = here("output", "diagnostics")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(spec_label, "_", scoring_scale, "_check.png")),
         plot, dpi = 300)

  # Get deviance (rather than raw) residuals for comparison across families
  r <- residuals(fit, type = "deviance")
  r <- r[is.finite(r)]
  centred <- r - mean(r)
  # Population (biased) moment estimates
  m2 <- mean(centred^2)
  row <- tibble::tibble(
    spec_label = spec_label,
    scale = scoring_scale,
    family = fit$family$family,
    link = fit$family$link,
    formula = paste(deparse(formula(fit)), collapse = " "),
    n = length(fit$y),
    aic = AIC(fit),
    dev_expl = summary(fit)$dev.expl,
    resid_skew = mean(centred^3) / m2^(3 / 2),
    resid_kurtosis = mean(centred^4) / m2^2,
    fitted_on = as.character(Sys.Date())
  )

  path <- file.path(dir, "fit-summary.csv")
  summary_table <- mutate(row, across(everything(), as.character))
  if (file.exists(path)) {
    summary_table <- read_csv(path, show_col_types = FALSE) |>
      # coerce so a previously-written column type can't block
      mutate(across(everything(), as.character)) |>
      filter(!(spec_label == row$spec_label & scale == row$scale)) |>
      bind_rows(summary_table)
  }
  write_csv(summary_table, path)
  invisible(row)
}
