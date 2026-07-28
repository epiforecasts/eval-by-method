# Aim: use a GAMM to model the effects of model structure and country target type on WIS
# Model:
# Method: model method (mechanistic, statistical, etc.)
# CountryTargets: model predicts for single- vs multi-country
# Trend: epidemic trend (stable, increasing, decreasing)
# Incidence: log of current incidence level (smooth)
# Location: location (random effect)
# VariantPhase: dominant variant phase (random effect)
# Horizon: forecast horizon (smooth, by model)
# Model: individual model (random effect)
#
# Response: WIS, modelled with a Tweedie family and log link on both scales.
# See R/sensitivity/check-family.R

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

# Shared joint-model RHS, reused by the sensitivity scripts so they fit an
# identical specification.
#
# s(Method, Epi_target) lets each model structure differ in how well it predicts
# cases versus deaths, rather than assuming one shared effect across both. Bosse
# et al. (2022) found human-judgement models did better on cases and worse on
# deaths, which a pooled structure effect cannot represent.
#
# There is deliberately no separate s(Method) term. mgcv's bs = "re" interaction
# is an unconstrained zero-mean prior over all Method x Epi_target cells, so its
# target-average is exactly what a Method main effect would capture; with both
# terms penalised, the split between them is decided by the relative variance
# estimates rather than the data. Fitted together, mgcv gave s(Method) 0.001 edf
# against 4.9 for the interaction. The pooled per-structure effect is recovered
# afterwards as a contrast across cells (method_pooled_effects()).
#
# Epi_target stays as a fixed effect for the mirror-image reason. The same
# aliasing applies -- the within-target average of the cells is what a target
# main effect represents -- but only one of the two terms is penalised, so the
# unpenalised fixed effect takes the component common to all structures and the
# cells keep only departures from it. That separation holds in the fitted model:
# the cells average to zero within each target to ~1e-13, so the entire
# deaths-versus-cases difference sits in the fixed coefficient (-1.03) and none
# leaks into the structure estimates (largest cell 0.12). Dropping it would force
# a large effect through a penalised term; making it random would try to estimate
# a variance from two levels.
m.formula_joint <- wis ~
  Epi_target +
  s(Method, Epi_target, bs = "re") +
  s(CountryTargets, bs = "re") +
  s(Incidence) +
  s(Trend, bs = "re") +
  s(Location, bs = "re") +
  s(VariantPhase, bs = "re") +
  s(Horizon, by = Model, k = 3, bs = "sz") +
  s(Model, bs = "re")

# Archive a fit's diagnostics under a stable label so successive model
# specifications can be compared rather than overwriting each other.
# Appends to output/diagnostics/fit-summary.csv, upserting on
# (spec_label, scale) so re-running a spec replaces its own row.
archive_diagnostics <- function(fit, spec_label, scoring_scale, plot,
                                dir = here("output", "diagnostics")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(spec_label, "_", scoring_scale, "_check.png")),
         plot, dpi = 300)

  # Deviance residuals put every family on a comparable footing; the raw
  # response residuals of a log-link Gaussian would confound family choice
  # with the skew of WIS itself.
  r <- residuals(fit, type = "deviance")
  r <- r[is.finite(r)]
  centred <- r - mean(r)
  # Population (biased) moment estimates: with n ~ 5e5 the small-sample
  # correction is negligible and this keeps the values comparable to the
  # skewness already quoted in the supplement.
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
      # coerce so a previously-written column type can't block the bind
      mutate(across(everything(), as.character)) |>
      filter(!(spec_label == row$spec_label & scale == row$scale)) |>
      bind_rows(summary_table)
  }
  write_csv(summary_table, path)
  invisible(row)
}

model_wis <- function(
  scoring_scale = "log",
  family_link = "log",
  output_dir = "output",
  spec_label = NULL
) {
  # --- Data handling ---
  m.data <- process_data(scoring_scale = scoring_scale)
  m.data <- m.data |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    filter(!is.na(wis)) |> # drop unscored forecasts explicitly (bam would drop these silently)
    mutate(Epi_target = as.factor(epi_target))

  # Settings for log or natural scale. Both scales use the same family
  if (scoring_scale == "log") {
    # log-transform incidence to match scoring on log scale
    m.data <- m.data |>
      mutate(Incidence = log(Incidence + 1))
  } else if (scoring_scale != "natural") {
    stop("scoring_scale must be either 'log' or 'natural'")
  }
  # tw() deparses its `link` argument, so passing the variable directly would
  # send the literal string "family_link". do.call forces the value through.
  m.family <- do.call(tw, list(link = family_link))

  # --- Model formula ---
  # Univariate for each
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

  # Full model: shared RHS defined at file scope (see top of file)

  # --- Model fitting ---
  # Single fit over the full dataset; epi_target is a fixed factor inside the model
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
  # Fit
  message("--------fitting univariate models")
  m.fits_uni <- map(m.formulas_uni, m.fit)

  message("--------fitting joint model")
  m.fits_joint <- m.fit(m.formula_joint)

  # --- Output handling ---
  # Epi_target is a fixed parametric term, so it is NOT returned by extract_ranef.
  # Pull the Deaths-vs-Cases contrast from the parametric table and shape it to
  # match the random-effects columns, so downstream plotting/tables treat it as a
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

  # Drop the raw interaction cells from `effects`: they are reported per target
  # via `method_by_target`, and the pooled structure effect below replaces what
  # a s(Method) main effect used to contribute here.
  random_effects_joint <- extract_ranef_terms(m.fits_joint) |>
    filter(group_var != "Method:Epi_target") |>
    bind_rows(method_pooled_effects(m.fits_joint)) |>
    mutate(model = "Adjusted") |>
    bind_rows(extract_target_effect(m.fits_joint, "Adjusted"))

  random_effects <- random_effects_joint |>
    bind_rows(random_effects_uni)

  # Per-target structure effects, one row per Method x Epi_target cell. Kept out
  # of `effects` so tables that print every group_var do not double-count them
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

  dir.create(here(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

  saveRDS(results, here(output_dir, "results.rds"))

  # Observed vs fitted, for a model-fit diagnostic plot in the supplement.
  # NAs are filtered upstream, so m.data rows align 1:1 with the fitted values.
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
