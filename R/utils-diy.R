# Refit the GAMM under your own specification and compare effects across fits.
# Used by R/diy-model.qmd.
#
# Data comes from the saved primary fit (output/<scale>/results.rds), which is
# already filtered and transformed as in model_wis(), so nothing is reprocessed.
# Fits are summarised to effects plus fit statistics and cached to
# output/diy/<label>_<scale>-<frac>.rds. A cache is reused only while its
# formula and family match, so editing a spec under the same label refits it.

library(here)
library(dplyr)
library(purrr)
library(mgcv)
library(ggplot2)
source(here("R", "utils-effects.R"))

diy_dir <- here("output", "diy")

# Subsample rows within each model, so every model keeps some forecasts.
# At frac = 0.25 the Method x Epi_target variance collapses towards zero and
# every structure effect shrinks to ~0; at 0.5 the estimates match the paper's
# in sign and size (about 50 s per joint fit, against 100 s for frac = 1).
# wis_offset adds a constant to every score, for families with no support at
# zero (as in R/sensitivity/check-family.R).
load_diy_data <- function(scale = "log", frac = 0.5, seed = 1,
                          wis_offset = 0) {
  data <- readRDS(here("output", scale, "results.rds"))$data
  if (frac < 1) {
    set.seed(seed)
    data <- data |>
      group_by(Model) |>
      slice_sample(prop = frac) |>
      ungroup()
  }
  data <- mutate(data, across(where(is.factor), droplevels),
                 wis = wis + wis_offset)
  attr(data, "diy_key") <- paste0(scale, "-", frac,
                                  if (wis_offset != 0) "-offset")
  data
}

# Effects in the same shape as results.rds$effects. Method is pooled from the
# Method x Epi_target cells, which are also kept as group_var
# "Method_by_target"; Model comes from the fs horizon term, when present.
diy_effects <- function(fit) {
  is_re <- map_lgl(fit$smooth, \(s) inherits(s, "random.effect"))
  is_fs <- map_lgl(fit$smooth, \(s) inherits(s, "fs.interaction"))
  has_method_target <- any(map_lgl(
    fit$smooth[is_re],
    \(s) setequal(all.vars(s$form), c("Method", "Epi_target"))
  ))

  effects <- if (any(is_re)) extract_ranef_terms(fit) else tibble()
  if (has_method_target) {
    effects <- effects |>
      filter(group_var != "Method:Epi_target") |>
      bind_rows(method_pooled_effects(fit),
                select(method_target_effects(fit), -Method, -Epi_target))
  }
  if (any(is_fs)) {
    effects <- bind_rows(effects, model_horizon_effects(fit))
  }
  effects
}

fit_spec <- function(label, formula, data, family = tw(link = "log")) {
  key <- paste(label, attr(data, "diy_key"), sep = "_")
  path <- file.path(diy_dir, paste0(gsub("[^A-Za-z0-9.]+", "-", key), ".rds"))
  spec <- paste(deparse1(formula), family$family, family$link)
  if (file.exists(path)) {
    cached <- readRDS(path)
    if (identical(cached$spec, spec)) {
      message("cached: ", label)
      return(cached)
    }
  }
  message("-------- fitting: ", label)
  time <- system.time(
    fit <- bam(formula, data = data, family = family,
               method = "fREML", discrete = TRUE)
  )[["elapsed"]]

  out <- list(
    label = label,
    spec = spec,
    data = attr(data, "diy_key"),
    n = nrow(data),
    time_s = round(time),
    family = fit$family$family,
    dev_expl = summary(fit)$dev.expl,
    aic = AIC(fit),
    effects = mutate(diy_effects(fit), model = label)
  )
  dir.create(diy_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, path)
  out
}

# One row per fit. AIC is comparable only between fits on the same data
# and family.
spec_stats <- function(fits) {
  map(fits, \(f) tibble(label = f$label, data = f$data, n = f$n,
                        family = f$family, time_s = f$time_s,
                        dev_expl = round(f$dev_expl, 3), aic = round(f$aic))) |>
    list_rbind()
}

# Effects for each fit side by side, as WIS ratios against the average.
compare_specs <- function(fits, variables = "Method") {
  effects <- map(fits, "effects") |>
    list_rbind() |>
    filter(group_var %in% variables) |>
    mutate(model = factor(model, levels = rev(map_chr(fits, "label"))))

  ggplot(effects, aes(x = group, col = model)) +
    geom_point(aes(y = exp(value)), position = position_dodge(width = 0.7)) +
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5)),
                   position = position_dodge(width = 0.7),
                   lwd = 2, alpha = 0.5) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.25) +
    scale_y_log10() +
    facet_wrap(~group_var, scales = "free_y") +
    guides(col = guide_legend(reverse = TRUE)) +
    labs(y = "WIS ratio vs average", x = NULL, col = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom")
}
