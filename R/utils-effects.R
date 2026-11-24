# Extract random-effect coefficients from an mgcv fit.
#
# Replaces gammit::extract_ranef(), which cannot handle a factor-by-factor
# random effect such as s(Method, Epi_target, bs = "re"). gammit takes only the
# last variable name of an interaction term and looks up that factor's levels,
# so it collects 5 labels for a term carrying 10 coefficients and then fails
# with a recycling error -- which takes down extraction for every term in the
# fit, not just the interaction.
#
# For a bs = "re" smooth, mgcv builds the design matrix from a formula stored on
# the smooth object (`$form`, e.g. `~Method:Epi_target - 1`), so its columns map
# one-to-one onto the coefficient block `$first.para:$last.para`. Rebuilding that
# model matrix and reading its column names therefore recovers the level labels
# exactly, in the right order, without assuming how mgcv orders interactions.
#
# Output matches the columns downstream code expects from gammit:
# group_var, effect, group, value, se, lower_*, upper_*.

library(dplyr)
library(purrr)
library(tibble)

# "MethodStatistical" -> "Statistical"
# "MethodStatistical:Epi_targetCases" -> "Statistical:Cases"
strip_level_labels <- function(labels, varnames) {
  map_chr(strsplit(labels, ":", fixed = TRUE), function(parts) {
    cleaned <- map_chr(parts, function(part) {
      hit <- varnames[startsWith(part, varnames)]
      if (length(hit) == 0) {
        return(part)
      }
      # longest match, so one variable name being a prefix of another
      # (e.g. "Model" and "ModelType") cannot strip the wrong one
      sub(paste0("^", hit[which.max(nchar(hit))]), "", part)
    })
    paste(cleaned, collapse = ":")
  })
}

# Locate the s(Method, Epi_target) random effect and describe its cells.
#
# A separate s(Method) main effect is deliberately NOT in the specification:
# mgcv's bs = "re" interaction is an unconstrained zero-mean prior over all
# Method x Epi_target cells, so its target-average is exactly what a Method main
# effect would represent. With both terms present and both penalised, the split
# between them is set by the relative variance estimates rather than by the
# data, and mgcv shrank the main effect to 0.001 edf while the interaction took
# 4.9. Dropping it removes the aliasing and leaves the cells interpretable.
method_target_cells <- function(fit) {
  re_smooths <- keep(fit$smooth, \(s) inherits(s, "random.effect"))
  is_inter <- map_lgl(
    re_smooths,
    \(s) identical(sort(all.vars(s$form)), sort(c("Method", "Epi_target")))
  )
  if (!any(is_inter)) {
    stop("Fit must contain an s(Method, Epi_target) term.")
  }
  inter <- re_smooths[[which(is_inter)[1]]]
  labels <- strip_level_labels(
    colnames(model.matrix(inter$form, fit$model)), all.vars(inter$form)
  )
  idx <- inter$first.para:inter$last.para
  stopifnot(length(labels) == length(idx))

  # Labels read "Method:Target" or "Target:Method" depending on the order mgcv
  # wrote the formula, so resolve each part against the known level sets rather
  # than assuming a position.
  methods <- levels(droplevels(fit$model$Method))
  parts <- strsplit(labels, ":", fixed = TRUE)
  cells <- tibble(
    index = idx,
    Method = map_chr(parts, \(p) p[p %in% methods][1]),
    Epi_target = map_chr(parts, \(p) p[!p %in% methods][1])
  )
  stopifnot(!anyNA(cells$Method), !anyNA(cells$Epi_target))
  cells
}

# Evaluate a set of contrast vectors against the fitted coefficients and their
# covariance: value = c'b, se = sqrt(c' Vp c). Using the full covariance matters
# whenever a contrast spans more than one coefficient, as the pooled effect does.
contrast_effects <- function(fit, contrasts, ci_level = 0.95, digits = 3) {
  gam_coef <- coef(fit)
  Vp <- fit$Vp
  mult <- stats::qnorm(1 - (1 - ci_level) / 2)
  map(contrasts, function(cvec) {
    tibble(
      value = as.numeric(cvec %*% gam_coef),
      se = sqrt(as.numeric(t(cvec) %*% Vp %*% cvec))
    )
  }) |>
    list_rbind() |>
    mutate(
      !!paste0("lower_", (1 - ci_level) / 2 * 100) := value - mult * se,
      !!paste0("upper_", (1 - (1 - ci_level) / 2) * 100) := value + mult * se
    ) |>
    mutate(across(where(is.numeric), \(x) round(x, digits)))
}

# Per-target partial effect of each model structure: one cell of the
# s(Method, Epi_target) term.
method_target_effects <- function(fit, ci_level = 0.95, digits = 3) {
  cells <- method_target_cells(fit)
  n_coef <- length(coef(fit))
  contrasts <- map(cells$index, function(i) {
    cvec <- numeric(n_coef)
    cvec[i] <- 1
    cvec
  })
  bind_cols(
    select(cells, Method, Epi_target),
    contrast_effects(fit, contrasts, ci_level, digits)
  ) |>
    mutate(group_var = "Method_by_target",
           group = paste(Method, Epi_target, sep = ":"))
}

# Pooled effect of each model structure, averaging over epidemiological targets.
# This is a contrast across the two cells, not a separate model term, so its
# interval accounts for the covariance between them.
method_pooled_effects <- function(fit, ci_level = 0.95, digits = 3) {
  cells <- method_target_cells(fit)
  n_coef <- length(coef(fit))
  methods <- unique(cells$Method)
  contrasts <- map(methods, function(m) {
    rows <- cells$index[cells$Method == m]
    cvec <- numeric(n_coef)
    cvec[rows] <- 1 / length(rows)
    cvec
  })
  bind_cols(
    tibble(group_var = "Method", effect = "Intercept", group = methods),
    contrast_effects(fit, contrasts, ci_level, digits)
  )
}

extract_ranef_terms <- function(fit, ci_level = 0.95, digits = 3) {
  re_smooths <- keep(fit$smooth, \(s) inherits(s, "random.effect"))
  if (length(re_smooths) == 0) {
    stop("No random-effect smooths in this fit.")
  }

  gam_coef <- coef(fit)
  gam_se <- sqrt(diag(fit$Vp))
  mult <- stats::qnorm(1 - (1 - ci_level) / 2)
  lower_name <- paste0("lower_", (1 - ci_level) / 2 * 100)
  upper_name <- paste0("upper_", (1 - (1 - ci_level) / 2) * 100)

  re_smooths |>
    map(function(s) {
      idx <- s$first.para:s$last.para
      # `fit$model` is the model frame, so factor levels match those actually fitted
      labels <- colnames(model.matrix(s$form, fit$model))
      # If this ever trips, mgcv's design-matrix construction has changed and the
      # coefficient-to-level mapping can no longer be trusted.
      stopifnot(length(labels) == length(idx))

      varnames <- all.vars(s$form)
      tibble(
        group_var = paste(varnames, collapse = ":"),
        effect = "Intercept",
        group = strip_level_labels(labels, varnames),
        value = unname(gam_coef[idx]),
        se = unname(gam_se[idx])
      ) |>
        mutate(
          !!lower_name := value - mult * se,
          !!upper_name := value + mult * se
        )
    }) |>
    list_rbind() |>
    mutate(across(where(is.numeric), \(x) round(x, digits)))
}

# Per-model effect under s(Horizon, Model, bs = "fs"): each model's deviation
# from the shared horizon curve, averaged over horizons. There is no separate
# s(Model) term, because the fs term already carries each model's level.
#
# `horizons = NULL` averages over the horizons each model actually forecast, with
# equal weight per horizon, so a model is never judged on horizons it did not
# forecast. A fixed vector (e.g. 1) evaluates every model at those horizons.
# Each effect is a linear contrast of the fs coefficients, so its interval uses
# the full posterior covariance.
model_horizon_effects <- function(fit, horizons = NULL, ci_level = 0.95,
                                  digits = 3) {
  fs <- keep(fit$smooth, \(s) inherits(s, "fs.interaction"))
  if (length(fs) != 1) {
    stop("Fit must contain one s(Horizon, Model, bs = \"fs\") term.")
  }
  fs <- fs[[1]]
  idx <- fs$first.para:fs$last.para
  models <- levels(droplevels(fit$model$Model))

  grid <- if (is.null(horizons)) {
    distinct(fit$model, Model, Horizon)
  } else {
    expand.grid(Model = models, Horizon = horizons)
  }
  grid <- mutate(grid, Model = factor(Model, levels = levels(fit$model$Model)))
  X <- PredictMat(fs, grid)

  n_coef <- length(coef(fit))
  contrasts <- map(models, function(m) {
    cvec <- numeric(n_coef)
    cvec[idx] <- colMeans(X[grid$Model == m, , drop = FALSE])
    cvec
  })
  bind_cols(
    tibble(group_var = "Model", effect = "Intercept", group = models),
    contrast_effects(fit, contrasts, ci_level, digits)
  )
}

# Mean row of the linear-predictor matrix over each outcome's own forecasts,
# as a named list (Cases, Deaths). Rows are processed in chunks to limit memory.
# Used to centre the per-outcome smooths and to average the outcome contrast.
outcome_mean_rows <- function(fit, data, chunk = 20000) {
  map(c(Cases = "Cases", Deaths = "Deaths"), \(o) {
    d <- data[data$Epi_target == o, ]
    rows <- split(seq_len(nrow(d)), ceiling(seq_len(nrow(d)) / chunk))
    total <- Reduce(`+`, map(rows, \(i) {
      colSums(predict(fit, newdata = d[i, ], type = "lpmatrix"))
    }))
    total / nrow(d)
  })
}

# Incidence and growth enter as one smooth per outcome (by = Epi_target), and
# mgcv centres a factor-by smooth over the pooled covariate values of both
# outcomes, not within its own. Death counts sit far below case counts, so the
# Epi_target coefficient alone compares the outcomes at an incidence deaths
# rarely reach, and each raw curve carries a level shared with the intercept.
# The two helpers below re-express both against each outcome's own forecasts.

# Deaths-versus-cases contrast averaged over each outcome's own forecasts: the
# outcome coefficient plus each outcome's incidence and growth smooths averaged
# over its own forecasts (other terms held fixed).
outcome_average_contrast <- function(fit, mean_rows, ci_level = 0.95,
                                     digits = 3) {
  cols <- grepl("^Epi_target|^s\\((Incidence|Growth)\\)", names(coef(fit)))
  cvec <- mean_rows$Deaths - mean_rows$Cases
  cvec[!cols] <- 0
  bind_cols(
    tibble(group_var = "Epi_target", group = "Deaths"),
    contrast_effects(fit, list(cvec), ci_level, digits)
  )
}

# Each outcome's incidence and growth curve relative to that outcome's average
# forecast, over its own observed range, on the link (log) scale. Each point
# is a contrast against the outcome's mean row on that smooth's columns, so the
# interval excludes the level shared with the intercept. Same columns as
# gratia::smooth_estimates(): .smooth, Epi_target, the covariate, .estimate, .se.
outcome_smooth_curves <- function(fit, data, mean_rows,
                                  covariates = c("Incidence", "Growth"),
                                  n = 200) {
  b <- coef(fit)
  expand_grid(covariate = covariates, Epi_target = c("Cases", "Deaths")) |>
    pmap(\(covariate, Epi_target) {
      label <- paste0("s(", covariate, "):Epi_target", Epi_target)
      cols <- startsWith(names(b), paste0(label, "."))
      own <- data[data$Epi_target == Epi_target, ]
      x <- seq(min(own[[covariate]]), max(own[[covariate]]), length.out = n)
      grid <- own[rep(1, n), ]
      grid[[covariate]] <- x
      X <- predict(fit, newdata = grid, type = "lpmatrix")[, cols, drop = FALSE]
      C <- sweep(X, 2, mean_rows[[Epi_target]][cols])
      tibble(.smooth = label, Epi_target = Epi_target, !!covariate := x,
             .estimate = as.numeric(C %*% b[cols]),
             .se = sqrt(rowSums((C %*% fit$Vp[cols, cols]) * C)))
    }) |>
    list_rbind()
}
