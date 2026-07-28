# Sensitivity: choice of error family for the log-scale (primary) GAMM.
#
# The primary model used gaussian(link = "log"). That leaves strongly skewed
# deviance residuals (skew ~5.8, kurtosis ~77), which is a poor description of
# the outcome rather than a cosmetic problem. Modelling log(WIS) directly would
# fix the residuals but loses propriety of the score, so the fix has to come
# from the error family instead (issue #159).
#
# Two things shape the choice:
#
#   1. WIS on the log scale is continuous, positive, and strongly right-skewed.
#      Gamma and Tweedie are the natural candidates. Symmetric heavy-tailed
#      families (e.g. scat()) are deliberately excluded: they could only
#      downweight the tail, not represent the skew, and they put support on
#      negative values, which is wrong for a strictly positive score.
#
#   2. 553 forecasts (0.27%) have WIS exactly 0 -- perfect predictions of
#      zero-incidence targets, almost all deaths in small countries (Iceland,
#      Liechtenstein, Malta). process-data.R adds 1e-7 to every score so these
#      are representable on a log link, which parks them at log(1e-7) = -16.1,
#      roughly 11 log-units below the next smallest score. That is a spike of
#      extreme leverage created by an arbitrary constant.
#
#      Tweedie with 1 < p < 2 has a genuine point mass at zero, so it can model
#      those forecasts as what they are instead of displacing them. The
#      "tweedie-nooffset" arm therefore removes the 1e-7 and keeps the exact
#      zeros.
#
# Run: source(here::here("R", "sensitivity", "check-family.R")); check_family()

library(here)
library(dplyr)
library(readr)
library(purrr)
library(mgcv)
library(ggplot2)
library(gratia)
source(here("R", "analysis-model.R")) # m.formula_joint, archive_diagnostics()

# Candidate families. `offset` records whether the 1e-7 added in process-data.R
# is retained; the no-offset arm is only meaningful for a family that admits
# exact zeros.
.family_candidates <- list(
  list(label = "gaussian-log", family = quote(gaussian(link = "log")), offset = TRUE),
  list(label = "gamma-log",    family = quote(Gamma(link = "log")),    offset = TRUE),
  list(label = "tweedie-log",  family = quote(tw(link = "log")),       offset = TRUE),
  list(label = "tweedie-nooffset", family = quote(tw(link = "log")),   offset = FALSE)
)

check_family <- function(candidates = .family_candidates,
                         spec_prefix = "family") {
  m.data <- process_data(scoring_scale = "log") |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    filter(!is.na(wis)) |>
    mutate(
      Epi_target = as.factor(epi_target),
      Incidence = log(Incidence + 1)
    )

  results <- map(candidates, \(cand) {
    message("-------- fitting family: ", cand$label)
    dat <- m.data
    if (!cand$offset) {
      # Undo the constant added in process-data.R, restoring the exact zeros.
      dat <- mutate(dat, wis = pmax(wis - 1e-7, 0))
    }

    # bam() signals non-convergence through a warning, not an error, so capture
    # it rather than let it scroll past in the fitting trace.
    warnings_seen <- character()
    fit <- withCallingHandlers(
      bam(
        formula = m.formula_joint,
        data = dat,
        family = eval(cand$family),
        method = "fREML",
        discrete = TRUE
      ),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    p <- appraise(fit)
    row <- archive_diagnostics(
      fit,
      spec_label = paste(spec_prefix, cand$label, sep = "-"),
      scoring_scale = "log",
      plot = p
    )

    converged <- !any(grepl("did not converge", warnings_seen))
    message(
      "   skew ", signif(as.numeric(row$resid_skew), 3),
      "  kurtosis ", signif(as.numeric(row$resid_kurtosis), 3),
      "  converged: ", converged
    )
    if (length(warnings_seen)) {
      message("   warnings: ", paste(unique(warnings_seen), collapse = "; "))
    }

    tibble::tibble(
      label = cand$label,
      offset = cand$offset,
      converged = converged,
      warnings = paste(unique(warnings_seen), collapse = "; ")
    )
  })

  # AIC is comparable only within an offset arm: the no-offset fit has a
  # different response vector, so its likelihood is on a different scale.
  bind_rows(results)
}
