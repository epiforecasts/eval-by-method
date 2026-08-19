# Sensitivity: choice of error family for the log-scale (primary) GAMM.
#
#
# Two considerations:
#
#   1. WIS on the log scale is continuous, positive, and strongly right-skewed.
#      Gamma and Tweedie are the natural candidates. Symmetric heavy-tailed
#      families (e.g. scat()) are deliberately excluded: they could only
#      downweight the tail, not represent the skew, and they put support on
#      negative values, which is wrong for a strictly positive score.
#
#   2. 553 forecasts (0.27%) have WIS exactly 0, as perfect predictions of
#      zero-incidence targets, almost all deaths in small countries (Iceland,
#      Liechtenstein, Malta). Gamma has no support at zero, so those arms add a
#      1e-7 constant to every score, which parks the zeros at log(1e-7) = -16.1,
#      roughly 11 log-units below the next smallest score.
#
#      Tweedie with 1 < p < 2 has a genuine point mass at zero, so it models
#      those forecasts as what they are. The primary specification therefore
#      keeps the exact zeros, and the "tweedie-offset" arm shows what adding the
#      constant back would cost.
# Using gaussian(link = "log") leaves strongly skewed
# deviance residuals (skew ~5.8, kurtosis ~77), which is a poor description of
# the outcome.
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

# Candidate families. `offset` records whether a 1e-7 constant is added to every
# score for that arm, displacing the exact zeros. It is required for Gamma,
# which has no support at zero, and retained for the Gaussian arm so the two
# comparison families see the same response. The Tweedie arms are fitted both
# ways to show what the constant costs.
.family_candidates <- list(
  list(label = "gaussian-log",    family = quote(gaussian(link = "log")), offset = TRUE),
  list(label = "gamma-log",       family = quote(Gamma(link = "log")),    offset = TRUE),
  list(label = "tweedie-log",     family = quote(tw(link = "log")),       offset = FALSE),
  list(label = "tweedie-offset",  family = quote(tw(link = "log")),       offset = TRUE)
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
    if (cand$offset) {
      # Displace the exact zeros so families without support at zero can fit.
      dat <- mutate(dat, wis = wis + 1e-7)
    }

    # bam() signals non-convergence through a warning
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
      "   skew ",
      signif(as.numeric(row$resid_skew), 3),
      "  kurtosis ",
      signif(as.numeric(row$resid_kurtosis), 3),
      "  converged: ",
      converged
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
