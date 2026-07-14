# Sensitivity arm: log-transformed response.
#
# The primary model (R/analysis-model.R) fits gaussian(link = "log") on raw WIS.
# A log *link* models log(E[WIS]) but keeps additive Gaussian errors on the raw,
# right-skewed WIS scale, so observation-level residuals are strongly skewed.
#
# Here we instead transform the *response* (log(wis)) and use an identity link.
# log(WIS) is near-symmetric, so the Gaussian error assumption holds far better
# (residual skew/kurtosis improve markedly). This is the epi-analysis convention
# for scores ("log-transformed Gaussian"). We refit only the joint model — the
# univariate set is not needed for this diagnostic comparison.
#
# Outputs (for the supplement):
#   output/log-resp/plots/check_joint.pdf  -- gratia::appraise diagnostics
#   output/log-resp/fit_obs.rds            -- observed vs fitted (log-WIS scale)
#   output/log-resp/kcheck.rds             -- mgcv::k.check basis adequacy

library(here)
library(dplyr)
library(mgcv)
library(gratia)
library(ggplot2)
source(here("R", "analysis-model.R")) # for the shared m.formula_joint RHS

model_wis_logresp <- function(output_dir = here("output", "log-resp")) {
  dir.create(file.path(output_dir, "plots"),
             recursive = TRUE, showWarnings = FALSE)

  m.data <- process_data(scoring_scale = "log") |>
    filter(!grepl("EuroCOVIDhub-", Model)) |>
    filter(!is.na(wis)) |>
    mutate(
      Epi_target = as.factor(epi_target),
      Incidence = log(Incidence + 1),
      # 0.3% of WIS are ~0 (perfect forecasts); floor before log so it is finite.
      wis_log = log(pmax(wis, 1e-4))
    )

  message("--------fitting log-response joint model")
  fit <- bam(
    formula = update(m.formula_joint, wis_log ~ .),
    data = m.data,
    family = gaussian(),
    method = "fREML",
    control = gam.control(trace = TRUE),
    discrete = TRUE
  )

  stopifnot(nrow(m.data) == length(fit$y))

  ggsave(file.path(output_dir, "plots", "check_joint.pdf"), appraise(fit))
  saveRDS(
    tibble::tibble(
      observed = fit$y,
      fitted = fitted(fit),
      epi_target = m.data$epi_target
    ),
    file.path(output_dir, "fit_obs.rds")
  )
  saveRDS(k.check(fit), file.path(output_dir, "kcheck.rds"))

  invisible(fit)
}
