# Sensitivity: structure-by-outcome effects under a different error family.
#
# The primary fit uses a Tweedie family (see check-family.R). The direction of
# the s(Method, Epi_target) cells is stable across families but their magnitude
# is not, so the supplement reports the Gaussian arm alongside the primary fit.
# This script refits the primary specification with gaussian(link = "log") and
# writes the cells to output/diagnostics/method-by-target-gaussian.csv, so the
# supplement can read the numbers rather than hardcoding them.
#
# Run: source(here::here("R", "sensitivity", "check-family-interaction.R")); check_family_interaction()

library(here)
library(dplyr)
library(readr)
library(mgcv)
source(here("R", "analysis-model.R")) # m.formula_joint
source(here("R", "utils-effects.R"))  # method_target_effects()

check_family_interaction <- function(
  family = gaussian(link = "log"),
  label = "gaussian-log",
  out_file = here("output", "diagnostics", "method-by-target-gaussian.csv")
) {
  m.data <- process_data(scoring_scale = "log") |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    filter(!is.na(wis)) |>
    mutate(
      Epi_target = as.factor(epi_target),
      Incidence = log(Incidence + 1)
    )

  message("-------- fitting interaction under family: ", label)
  fit <- bam(
    formula = m.formula_joint,
    data = m.data,
    family = family,
    method = "fREML",
    discrete = TRUE
  )

  cells <- method_target_effects(fit) |>
    mutate(family = label, .before = everything())

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  write_csv(cells, out_file)
  message("   written to ", out_file)
  return(cells)
}
