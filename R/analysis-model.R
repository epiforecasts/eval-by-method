# Aim: use a GAMM to model the effects of model structure and country target type on WIS
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(mgcv)
library(gammit)
library(gratia)
source(here("R", "process-data.R"))
source(here("R", "analysis-descriptive.R"))

# --- Get data ---
data <- process_data(scoring_scale = "log")
m.data <- data |>
  filter(!grepl("EuroCOVIDhub-", Model))
outcomes <- unique(data$outcome_target)

# --- Model formula ---
# Univariate for each explanatory variable
m.formulas_uni <- list(
  method = wis ~ s(Method, bs = "re"),
  target = wis ~ s(CountryTargets, bs = "re"),
  trend = wis ~ s(Trend, bs = "re"),
  location = wis ~ s(Location, bs = "re"),
  variant = wis ~ s(VariantPhase, k = 6, bs = "re"),
  horizon = wis ~ s(Horizon, k = 3, by = Model, bs = "sz"),
  model = wis ~ s(Model, bs = "re")
)

# Full model
m.formula_joint <- wis ~
  # Method
  s(Method, bs = "re") +
  # Number of target countries
  s(CountryTargets, bs = "re") +
  # -----------------------------
  # Trend
  s(Trend, bs = "re") +
  # Location
  s(Location, bs = "re") +
  # Variant phase
  s(VariantPhase, k = 6, bs = "re") +
  # Horizon
  s(Horizon, k = 3, by = Model, bs = "sz") +
  # Individual model
  s(Model, bs = "re")

# --- Model fitting ---
# Set up to fit to each outcome target (cases, deaths)
m.fit <- function(outcomes, m.formula) {
  outcomes |>
  set_names() |>
  map(\(outcome) {
    bam(
      formula = m.formula,
      data = m.data |> filter(outcome_target == outcome),
      family = gaussian(link = "log"),
      method = "fREML",
      control = gam.control(trace = TRUE),
      discrete = TRUE
    )
  })
}
# Fit
cat("--------fitting univariate models")
m.fits_uni <- map(m.formulas_uni, ~ m.fit(outcomes, .x))

cat("--------fitting joint model")
m.fits_joint <- m.fit(outcomes, m.formula_joint)
cat("finished fitting")

# --- Output handling ---
# Extract estimates for random effects
random_effects_uni <- m.fits_uni[!grepl("horizon", names(m.fits_uni))] |>
  map_depth(.depth = 2, ~ extract_ranef(.x)) |>
  map(~ list_rbind(.x, names_to = "outcome_target")) |>
  list_rbind() |>
  mutate(model = "Unadjusted")

random_effects_joint <- map_df(m.fits_joint,
                         extract_ranef,
                        .id = "outcome_target") |>
  mutate(model = "Adjusted")

random_effects <- random_effects_joint |>
  bind_rows(random_effects_uni)

# Extract model checks
checks <- map(m.fits_joint, k.check)
formula <- m.fits_joint[[1]]$formula
results <- list(
  effects = random_effects,
  checks = checks,
  formula = formula
)

saveRDS(results, here("output", "results.rds"))

iwalk(m.fits_joint, \(x, target) {
  p <- appraise(x)
  ggsave(here("plots", paste0("check_", target, ".pdf")), p)
})
