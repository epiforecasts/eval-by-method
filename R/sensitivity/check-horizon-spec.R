# Sensitivity: how the model x horizon interaction is specified.
#
# Reduced model, LWIS ~ outcome + model + horizon, so the horizon terms can be
# compared without the rest of the joint specification. Each arm is a valid
# way to give every model its own horizon curve (or not, for the additive
# arm). The primary model uses arm "by-model": `bs = "sz"` is written there,
# but mgcv ignores it when the factor is passed through `by =` and builds
# ordinary thin-plate smooths, so the arm is fitted without it.
#
# Each fit is summarised to a small list (fit statistics and a prediction grid)
# and cached to output/sensitivity/horizon-spec/<label>.rds, so a crash costs
# one fit. Delete a cache file to refit that arm.
#
# Run detached (fits take minutes each):
#   nohup caffeinate -is Rscript -e \
#     'source(here::here("R", "sensitivity", "check-horizon-spec.R")); fit_horizon_specs()' \
#     > output/logs/horizon-spec_$(date +%F-%H%M).log 2>&1 &

library(here)
library(dplyr)
library(purrr)
library(mgcv)

horizon_specs <- list(
  "additive" = wis ~ Epi_target + s(Model, bs = "re") + s(Horizon, k = 3),
  "by-model" = wis ~ Epi_target + s(Model, bs = "re") +
    s(Horizon, by = Model, k = 3),
  "sz" = wis ~ Epi_target + s(Horizon, k = 3) +
    s(Model, Horizon, k = 3, bs = "sz"),
  "sz + model re" = wis ~ Epi_target + s(Model, bs = "re") + s(Horizon, k = 3) +
    s(Model, Horizon, k = 3, bs = "sz"),
  "fs" = wis ~ Epi_target + s(Horizon, k = 3) +
    s(Horizon, Model, k = 3, bs = "fs"),
  "factor re" = wis ~ Epi_target + HorizonF + s(Model, bs = "re") +
    s(Model, HorizonF, bs = "re")
)

horizon_spec_dir <- here("output", "sensitivity", "horizon-spec")

# Same data as the primary fit: the saved results carry the fitted data.
load_horizon_data <- function() {
  readRDS(here("output", "log", "results.rds"))$data |>
    filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
    select(wis, Model, Horizon, Epi_target) |>
    mutate(Model = droplevels(Model), HorizonF = factor(Horizon))
}

fit_horizon_spec <- function(label, data) {
  path <- file.path(horizon_spec_dir, paste0(gsub("[^a-z0-9]+", "-", label), ".rds"))
  if (file.exists(path)) {
    message("cached: ", label)
    return(readRDS(path))
  }
  message("-------- fitting: ", label)
  time <- system.time(
    fit <- bam(horizon_specs[[label]], data = data, family = tw(link = "log"),
               method = "fREML", discrete = TRUE)
  )[["elapsed"]]

  grid <- expand.grid(Model = levels(data$Model), Horizon = 1:4,
                      Epi_target = levels(data$Epi_target)) |>
    mutate(HorizonF = factor(Horizon, levels = levels(data$HorizonF)))
  pred <- predict(fit, grid, type = "link", se.fit = TRUE)

  out <- list(
    label = label,
    formula = deparse1(horizon_specs[[label]]),
    time_s = time,
    edf = sum(fit$edf),
    dev_expl = summary(fit)$dev.expl,
    tweedie_p = fit$family$getTheta(TRUE),
    converged = fit$converged,
    grid = mutate(grid, eta = pred$fit, se = pred$se.fit)
  )
  dir.create(horizon_spec_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, path)
  rm(fit)
  gc()
  out
}

fit_horizon_specs <- function(labels = names(horizon_specs)) {
  data <- load_horizon_data()
  map(set_names(labels), \(label) fit_horizon_spec(label, data))
}

# Read whatever arms are cached, without fitting.
read_horizon_specs <- function() {
  files <- list.files(horizon_spec_dir, pattern = "\\.rds$", full.names = TRUE)
  res <- map(files, readRDS)
  res <- set_names(res, map_chr(res, "label"))
  res[intersect(names(horizon_specs), names(res))]
}
