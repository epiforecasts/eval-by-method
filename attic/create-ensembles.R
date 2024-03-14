# Create new ensembles from each method type
library(here)
library(dplyr)

# Criteria for forecasts entering an ensemble
# - 23 quantiles (pre-filtered in `import-data.R`)
# - 4 horizons
# - At least 3 models

create_ensembles <- function(forecasts) {
  # Load forecasts -------------------------------
  # check how many weeks included in each forecast
  horizons <- forecasts |>
    filter(quantile == 0.5) |>
    group_by(forecast_date, location, model) |>
    summarise(n = n())
  # keep only forecasts with 1:4 weeks
  forecasts_4wk <- filter(horizons, n == 4) |>
    select(-n) |>
    left_join(forecasts)

  # Get method types --------------------------
  metadata <- read_csv(here("data", "model-classification.csv")) |>
    select(model, method_type = classification)
  # join to forecasts
  forecasts_4wk_method <- left_join(forecasts_4wk, metadata) |>
    filter(!grepl("ABM|Qualitative", method_type) &
             !grepl("EuroCOVIDhub-ensemble", model))

  # Create each ensemble ------------------
  # median
  ensembles_median <- forecasts_4wk_method |>
    group_by(forecast_date, target_end_date,
             method_type, location, quantile) |>
    summarise(prediction = median(prediction, na.rm = TRUE),
              n = n(),
              ensemble_type = "EuroCOVIDhub-ensemble_median",
              .groups = "drop")
  # mean
  ensembles_mean <- forecasts_4wk_method |>
    group_by(forecast_date, target_end_date,
             method_type, location, quantile) |>
    summarise(prediction = mean(prediction, na.rm = TRUE),
              n = n(),
              ensemble_type = "EuroCOVIDhub-ensemble_mean",
              .groups = "drop")

  # Combine ----------------
  ensembles <- ensembles_median |>
    bind_rows(ensembles_mean) |>
    filter(n >= 3) |>
    mutate(model = paste0(ensemble_type, "_", method_type),
           horizon = as.numeric(target_end_date - forecast_date + 1) / 7) |>
    select(-c(ensemble_type, method_type, n))

  return(ensembles)
}
