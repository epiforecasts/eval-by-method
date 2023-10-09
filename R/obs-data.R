# Additional explanatory variables
# Incidence level over time
# Absolute number of deaths
# Relative to mean - standardised z-score
# Relative to peak

# Get observed data
obs <- import_obs()
obs <- obs |>
  group_by(location) |>
  mutate(z_score = scale(true_value, center = TRUE, scale = TRUE))

# Forecasts - to check for anomaly removal
forecasts <- arrow::read_parquet(here("data", "forecasts.parquet"))
forecasts <- join_obs(forecasts, remove_anomalies = TRUE)

# New variant introduction phases

# Growth rate

