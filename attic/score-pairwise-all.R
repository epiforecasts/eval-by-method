# Pairwise over time, horizon, location - each model rel to ensemble

quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)
scores_pairwise_date <- read_csv(here("data", "scores-pw-target-date.csv")) |>
  filter(!grepl("EuroCOVIDhub-ensemble", model))

by_method_date <- scores_pairwise_date |>
  left_join(metadata) |>
  filter(!grepl("Other", method_type) & !is.na(method_type))

plot_method_var <- by_method_date |>
  filter(scale == "log" & !is.infinite(rel_wis)) |>
  group_by(target_end_date, method_type) |>
  summarise(
    n = n(),
    value = quantile(rel_wis, quantiles),
    quantile = paste0("q", quantiles),
    .groups = "drop") |>
  pivot_wider(names_from = quantile) |>
  ggplot(aes(x = target_end_date, col = method_type, fill = method_type)) +
  geom_line(aes(y = q0.5), alpha = 0.3, lwd = 1) +
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75), alpha = 0.6, col = NA) +
  geom_ribbon(aes(ymin = q0.01, ymax = q0.99), alpha = 0.4, col = NA) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(y = NULL, x = NULL,
       col = "Method type",
       fill = "Method type") +
  scale_color_viridis_d(aesthetics = c("fill", "colour")) +
  theme(legend.position = "bottom")

plot_method_var

#


# -------------------------------------------------------------------------

# Get scores
scores_raw <- read_csv(here("data", "scores-raw.csv"))
scores_pairwise_all <- scores_raw |>
  select(location, forecast_date,
         target_end_date,
         model, horizon,
         interval_score) |>
  pairwise_comparison(
    metric = "interval_score",
    baseline = "EuroCOVIDhub-ensemble",
    by = c("target_end_date", "location", "horizon", "model"))
