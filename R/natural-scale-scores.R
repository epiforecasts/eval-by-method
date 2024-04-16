## Scores on the natural scale ----------------------------------
# Scores on the natural scale are in pairwise comparison relative to the Hub ensemble
quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)
# Pairwise scores
scores_pairwise_horizon <- read_csv(here("data",
                                         "scores-pw-horizon.csv"))
metadata <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification)
# Set up method types
method_factor <- c("Qualitative",
                   "Mechanistic", "Semi-mechanistic", "Statistical")
names(method_factor) <- method_factor

# Get scores by method type and horizon
scores_method <- scores_pairwise_horizon |>
  filter(!grepl("EuroCOVIDhub-ensemble", model)) |>
  left_join(metadata, by = "model") |>
  mutate(method_type = factor(method_type,
                              method_factor, names(method_factor),
                              ordered = TRUE))

# Summarise by quantiles across Stat/Semi-/Mech models, point-score for others
scores_method_all <- scores_method |>
  filter(!grepl("Qualitative", method_type)) |>
  group_by(method_type, horizon, scale) |>
  summarise(
    n = n(),
    value = quantile(rel_wis, quantiles),
    quantile = paste0("q", quantiles),
    .groups = "drop")
scores_method_other <- scores_method |>
  filter(grepl("Qualitative", method_type)) |>
  select(model, method_type, horizon, value = rel_wis) |>
  mutate(quantile = "q0.5", n = 1)

# Plot
nat_plot_method_target <- scores_method_all |>
  bind_rows(scores_method_other) |>
  filter(scale == "natural") |>
  pivot_wider(names_from = quantile) |>
  mutate(horizon = as.factor(horizon)) |>
  ggplot(aes(y = method_type, col = horizon, fill = horizon)) +
  geom_point(aes(x = q0.5), alpha = 0.8, position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.25, xmax = q0.75), linewidth = 4,
                 alpha = 0.5, position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.01, xmax = q0.99), linewidth = 4,
                 alpha = 0.2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1, lty = 2) +
  labs(y = NULL, x = NULL,
       col = "Week ahead forecast horizon",
       fill = "Week ahead forecast horizon") +
  scale_color_viridis_d(direction = 1) +
  theme(legend.position = "bottom")

nat_plot_method_target
