# Coding epidemic phases

# Rise: linear, exponential
# Fall
# Stable
# New variant introduction (0-25%)
# New variant takeover (25-75%)
#
# By growth rate or R
# Get R estimate for each country using national ?case?death data
# Use as predictive var for forecast performance against ?? baseline
# Group by model type / location



scores_raw <- read_csv(here("data", "scores-raw.csv"))

gr <- read_csv(here("data", "gr-epiforecasts-covid.csv")) |>
  rename(target_end_date = date, location_name = country) |>
  filter(between(target_end_date,
                 min(scores_raw$target_end_date),
                 min(scores_raw$target_end_date)+365))
plot_gr <- gr |>
  ggplot(aes(x = target_end_date)) +
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20), alpha = 0.8, col = NA) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.5, col = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = NULL) +
  facet_grid(rows = vars(location_name), scales = "free") +
  theme(legend.position = "bottom")

ggsave(filename = here("output", "gr.pdf"),
       plot = plot_gr,
       height = 30, width = 5)
