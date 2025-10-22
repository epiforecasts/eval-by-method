# Score by variant phase

# Get scores
scores_raw <- read_csv(here("data", "scores-raw.csv"))

# Get variant introduction dates: 5-50%
source("https://gist.githubusercontent.com/kathsherratt/a534bff397f0824403a2e81ba83ddbb9/raw/7dd2f9f96f0edd8e55af2ff9a35b7900f4be8055/download_variant_introduction.R")
variant_names <- c("B.1.617.2" = "Delta",
                   "B.1.1.529" = "Omicron",
                   "BA.1" = "Omicron-BA1")

variant_data <- download_variant_introduction(variant_codes = names(variant_names),
                                              introduction_percent = 25)

delta <- variant_data |>
  filter(variant == "B.1.617.2") |>
  select(location_name = country,
         delta_intro = date_introduction,
         delta_dominant = date_dominant)

omicron <- variant_data |>
  filter(variant == "BA.1") |>
  select(location_name = country,
         omicron_intro = date_introduction,
         omicron_dominant = date_dominant)

locations <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
  select(location_name, location)

scores_raw_var <- scores_raw |>
  filter(scale == "log") |>
  # distinct(location, target_end_date) |>
  left_join(locations, by = "location") |>
  left_join(delta, by = "location_name") |>
  left_join(omicron, by = "location_name") |>
  mutate(variant = case_when(
    target_end_date <= delta_intro ~ "pre-delta",
    target_end_date >= delta_intro & target_end_date <= delta_dominant ~ "delta intro",
    target_end_date >= delta_dominant & target_end_date <= omicron_intro ~ "delta dominant",
    target_end_date >= omicron_intro & target_end_date <= omicron_dominant ~ "omicron intro",
    target_end_date >= omicron_dominant ~ "omicron dominant"
  )) |>
  filter(!is.na(variant))  # exclude countries with no variant data (UK, Swiss)

scores_pairwise_var <- scores_raw_var |>
  select(location, forecast_date,
         model, variant, horizon,
         interval_score) |>
  pairwise_comparison(
    metric = "interval_score",
    baseline = "EuroCOVIDhub-ensemble",
    by = c("variant", "horizon", "model"))

scores_pairwise_var <- scores_pairwise_var |>
  filter(compare_against == "EuroCOVIDhub-ensemble") |>
  select(model, variant, horizon,
         rel_wis = scaled_rel_skill) |>
  mutate(variant = factor(variant, levels = c("pre-delta",
                                              "delta intro",
                                              "delta dominant",
                                              "omicron intro",
                                              "omicron dominant")))

write_csv(variant_data, here("data", "variant_data.csv"))
write_csv(scores_pairwise_var, here("data", "scores-pairwise-rt.csv"))


# -------------------------------------------------------------------------
# visualise by method
metadata <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification)
targets_by_model <- read_csv(here("data", "targets-by-model.csv"))

by_method_var <- scores_pairwise_var |>
  left_join(metadata) |>
  filter(!grepl("Other", method_type) & !is.na(method_type)) |>
  left_join(targets_by_model, by = "model")

quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)

plot_method_var <- by_method_var |>
  filter(!grepl("pre-delta", variant)) |>
  group_by(target_type,
           horizon, variant) |>
  summarise(
    n = n(),
    value = quantile(rel_wis, quantiles),
    quantile = paste0("q", quantiles),
    .groups = "drop") |>
  pivot_wider(names_from = quantile) |>
  mutate(horizon = as.factor(horizon)) |>
  ggplot(aes(y = target_type,
             col = horizon, fill = horizon)) +
  geom_point(aes(x = q0.5), size = 0.5, alpha = 0.8,
             position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.25, xmax = q0.75), linewidth = 4,
                 alpha = 0.5, position = position_dodge(width = 1)) +
  geom_linerange(aes(xmin = q0.01, xmax = q0.99), linewidth = 4,
                 alpha = 0.2, position = position_dodge(width = 1)) +
  geom_vline(xintercept = 1, lty = 2) +
  labs(y = NULL, x = NULL,
       col = "Week ahead forecast horizon",
       fill = "Week ahead forecast horizon") +
  scale_colour_viridis_d() +
  facet_grid(rows = vars(horizon), cols = vars(variant), scales = "free") +
  # facet_wrap(~variant, nrow = 1, scales = "free") +
  theme(legend.position = "bottom")

plot_method_var

ggsave(here("output",
            "target-by-variant.jpg"),
       width = 6, height = 4)
