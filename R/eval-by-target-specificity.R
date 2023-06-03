# Identify whether models submit one or multiple location targets
# Note: the count of targets per model is based off scoring data so is probably inaccurate (as we knock out scores around anomalies). This might over-count the number of single-target models. Properly, should get number of targets from the full forecast dataset.

library(here)
library(arrow)
library(dplyr)
library(ggplot2)
source(here("R", "get-scores.R"))

# Get evaluation data for each model by week and target
scores_raw <- get_scores()

# Identify number of targets for each model by week -----------------
freq_targets <- scores_raw |>
  filter(!grepl("EuroCOVIDhub", model)) |>
  select(model, forecast_date, location) |>
  distinct() |>
  ungroup() |>
  group_by(model, forecast_date) |>
  summarise(target_count = n()) |>
  mutate(target_type = ifelse(target_count == 1, "Single-country", "Multi-country")) |>
  ungroup()

# Plot number of models by target over time
freq_targets |>
  ggplot(aes(x = forecast_date, fill = target_type)) +
  geom_histogram(stat = "count") +
  labs(x = NULL, y = "Number of forecasting models",
       fill = "Models' forecast targets",
       title = "Forecasting models over time") +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_bw() +
  theme(legend.position = "bottom")

# Assign target type by whether model has ever forecast for >1 target
targets_by_model <- freq_targets |>
  group_by(model) |>
  summarise(target_type = all(target_count == 1)) |>
  mutate(target_type = factor(target_type,
                              levels = c(TRUE, FALSE),
                              labels = c("Single-country", "Multi-country")))

scores <- scores_raw |>
  left_join(targets_by_model, by = c("model"))

# For single country, look at which countries forecasted
target_single <- scores |>
  filter(target_type == "Single-country") |>
  group_by(location_name) |>
  summarise(models = n_distinct(model)) |>
  arrange(desc(models))
# cat(paste0(target_single$location_name, " (", target_single$models, "),"))

target_multi <- scores |>
  filter(target_type == "Multi-country") |>
  group_by(model) |>
  summarise(locations = n_distinct(location_name)) |>
  arrange(desc(locations))

# Pairwise scoring ------------------------------------------
# Relative scoring to ensemble
scores <- scores |>
  mutate(model = ifelse(grepl("EuroCOVIDhub", model), model,
                        paste0(target_type, "=", model)))
pw_targets <- pairwise_scoring(scores = scores,
                              baseline_model = "EuroCOVIDhub-ensemble",
                              by_variables = c("model", "horizon"))

pw_targets_data <- pw_targets |>
  tidyr::separate(model, into = c("type", "model_abbr"),
                  sep = "=") |>
  mutate(horizon = as.factor(horizon),
         type = as.factor(type)) |>
  filter(!grepl("EuroCOVIDhub", type))

targets_counts <- c("n" = nrow(pw_targets_data),
            "n_5p" = nrow(filter(pw_targets_data, rel_wis > 5)))

# Boxplot
pw_targets_data |>
  ggplot(aes(y = rel_wis, x = horizon,
             col = type)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Horizon", y = "Relative WIS",
       col = "Model forecast targets",
       title = "Performance by horizon and target type",
       caption = paste0("N=", counts[["n"]],
                        ". Outliers with rWIS >5 not shown, n=",
                        counts[["n_5p"]])) +
  scale_y_continuous(limits = c(NA,5)) +
  scale_color_brewer(type = "qual", palette = 6) +
  theme_bw() +
  theme(legend.position = "bottom")
