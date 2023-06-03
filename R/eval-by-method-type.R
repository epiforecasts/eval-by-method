# scores by model type
library(here)
library(ggplot2)
source(here("R", "get-metadata.R"))
source(here("R", "get-scores.R"))

# Get scores and metadata ------------------------------
metadata <- get_metadata_processed() |>
  select(model_abbr, method_type)
scores_raw <- get_scores() |>
  left_join(metadata, by = c("model" = "model_abbr"))

# Identify number of each method type by week
freq_methods <- scores_raw |>
  filter(!grepl("EuroCOVIDhub", model)) |>
  select(model, forecast_date, method_type) |>
  distinct() |>
  group_by(forecast_date, method_type) |>
  summarise(method_count = n(), .groups = "drop")

freq_methods |>
  ggplot(aes(x = forecast_date, y = method_count,
               fill = method_type)) +
  geom_col() +
  labs(x = NULL, y = "Number of forecasting models",
       fill = "Model method",
       title = "Forecasting models over time") +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_bw() +
  theme(legend.position = "bottom")

scores_raw |>
  filter(!grepl("EuroCOVIDhub", model)) |>
  distinct(method_type, model) |>
  group_by(method_type) |>
  tally()


scores_raw |>
  filter(method_type == "Ensemble") |>
  distinct(model)

# Pairwise scoring -----------------------------
# Relabel model field to keep model + model type after scoring
scores <- scores_raw |>
  mutate(model = ifelse(grepl("EuroCOVIDhub", model), model,
                        paste0(method_type, "=", model)))

# Relative scoring to ensemble
pw_methods <- pairwise_scoring(scores = scores,
                              baseline_model = "EuroCOVIDhub-ensemble",
                              by_variables = c("model", "horizon"))

# Plot relative WIS. Exclude ensemble and use higher groupings of model method type
pw_methods_data <- pw_methods |>
  tidyr::separate(model, into = c("type", "model_abbr"),
                  sep = "=") |>
  mutate(any_mechanistic = ifelse(type == "Empirical",
                                  "Empirical",
                                  "Semi/fully mechanistic"),
         horizon = as.factor(horizon),
         type = as.factor(type)) |>
  filter(!grepl("EuroCOVIDhub", type) &
           !grepl("ensemble", type))

methods_counts <- c("n" = nrow(pw_methods_data),
            "n_5p" = nrow(filter(pw_methods_data, rel_wis > 5)))
# Boxplot
pw_methods_data |>
  ggplot(aes(y = rel_wis, x = horizon,
             col = any_mechanistic)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Horizon", y = "Relative WIS",
       col = "Model type",
       title = "Performance by horizon and model type",
       caption = paste0("N=", methods_counts[["n"]],
                        ". Outliers with rWIS >5 not shown, n=",
                        methods_counts[["n_5p"]])) +
  scale_y_continuous(limits = c(NA,5)) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_bw() +
  theme(legend.position = "bottom")



