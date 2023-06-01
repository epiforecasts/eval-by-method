# scores by model type
library(here)
library(ggplot2)
source(here("R", "get-metadata.R"))
source(here("R", "evaluation.R"))

metadata <- get_metadata_processed()
metadata <- metadata |>
  select(model_abbr, model_main)
scores <- get_scores() |>
  left_join(metadata, by = c("model" = "model_abbr")) |>
  mutate(model = ifelse(grepl("EuroCOVIDhub", model), model,
                        paste0(model_main, "=", model)))


# Pairwise scoring -----------------------------
pw_scores <- pairwise_scoring(scores = scores,
                              baseline_model = "EuroCOVIDhub-ensemble",
                              by_variables = c("model", "horizon"))

# Plot --------------------------------------------------------------------
pw_scores |>
  tidyr::separate(model, into = c("type", "model_abbr"), sep = "=") |>
  mutate(horizon = as.factor(horizon),
         type = as.factor(type)) |>
  filter(!grepl("EuroCOVIDhub", type)) |>
  ggplot(aes(y = rel_wis, x = horizon,
             group = model_abbr,
             fill = type, col = type)) +
  geom_point(alpha = 0.5,
             position = position_jitter(width = 0.1)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  theme_bw()

