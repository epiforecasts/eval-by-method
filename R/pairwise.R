# scores by model type
source(here("R", "get-metadata.R"))
source(here("R", "evaluation.R"))

metadata <- get_metadata_processed()
metadata <- metadata |>
  select(model_abbr, model_main)
scores <- get_scores() |>
  left_join(metadata, by = c("model" = "model_abbr"))

pw_scores <- scores |>
  #mutate(model = paste0(model_main, "~", model)) |>
  pairwise_scoring(scores = scores, by_variables = "horizon")
