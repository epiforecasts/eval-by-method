# Count number of targets from each model by week
targets_by_model <- scores_raw |>
  filter(!grepl("EuroCOVIDhub-ensemble", model)) |>
  select(model, forecast_date, location) |>
  distinct() |>
  ungroup() |>
  group_by(model, forecast_date) |>
  summarise(target_count = n()) |>
  mutate(target_type = ifelse(target_count <= 2, "Single-country", "Multi-country")) |>
  ungroup() |>
  group_by(model) |>
  summarise(target_type = all(target_count <= 2)) |>
  mutate(target_type = factor(target_type,
                              levels = c(TRUE, FALSE),
                              labels = c("Single-country", "Multi-country")))
write_csv(targets_by_model, here("data", "targets-by-model.csv"))
