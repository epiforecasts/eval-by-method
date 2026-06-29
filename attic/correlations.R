library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
source(here("R", "process-data.R"))

scores <- process_data(scoring_scale = "log")
ensemble <- scores |>
  filter(grepl("EuroCOVIDhub-ensemble", Model))
scores <- scores |>
  filter(!grepl("EuroCOVIDhub-", Model))
#-----------------------

target_id <- c("epi_target", "Location", "target_end_date", "Horizon")

scores_ens <- scores |>
  left_join(
    ensemble |>
      select(all_of(target_id), wis_ens = wis),
    by = target_id
  )

scores_ens |>
  ggplot(aes(x = wis_ens, y = wis, col = Model)) +
  geom_point() +
  facet_wrap(~Method) +
  theme(legend.position = "none")
scores_mean_target <- scores |>
  group_by(epi_target, Location, target_end_date, Horizon) |>
  summarise(
    wis_mean = mean(wis, na.rm = TRUE),
    n = n()
  )
scores_mean_method <- scores |>
  group_by(epi_target, Location, target_end_date, Horizon, Method) |>
  summarise(
    wis_mean_method = mean(wis, na.rm = TRUE),
    n = n()
  )
scores_mean_method |>
  filter(Horizon == 1 & epi_target == "Cases") |>
  ggplot(aes(x = target_end_date, y = Location, fill = n)) +
  geom_tile() +
  scale_fill_binned() +
  facet_wrap(~Method) +
  theme(legend.position = "bottom")

  scores_mean_method |>
  filter(Horizon == 1 & epi_target == "Cases") |>
  ggplot(aes(x = target_end_date, y = Location, fill = wis_mean_method)) +
  geom_tile() +
  facet_wrap(~Method) +
  theme(legend.position = "bottom")

scores |>
  filter(Horizon == 1 & epi_target == "Cases") |>
    ggplot(aes(x = target_end_date, y = wis, col = Model)) +
    geom_line() +
      theme(legend.position = "none")











#-------

plot_effects(results$effects, variables = c("Method"))
plot_effects(results$effects,
  variables = c(unique(results$effects$group_var))
) +
  coord_flip()


plot_effects <- function(random_effects,
                         variables = NULL) {
  if(is.null(variables)){variables <- unique(random_effects$group_var)}
random_effects |>
  filter(group_var %in% variables) |>
  mutate(
    group = factor(group, levels = unique(as.character(rev(group)))),
    Model = factor(model, levels = c("Adjusted", "Unadjusted"))
  ) |>
  ggplot(aes(
    x = group, col = group_var,
    lty = epi_target, shape = epi_target
  )) +
  geom_point(aes(y = value),
    position = position_dodge(width = 1)
  ) +
  geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5, ),
    position = position_dodge(width = 1)
  ) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
  scale_shape_manual(values = c("Adjusted" = 16, "Unadjusted" = 1)) +
  facet_wrap(~Model, scales = "free_y") +
  labs(y = "Partial effect (log WIS scale)", x = NULL, colour = NULL) +
  scale_colour_brewer(
    type = "qual", palette = "Set1",
    guide = "none"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_flip()
                         }

results <- readRDS(here("output", "log", "results.rds"))
table_effects <- results$effects |>
  mutate(upper_97.5_text = if_else(
           upper_97.5 < 0,
           paste0("(", round(upper_97.5, 2), ")"),
           as.character(round(upper_97.5, 2))
         )) |>
  mutate(value_ci = paste0(round(value, 2),
                           " (", round(lower_2.5, 2), "-",
                           upper_97.5_text, ")"),
         group = paste(epi_target, model, group, sep = "_")) |>
  column_to_rownames("group")

effects_comp <- results$effects |>
  pivot_longer(cols = c(value, lower_2.5, upper_97.5, se)) |>
  pivot_wider(names_from = model) |>
  mutate(diff = abs(Unadjusted - Adjusted))|>
  filter(group_var %in% c("Method", "CountryTargets"))

  |>
  select(-c("effect", "Unadjusted"))
