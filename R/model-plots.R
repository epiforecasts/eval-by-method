library("purrr")
library("dplyr")
library("ggplot2")
library("patchwork")
library("gammit")
source(here("R", "prep-data.R"))
source(here("R", "descriptive.R"))

plot_models <- function(random_effects, scores, x_labels = TRUE,
                        anonymise = TRUE) {
  classification <- classify_models() |>
    rename(group = model)
  targets <- table_targets(scores) |>
    select(group = Model, CountryTargets) |>
    distinct()
  effects <- random_effects |>
    filter(group_var == "Model") |>
    left_join(classification) |>
    left_join(targets)
  models <- effects |>
    select(classification, CountryTargets, group) |>
    distinct() |>
    group_by(classification, CountryTargets) |>
    mutate(
      id = row_number(),
      anon_group = paste(classification, CountryTargets, id),
      ) |>
    ungroup() |>
    arrange(classification, CountryTargets, id) |>
    mutate(anon_group = factor(anon_group, levels = rev(unique(anon_group)))) |>
    select(group, anon_group)
  group_var <- ifelse(anonymise, "anon_group", "group")
  plot <- effects |>
      left_join(models, by = "group") |>
      mutate(
        Model = factor(model, levels = c("Adjusted", "Unadjusted"))
      ) |>
      ggplot(aes(x = .data[[group_var]], col = classification,
                 shape = CountryTargets, lty = Model, alpha = Model)) +
      geom_point(aes(y = value),
                 position = position_dodge(width=1)) +
      geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5),
                     position = position_dodge(width=1)) +
      geom_hline(yintercept = 0, lty = 2) +
      labs(y = "Partial effect", x = "", colour = NULL, shape = NULL) +
      scale_alpha_manual(values = c("Adjusted" = 1, "Unadjusted" = 0.3)) +
      scale_colour_brewer(type = "qual", palette = 2) +
      facet_wrap(~outcome_target, scales = "free_y", drop = TRUE) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank()
      ) +
      coord_flip()
    if (!x_labels) {
      plot <- plot +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
  return(plot)
}

plot_effects <- function(random_effects,
                         variables = c("Method", "CountryTargets")) {
  random_effects |>
    filter(group_var %in% variables) |>
    mutate(group = factor(group, levels = unique(as.character(rev(group)))),
           Model = factor(model, levels = c("Adjusted", "Unadjusted"))) |>
    ggplot(aes(x = group, col = group_var,
               lty = Model, alpha = Model)) +
    geom_point(aes(y = value),
               position = position_dodge(width=1)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5,),
                   position = position_dodge(width=1)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
    scale_alpha_manual(values = c("Adjusted" = 1, "Unadjusted" = 0.3)) +
    facet_wrap(~outcome_target, scales = "free_y") +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = "Set1",
                        guide = FALSE) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}
