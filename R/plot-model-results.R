library("purrr")
library("dplyr")
library("ggplot2")
library("patchwork")
library("gammit")
source(here("R", "process-data.R"))
source(here("R", "analysis-descriptive.R"))

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
                 shape = CountryTargets, lty = Model)) +
      geom_point(aes(y = value),
                 position = position_dodge(width=1)) +
      geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5),
                     position = position_dodge(width=1)) +
      geom_hline(yintercept = 0, lty = 2) +
      labs(y = "Partial effect on log WIS", x = "", colour = NULL, shape = NULL) +
      scale_shape_manual(
        values = c("Single-country" = 16, "Multi-country" = 17),
        drop = FALSE
      ) +
      scale_colour_brewer(type = "qual", palette = 2) +
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

plot_fit_obs <- function(fit_obs, scale_label = "WIS") {
  p <- ggplot(fit_obs, aes(observed, fitted)) +
    geom_point(alpha = 0.1, size = 0.4) +
    geom_abline(slope = 1, intercept = 0, lty = 2, colour = "red") +
    labs(x = paste("Observed", scale_label), y = paste("Fitted", scale_label)) +
    coord_equal() +
    theme(strip.background = element_blank())
  if ("epi_target" %in% names(fit_obs)) {
    p <- p + facet_wrap(~epi_target, scales = "free")
  }
  return(p)
}

plot_effects <- function(random_effects,
                         variables = NULL) {
  if(is.null(variables)){variables <- unique(random_effects$group_var)}

  random_effects |>
    filter(group_var %in% variables) |>
    mutate(group = factor(group, levels = unique(as.character(rev(group)))),
           Model = factor(model, levels = c("Adjusted", "Unadjusted"))) |>
    ggplot(aes(x = group, col = group_var,
               lty = Model, shape = Model)) +
    geom_point(aes(y = value),
               position = position_dodge(width=1)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5,),
                   position = position_dodge(width=1)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
    scale_shape_manual(values = c("Adjusted" = 16, "Unadjusted" = 1)) +
    labs(y = "Partial effect (log WIS scale)", x = NULL, colour = NULL) +
    scale_colour_brewer(type = "qual", palette = "Set1",
                        guide = "none") +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}
