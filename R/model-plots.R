library("purrr")
library("dplyr")
library("ggplot2")
library("patchwork")
library("gammit")
source(here("R", "prep-data.R"))
source(here("R", "descriptive.R"))

plot_models <- function(random_effects, scores, x_labels = TRUE) {
  outcomes <- unique(scores$outcome_target)
  classification <- classify_models() |>
    rename(group = model)
  targets <- table_targets(scores) |>
    select(group = Model, CountryTargets) |>
    distinct()
  plots <- map(random_effects, function(effects) {
    plot <- effects |>
      filter(group_var == "Model") |>
      left_join(classification) |>
      left_join(targets) |>
      group_by(classification, CountryTargets) |>
      mutate(group = paste(classification, CountryTargets, row_number())) |>
      select(-group_var) |>
      arrange(-value) |>
      ungroup() |>
      mutate(group = factor(group)) |>
      ggplot(aes(x = group, col = classification, shape = CountryTargets)) +
      geom_point(aes(y = value)) +
      geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
      geom_hline(yintercept = 0, lty = 2) +
      labs(y = "Partial effect", x = "", colour = NULL, shape = NULL) +
      scale_colour_brewer(type = "qual", palette = 2) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
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
  })
  ## remove legends
  plots <- map(seq_along(plots), \(i) {
    plots[[i]] + ggtitle(outcomes[i])
  })
  Reduce(`+`, plots) + plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
}

plot_effects <- function(random_effects) {
  random_effects |>
    bind_rows(.id = "outcome_target") |>
    filter(!(group_var %in% c("Model", "location", "Trend"))) |>
    mutate(group = factor(group, levels = unique(as.character(rev(group))))) |>
    ggplot(aes(x = group, col = group_var)) +
    geom_point(aes(y = value)) +
    geom_linerange(aes(ymin = lower_2.5, ymax = upper_97.5)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.25) +
    facet_wrap(~outcome_target, scales = "free_y") +
    labs(y = "Partial effect", x = NULL, colour = NULL, shape = NULL) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}
