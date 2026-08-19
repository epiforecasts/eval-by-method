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
      geom_point(aes(y = exp(value)),
                 position = position_dodge(width=1)) +
      geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5)),
                     position = position_dodge(width=1)) +
      geom_hline(yintercept = 1, lty = 2) +
      labs(y = "Performance ratio (vs average model)", x = "",
           colour = NULL, shape = NULL) +
      scale_y_log10() +
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

# Partial effect of each model structure, shown separately for case and death
# forecasts. These come from the s(Method, Epi_target) cells, so the two points
# for a structure are the quantity the interaction is there to expose: whether a
# structure predicts one outcome relatively better than the other.
plot_method_target <- function(method_by_target) {
  method_by_target |>
    mutate(
      Method = factor(Method, levels = rev(sort(unique(Method)))),
      `Epidemiological target` = factor(Epi_target,
                                        levels = c("Cases", "Deaths"))
    ) |>
    ggplot(aes(x = Method, col = `Epidemiological target`,
               shape = `Epidemiological target`)) +
    geom_point(aes(y = exp(value)), position = position_dodge(width = 0.6)) +
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5)),
                   position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.4) +
    scale_y_log10() +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    labs(y = "Performance ratio (vs average model)", x = NULL,
         colour = NULL, shape = NULL) +
    theme(legend.position = "bottom", strip.background = element_blank()) +
    coord_flip()
}

plot_fit_obs <- function(fit_obs, scale_label = "WIS") {
  p <- ggplot(fit_obs, aes(observed, fitted)) +
    geom_point(alpha = 0.1, size = 0.4) +
    geom_abline(slope = 1, intercept = 0, lty = 2, colour = "red") +
    labs(x = paste("Observed", scale_label), y = paste("Fitted", scale_label)) +
    theme(strip.background = element_blank())
  if ("epi_target" %in% names(fit_obs)) {
    # free scales (case/death WIS ranges differ) are incompatible with a fixed
    # aspect ratio, so omit coord_equal here
    p <- p + facet_wrap(~epi_target, scales = "free")
  } else {
    p <- p + coord_equal()
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
    geom_point(aes(y = exp(value)),
               position = position_dodge(width=1)) +
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5),),
                   position = position_dodge(width=1)) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.25) +
    scale_y_log10() +
    scale_shape_manual(values = c("Adjusted" = 16, "Unadjusted" = 1)) +
    labs(y = "Performance ratio (vs average model)", x = NULL, colour = NULL) +
    scale_colour_brewer(type = "qual", palette = "Set1",
                        guide = "none") +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    coord_flip()
}

# Model ranking before and after adjustment (#168) ---------------------------
#
# Individual-model effects come in two versions: "Unadjusted" from a univariate
# model with s(Model) alone, and "Adjusted" from the joint model. Ranking
# models under each gives the difference an increasingly controlled evaluation
# design makes to which models look best, holding the score itself fixed.
#
# Rank 1 is the best-performing model (lowest partial effect on the score).
rank_models <- function(random_effects, anonymise = TRUE) {
  classification <- classify_models() |>
    select(group = model, classification)
  ranks <- random_effects |>
    filter(group_var == "Model") |>
    select(model, group, value) |>
    tidyr::pivot_wider(names_from = model, values_from = value) |>
    mutate(
      rank_unadjusted = rank(Unadjusted, ties.method = "first"),
      rank_adjusted = rank(Adjusted, ties.method = "first"),
      rank_change = rank_unadjusted - rank_adjusted
    ) |>
    left_join(classification, by = "group")
  if (anonymise) {
    ranks <- ranks |>
      arrange(classification, rank_unadjusted) |>
      group_by(classification) |>
      mutate(label = paste(classification, row_number())) |>
      ungroup()
  } else {
    ranks <- mutate(ranks, label = group)
  }
  return(ranks)
}

# Summary statistics for the ranking comparison, so the text tracks the fit.
summarise_ranks <- function(ranks, threshold = 5) {
  list(
    n = nrow(ranks),
    spearman = cor(ranks$rank_unadjusted, ranks$rank_adjusted,
                   method = "spearman"),
    n_moved = sum(abs(ranks$rank_change) >= threshold),
    threshold = threshold,
    max_change = max(abs(ranks$rank_change)),
    max_model = ranks$label[which.max(abs(ranks$rank_change))]
  )
}

plot_model_ranks <- function(ranks) {
  ranks |>
    tidyr::pivot_longer(
      cols = c(rank_unadjusted, rank_adjusted),
      names_to = "Evaluation", values_to = "Rank"
    ) |>
    mutate(
      Evaluation = factor(
        Evaluation,
        levels = c("rank_unadjusted", "rank_adjusted"),
        labels = c("Unadjusted", "Adjusted")
      )
    ) |>
    ggplot(aes(x = Evaluation, y = Rank, group = label,
               colour = classification)) +
    geom_line(alpha = 0.6) +
    geom_point(size = 1.2) +
    scale_y_reverse(breaks = scales::breaks_pretty()) +
    scale_colour_brewer(type = "qual", palette = 2) +
    labs(x = NULL, y = "Rank (1 = best)", colour = NULL) +
    theme(legend.position = "bottom", strip.background = element_blank())
}
