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
  # Order models by their adjusted effect, so the figure reads as a ranking.
  # Anonymised labels are still numbered within a structure, so a label
  # identifies a model without revealing which team it belongs to.
  effect_order <- effects |>
    filter(model == "Adjusted") |>
    arrange(value) |>
    pull(group)
  models <- effects |>
    select(classification, CountryTargets, group) |>
    distinct() |>
    group_by(classification, CountryTargets) |>
    mutate(
      id = row_number(),
      anon_group = paste(classification, CountryTargets, id),
      ) |>
    ungroup() |>
    mutate(group = factor(group, levels = effect_order)) |>
    arrange(group) |>
    mutate(
      anon_group = factor(anon_group, levels = rev(unique(anon_group))),
      group = as.character(group)
    ) |>
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
           colour = NULL, shape = NULL, lty = NULL) +
      scale_y_log10() +
      scale_shape_manual(
        values = c("Single-country" = 16, "Multi-country" = 17),
        drop = FALSE
      ) +
      scale_colour_brewer(type = "qual", palette = 2) +
      # Three keys do not fit on one row at this width, so stack them
      guides(colour = guide_legend(nrow = 1, order = 1),
             shape = guide_legend(nrow = 1, order = 2),
             # a single series needs no adjusted/unadjusted key
             lty = if (n_distinct(effects$model) > 1) {
               guide_legend(nrow = 1, order = 3)
             } else {
               "none"
             }) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(t = 0, b = 0),
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
plot_method_target <- function(method_by_target, method_levels = NULL) {
  if (is.null(method_levels)) {
    method_levels <- rev(sort(unique(method_by_target$Method)))
  }
  method_by_target |>
    mutate(
      Method = factor(Method, levels = method_levels),
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

# Structure effects as one figure: pooled across outcomes (A) and separately by
# outcome (B). The two panels answer the same question at different resolution,
# so they share a y axis ordering and only panel A carries the labels.
plot_structure_effects <- function(effects, method_by_target) {
  method_levels <- effects |>
    filter(group_var == "Method") |>
    pull(group) |>
    unique() |>
    as.character() |>
    sort() |>
    rev()

  pooled <- plot_effects(effects, variables = "Method") +
    scale_x_discrete(limits = method_levels) +
    labs(y = "Performance ratio (vs average model)")

  by_target <- plot_method_target(method_by_target, method_levels = method_levels) +
    labs(y = "Performance ratio (vs average model)") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  (pooled | by_target) +
    patchwork::plot_annotation(tag_levels = "A")
}

plot_fit_obs <- function(fit_obs, scale_label = "WIS") {
  p <- ggplot(fit_obs, aes(observed, fitted)) +
    geom_point(alpha = 0.1, size = 0.4) +
    geom_abline(slope = 1, intercept = 0, lty = 2, colour = "grey40") +
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

  # Colour separates the variables when several are shown together. With a
  # single variable it would encode nothing, so it becomes one fixed grey.
  colour_scale <- if (length(variables) > 1) {
    scale_colour_brewer(type = "qual", palette = "Dark2", guide = "none")
  } else {
    scale_colour_manual(values = "grey30", guide = "none")
  }

  random_effects |>
    filter(group_var %in% variables) |>
    mutate(group = factor(group, levels = unique(as.character(rev(group)))),
           Model = factor(model, levels = c("Adjusted", "Unadjusted"))) |>
    ggplot(aes(x = group, shape = Model, col = group_var)) +
    geom_point(aes(y = exp(value)),
               position = position_dodge(width=1)) +
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5),),
                   position = position_dodge(width=1)) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.25) +
    scale_y_log10() +
    scale_shape_manual(values = c("Adjusted" = 16, "Unadjusted" = 1)) +
    labs(y = "Performance ratio (vs average model)", x = NULL,
         colour = NULL, shape = NULL) +
    colour_scale +
    theme(legend.position = "bottom", strip.background = element_blank()) +
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

plot_model_ranks <- function(ranks, annotate = TRUE) {
  # Unadjusted against adjusted rank. Distance from the diagonal is how far a
  # model moved once the difficulty of its targets was accounted for; a
  # scatter avoids the crossing lines of a slope chart at this many models.
  rank_summary <- summarise_ranks(ranks)
  n_models <- rank_summary$n

  p <- ggplot(ranks, aes(x = rank_unadjusted, y = rank_adjusted,
                         colour = classification)) +
    geom_abline(slope = 1, intercept = 0, lty = 2, colour = "grey50") +
    geom_point(size = 1.6, alpha = 0.9) +
    scale_x_continuous(limits = c(1, n_models),
                       breaks = c(1, seq(10, n_models, by = 10))) +
    scale_y_continuous(limits = c(1, n_models),
                       breaks = c(1, seq(10, n_models, by = 10))) +
    scale_colour_brewer(type = "qual", palette = 2) +
    coord_equal() +
    labs(x = "Unadjusted rank (1 = best)", y = "Adjusted rank (1 = best)",
         colour = NULL) +
    theme(legend.position = "bottom", strip.background = element_blank())

  if (annotate) {
    p <- p + annotate(
      "text", x = 1, y = n_models, hjust = 0, vjust = 1, size = 3,
      colour = "grey30",
      label = paste0("Spearman ", round(rank_summary$spearman, 2), "\n",
                     rank_summary$n_moved, " of ", n_models,
                     " move \u2265 ", rank_summary$threshold, " places")
    )
  }
  return(p)
}

# Individual-model variation as one figure: adjusted effect per model (A), and
# what adjustment does to their ranking (B).
plot_model_variation <- function(effects, scores, ranks = NULL) {
  if (is.null(ranks)) ranks <- rank_models(effects)
  effects_adjusted <- filter(effects, model == "Adjusted" | group_var != "Model")
  # Panel A's legend covers both panels: same structures, same palette
  (plot_models(effects_adjusted, scores) |
     (plot_model_ranks(ranks) + guides(colour = "none"))) +
    patchwork::plot_layout(widths = c(1.6, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")
}
