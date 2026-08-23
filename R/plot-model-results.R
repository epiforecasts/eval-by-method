library("purrr")
library("dplyr")
library("ggplot2")
library("patchwork")
library("gammit")
source(here("R", "process-data.R"))
source(here("R", "analysis-descriptive.R"))

# Note plot style, to use everywhere: lineranges showing CIs should always be bars (lwd=2) surrounding a point the same colour, with lower alpha; effects shown as forest plots should order in ascending order of the point estimate, except for method (model structure) and epi outcome, which should be shown in the specified order; epi outcome should be coloured as specified throughout

plot_config <- list(
  epi_levels = ordered(c("Cases"= "#fe9929", "Deaths" = "#993404")),
  method_levels = ordered(c("Judgement" = "#0c2c84",
                            "Statistical" = "#225ea8",
                            "Semi-mechanistic" = "#1d91c0",
                            "Mechanistic" = "#7fcdbb",
                            "Agent-based" = "#c7e9b4"))
)

# Fit vs observed
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

# Partial effect of each model structure, for case and death
plot_method_target <- function(method_by_target, plot_config) {
  if (is.null(method_levels)) {
    method_levels <- rev(sort(unique(method_by_target$Method)))
  }

  method_by_target |>
    mutate(
      Method = factor(Method, levels = plot_config$method_levels),
      `Epidemiological target` = factor(Epi_target,
                                        levels = c("Cases", "Deaths"))
    ) |>
    ggplot(aes(x = Method, col = `Epidemiological target`)) +
    geom_point(aes(y = exp(value)), position = position_dodge(width = 0.6),
               size = 1.5, alpha = 0.8)+
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5)),
                   position = position_dodge(width = 0.6), lwd = 2,
                   alpha = 0.6) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.4) +
    scale_y_log10() +
    scale_colour_manual(values = colour_key) +
    labs(y = "Adjusted performance ratio", x = NULL,
         colour = NULL) +
    theme(legend.position = "bottom", strip.background = element_blank()) +
    coord_flip()
}

# All effects (used in supplement)
plot_effects <- function(random_effects,
                         variables = NULL) {
  if(is.null(variables)){variables <- unique(random_effects$group_var)}

  # Colour separates the variables when several are shown together
  colour_scale <- if (length(variables) > 1) {
    scale_colour_viridis_d("B", guide = "none")
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
                   position = position_dodge(width=1), lwd = 2, alpha = 0.7) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.25) +
    scale_y_log10() +
    scale_shape_manual(values = c("Adjusted" = 16, "Unadjusted" = 1)) +
    labs(y = "Performance ratio vs average", x = NULL,
         colour = NULL, shape = NULL) +
    colour_scale +
    theme(legend.position = "bottom", strip.background = element_blank()) +
    coord_flip()
}

# Individual models before and after adjustment ---------------------------
# Individual-model effects come in two versions: "Unadjusted" from a univariate
# model with s(Model) alone, and "Adjusted" from the joint model
# Adjusted effect of each model, ordered by that effect but labelled by the
# model's unadjusted rank
plot_models <- function(random_effects, ranks = NULL, x_labels = TRUE) {
  if (is.null(ranks)) ranks <- rank_models(random_effects)
  effects <- random_effects |>
    filter(group_var == "Model") |>
    left_join(select(ranks, group, rank_unadjusted), by = "group") |>
    mutate(label = paste("Unadjusted rank", rank_unadjusted))
  # Levels run from worst to best adjusted effect: coord_flip() then puts the
  # best-performing model at the top.
  label_levels <- effects |>
    filter(model == "Adjusted") |>
    arrange(desc(value)) |>
    pull(label)
  plot <- effects |>
    mutate(label = factor(label, levels = label_levels)) |>
    ggplot(aes(x = label, col = rank_unadjusted)) +
    geom_point(aes(y = exp(value))) +
    geom_linerange(aes(ymin = exp(lower_2.5), ymax = exp(upper_97.5))) +
    geom_hline(yintercept = 1, lty = 2) +
    labs(y = "Adjusted performance ratio", x = "",
         colour = "Unadjusted rank (1 = best)") +
    scale_y_log10() +
    scale_colour_viridis_b() +
    guides(colour = guide_colourbar(barwidth = 10, barheight = 0.5,
                                    title.position = "top")) +
    theme(
      legend.position = "bottom",
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
summarise_ranks <- function(ranks, threshold = 10) {
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
    scale_colour_manual(values = plot_config$method_levels) +
    coord_equal() +
    labs(x = "Unadjusted rank (1 = best)", y = "Adjusted rank (1 = best)",
         colour = NULL) +
    # five structures do not fit on one row at this panel width
    guides(colour = guide_legend(nrow = 2)) +
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
plot_model_variation <- function(effects, ranks = NULL) {
  if (is.null(ranks)) ranks <- rank_models(effects)
  effects_adjusted <- filter(effects, model == "Adjusted" | group_var != "Model")
  # The panels no longer share an aesthetic, so each carries its own key
  (plot_models(effects_adjusted, ranks = ranks) |
     plot_model_ranks(ranks)) +
    patchwork::plot_layout(widths = c(1.6, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")
}
