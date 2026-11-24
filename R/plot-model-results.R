library("here")
library("purrr")
library("dplyr")
library("ggplot2")
library("patchwork")
library("gammit")
source(here("R", "process-data.R"))
source(here("R", "analysis-descriptive.R"))

# Note plot style, to use everywhere: lineranges showing CIs should always be bars (lwd=2) surrounding a point the same colour, with lower alpha; effects shown as forest plots should order in ascending order of the point estimate, except for method (model structure) and epi outcome, which should be shown in the specified order; epi outcome should be coloured as specified throughout

plot_config <- list(
  epi_colours = c("Cases" = "#fe9929", "Deaths" = "#993404"),
  epi_levels = c("Cases", "Deaths"),
  method_colours = c("Judgement" = "#0c2c84",
                     "Statistical" = "#225ea8",
                     "Semi-mechanistic" = "#1d91c0",
                     "Mechanistic" = "#7fcdbb",
                     "Agent-based" = "#c7e9b4"),
  method_levels = c("Judgement", "Statistical", "Semi-mechanistic",
                    "Mechanistic", "Agent-based")
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
plot_method_target <- function(method_by_target) {
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
    scale_colour_manual(values = plot_config$epi_colours) +
    labs(y = "Adjusted performance ratio", x = NULL,
         colour = NULL) +
    theme(legend.position = "bottom", strip.background = element_blank()) +
    coord_flip()
}

# Smooth effect of a continuous covariate (Growth or Incidence), one panel
# per outcome, as a ratio of expected LWIS to that outcome's average forecast.
# `smooths` is results$smooths (outcome_smooth_curves(), already relative to
# each outcome's average); `data` is results$data, used to limit each
# curve to the central `coverage` of that outcome's observed values.
plot_covariate_smooth <- function(smooths, data, covariate = "Growth",
                                  coverage = 0.98) {
  tails <- (1 - coverage) / 2
  observed_range <- data |>
    group_by(Epi_target = as.character(Epi_target)) |>
    summarise(lo = quantile(.data[[covariate]], tails),
              hi = quantile(.data[[covariate]], 1 - tails), .groups = "drop")
  smooths |>
    filter(grepl(paste0("^s\\(", covariate, "\\)"), .smooth)) |>
    mutate(x = .data[[covariate]]) |>
    left_join(observed_range, by = "Epi_target") |>
    filter(between(x, lo, hi)) |>
    mutate(Epi_target = factor(Epi_target, levels = plot_config$epi_levels)) |>
    ggplot(aes(x = x, y = exp(.estimate))) +
    geom_hline(yintercept = 1, lty = 2, alpha = 0.4) +
    geom_ribbon(aes(ymin = exp(.estimate - 1.96 * .se),
                    ymax = exp(.estimate + 1.96 * .se), fill = Epi_target),
                alpha = 0.3) +
    geom_line(aes(colour = Epi_target)) +
    facet_wrap(~Epi_target, scales = "free_x") +
    scale_y_log10() +
    scale_colour_manual(values = plot_config$epi_colours, guide = "none") +
    scale_fill_manual(values = plot_config$epi_colours, guide = "none") +
    labs(x = covariate, y = "Adjusted performance ratio") +
    theme(strip.background = element_blank())
}

# Ratio of expected LWIS (95% CI) read off a smooth at given covariate values,
# by linear interpolation on the saved grid
smooth_ratio_at <- function(smooths, covariate = "Growth",
                            at = c(-0.2, 0, 0.2)) {
  smooths |>
    filter(grepl(paste0("^s\\(", covariate, "\\)"), .smooth)) |>
    group_by(Epi_target) |>
    reframe(
      at = at,
      est = approx(.data[[covariate]], .estimate, xout = at)$y,
      se = approx(.data[[covariate]], .se, xout = at)$y
    ) |>
    mutate(ratio = exp(est), lower = exp(est - 1.96 * se),
           upper = exp(est + 1.96 * se),
           ratio_ci = sprintf("%.2f (%.2f-%.2f)", ratio, lower, upper))
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
      rank_change = rank_unadjusted - rank_adjusted,
      quartile_unadjusted = ntile(rank_unadjusted, 4),
      quartile_adjusted = ntile(rank_adjusted, 4)
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
# Quartile changes are set against 75%, the share expected to change quartile
# if the two rankings were unrelated (1 - 1/4).
summarise_ranks <- function(ranks) {
  quartile_change <- abs(ranks$quartile_unadjusted - ranks$quartile_adjusted)
  list(
    n = nrow(ranks),
    spearman = cor(ranks$rank_unadjusted, ranks$rank_adjusted,
                   method = "spearman"),
    n_quartile_changed = sum(quartile_change > 0),
    pct_quartile_changed = 100 * mean(quartile_change > 0),
    pct_quartile_chance = 75,
    n_quartile_2plus = sum(quartile_change >= 2)
  )
}

# Models by quartile of unadjusted (rows) and adjusted (columns) rank
tabulate_rank_quartiles <- function(ranks) {
  quartile_labels <- c("Q1 (best)", "Q2", "Q3", "Q4 (worst)")
  table(
    Unadjusted = factor(ranks$quartile_unadjusted, 1:4, quartile_labels),
    Adjusted = factor(ranks$quartile_adjusted, 1:4, quartile_labels)
  )
}

plot_model_ranks <- function(ranks) {
  # Unadjusted against adjusted rank. Distance from the diagonal is how far a
  # model moved once the difficulty of its targets was accounted for; a
  # scatter avoids the crossing lines of a slope chart at this many models.
  # Faint lines mark quartile boundaries: points off the diagonal blocks
  # changed quartile.
  n_models <- nrow(ranks)
  quartile_breaks <- n_models / 4 * 1:3 + 0.5

  p <- ggplot(
    ranks,
    aes(x = rank_unadjusted, y = rank_adjusted, colour = classification, shape = classification)
  ) +
    geom_vline(xintercept = quartile_breaks, colour = "grey85") +
    geom_hline(yintercept = quartile_breaks, colour = "grey85") +
    geom_abline(slope = 1, intercept = 0, lty = 2, colour = "grey50") +
    geom_point(size = 1.8, alpha = 0.9) +
    scale_x_continuous(
      limits = c(1, n_models),
      breaks = c(1, seq(10, n_models, by = 10))
    ) +
    scale_y_continuous(
      limits = c(1, n_models),
      breaks = c(1, seq(10, n_models, by = 10))
    ) +
    scale_colour_manual(values = plot_config$method_colours,
                       breaks = plot_config$method_levels) +
    scale_shape_manual(values = c(16, 17, 15, 18, 8),
                      breaks = plot_config$method_levels) +
    coord_equal() +
    labs(
      x = "Unadjusted rank (1 = best)",
      y = "Adjusted rank (1 = best)",
      colour = NULL,
      shape = NULL
    ) +
    # five structures do not fit on one row at this panel width
    guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", strip.background = element_blank())

  return(p)
}

# Individual-model variation as one figure: adjusted effect per model (A), and
# what adjustment does to their ranking (B).
plot_model_variation <- function(effects, ranks = NULL) {
  if (is.null(ranks)) {
    ranks <- rank_models(effects)
  }
  effects_adjusted <- filter(
    effects,
    model == "Adjusted" | group_var != "Model"
  )

  (plot_models(effects_adjusted, ranks = ranks) |
    plot_model_ranks(ranks)) +
    patchwork::plot_layout(widths = c(1.6, 1)) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")
}
