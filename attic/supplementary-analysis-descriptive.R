# Describe interval score in terms of model structure and country target type
# Load data:
# source(here("R", "process-data.R"))
# scores <- process_data(scoring_scale = "log")
library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(forcats)
library(patchwork)
library(janitor)
library(kableExtra)
library(stringr)
library(boot)

# Plot over time ----------------------------------
plot_over_time <- function(scores, ensemble, add_plot, show_uncertainty = TRUE) {
    plot_data <- scores |>
        group_by(target_end_date, epi_target, CountryTargets, Method) |>
        reframe(
            n = n(),
            mean = mean(wis, na.rm = TRUE),
            ci = calc_ci(wis, na.rm = TRUE, R = 1000)
        ) |>
        unnest(ci)

    score_plot <- plot_data |>
        ggplot(aes(x = target_end_date, col = Method, fill = Method)) +
        geom_line(aes(y = mean), alpha = 0.7)

    if (show_uncertainty) {
        score_plot <- score_plot +
            geom_ribbon(aes(ymin = lboot, ymax = uboot), alpha = 0.1, col = NA)
    }

    score_plot <- score_plot +
        facet_grid(CountryTargets ~ epi_target, scales = "free_y") +
        scale_x_date(date_labels = "%b %Y") +
        scale_fill_brewer(aesthetics = c("col", "fill"), type = "qual", palette = "Set2") +
        labs(x = NULL, y = "Mean WIS (log scale)", fill = NULL, col = NULL) +
        theme(
            legend.position = "bottom",
            strip.background = element_blank()
        )

    return(score_plot)
}

# Ridge plot by model --------------------
plot_ridges <- function(scores, target = "Deaths") {
    scores |>
        filter(epi_target == target) |>
        group_by(Model) |>
        mutate(
            median_score = median(wis, na.rm = TRUE),
            lq = quantile(wis, 0.25, na.rm = TRUE),
            uq = quantile(wis, 0.75, na.rm = TRUE)
        ) |>
        ungroup() |>
        mutate(Model = fct_reorder(Model, median_score)) |>
        filter(wis >= lq & wis <= uq) |>
        # Plot
        ggplot(aes(x = wis, y = Model, fill = stat(x))) +
        geom_density_ridges_gradient(
            scale = 1.5,
            rel_min_height = 0.01,
            quantile_lines = TRUE, quantiles = 2
        ) +
        scale_fill_viridis_c(
            name = "Interval score",
            option = "C", direction = -1
        ) +
        theme_ridges() +
        labs(x = "WIS (IQR)", y = "Model") +
        theme(legend.position = "none")
}

# Data --------------------
# plot mean wis [supplement]
data_plot <- function(scores, log = FALSE, all = FALSE) {
    data <- scores |>
        select(Location, epi_target, target_end_date, Incidence) |>
        distinct()
    pop <- read_csv(here("data", "populations.csv"), show_col_types = FALSE) |>
        rename(Location = location)
    data <- data |>
        left_join(pop, by = join_by(Location)) |>
        mutate(
            rel_inc = Incidence / population * 1e5,
            log_inc = log(Incidence + 1)
        )
    total <- data |>
        group_by(epi_target, target_end_date) |>
        summarise(
            Incidence = sum(Incidence),
            population = sum(population),
            .groups = "drop"
        ) |>
        mutate(
            rel_inc = Incidence / population * 1e5,
            log_inc = log(Incidence + 1),
            Location = "Total"
        )
    var_name <- ifelse(log, "log_inc", "rel_inc")
    plot <- ggplot(mapping = aes(
        x = target_end_date, y = .data[[var_name]], group = Location
    ))

    if (all) {
        plot <- plot + geom_line(data = data, alpha = 0.1)
    }

    plot <- plot +
        geom_line(data = total, linewidth = ifelse(all, 2, 1)) +
        facet_wrap(~epi_target, scales = "free") +
        xlab("")

    if (log) {
        plot <- plot + ylab("Observed incidence (log scale)")
    } else {
        plot <- plot + ylab("Incidence per 100,000")
    }
    plot <- plot +
        theme(strip.background = element_blank())

    return(plot)
}
