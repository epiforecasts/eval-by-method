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

data_plot <- function(scores, log_scale = FALSE, all = FALSE) {
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

    return(plot)
}
