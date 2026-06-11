library(here)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
source(here("R", "process-data.R"))

scores <- process_data(scoring_scale = log)
ensemble <- scores |>
  filter(grepl("EuroCOVIDhub-ensemble", Model))
scores <- scores |>
  filter(!grepl("EuroCOVIDhub-", Model))
#-----------------------

target_id <- c(epi_target, Location, target_end_date, Horizon)

scores_ens |>
  ggplot(aes(x = wis_ens, y = wis, col = Model)) +
  geom_point() +
  facet_wrap(~Method) +
  theme(legend.position = none)

scores_target <- scores |>
  group_by(epi_target, Location, target_end_date, Horizon, Method) |>
  summarise(wis_mean_method = mean(wis, na.rm = TRUE),
    n = n())

scores |>
filter(Horizon == 1 & epi_target == "Cases") |>
ggplot(aes(x = target_end_date, y = Location, col = n)) +

facet_wrap(~Method) +
theme(legend.position = "bottom")

scores |>
filter(Horizon == 1 & epi_target == "Cases") |>
ggplot(aes(x = target_end_date, y = wis, col = Model)) +
geom_density_2d() +
theme(legend.position = "none")
