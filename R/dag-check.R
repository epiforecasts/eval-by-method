# Define DAG for model structure and WIS
# Variables
# Method: model structure (exposure)
# wis: weighted interval score (outcome)
# team_resources: resources available to the team (latent, unobserved)
# CountryTargets: single- vs multi-country model
# epidemic_state: latent epidemic transmission dynamics at (Location, date)
# Location: country target location (cause of epidemic_state)
# VariantPhase: dominant variant phase at forecast target date (cause of epidemic_state)
# Trend: increasing, decreasing, or stable epidemic trend (observable proxy of epidemic_state)
# Incidence: observed incidence level at forecast target date (observable proxy of epidemic_state)
# Horizon: forecast horizon

library(dagitty)
library(ggdag)
library(ggplot2)

dag <- dagitty('dag {
  Method[exposure]
  wis[outcome]
  team_resources[latent]
  epidemic_state[latent]

  team_resources -> Method
  team_resources -> CountryTargets
  team_resources -> wis

  CountryTargets -> wis
  Method -> wis

  Location -> epidemic_state
  VariantPhase -> epidemic_state
  epidemic_state -> Trend
  epidemic_state -> Incidence
  epidemic_state -> wis

  Horizon -> wis
}')

# adjustmentSets(dag, effect = "total")

# adjustmentSets(dag, effect = "direct")

# impliedConditionalIndependencies(dag)

# paths(dag, from = "Method", to = "wis")

p <- ggdag_status(dag, text = FALSE, use_labels = "name") +
  theme_dag()

ggsave("report/_quarto/supplement/dag.pdf", p, width = 8, height = 6)
