# Define DAG for model structure and WIS
# Variables:
# model_structure: model structure (exposure)
# wis: weighted interval score (outcome)
# team_resources: resources available to the team (latent)
# transmission_dynamics: underlying transmission dynamics (latent)
# country_targets: whether the model forecasts for one or multiple countries
# variant: dominant variant phase
# country: country target location
# trend: increasing, decreasing, or stable epidemic trend
# horizon: forecast horizon

library(dagitty)
library(ggdag)
library(ggplot2)

dag <- dagitty('dag {
  model_structure[exposure]
  wis [outcome]
  model_team_resources[latent]
  transmission_dynamics[latent]
  
  model_team_resources -> model_structure
  model_team_resources -> wis
  model_team_resources -> country_targets
  
  model_country_targets -> model_structure
  model_country_targets -> wis

  country -> variant
  variant -> transmission_dynamics
  transmission_dynamics -> country
  transmission_dynamics -> trend

  transmission_dynamics -> wis
  variant -> wis
  trend -> wis
  country -> wis
  horizon -> wis

  model_structure -> wis
}')

cat("\n--- Minimal adjustment sets for TOTAL effect of model_structure on wis ---\n")
print(adjustmentSets(dag, effect = "total"))

cat("\n--- Minimal adjustment sets for DIRECT effect of model_structure on wis ---\n")
print(adjustmentSets(dag, effect = "direct"))

cat("\n--- Implied conditional independencies ---\n")
print(impliedConditionalIndependencies(dag))

cat("\n--- All paths from model_structure to wis (open/closed unconditional) ---\n")
print(paths(dag, from = "model_structure", to = "wis"))

cat("\n--- Paths given the total-effect adjustment set ---\n")
adj <- adjustmentSets(dag, effect = "total")
if (length(adj) > 0) {
  print(paths(dag, from = "model_structure", to = "wis", Z = adj[[1]]))
}

p <- ggdag_status(dag, text = FALSE, use_labels = "name") +
  theme_dag()

dir.create("report/supplement", recursive = TRUE, showWarnings = FALSE)
ggsave("report/supplement/dag.pdf", p, width = 8, height = 6)
cat("\nDAG figure saved to report/supplement/dag.pdf\n")
