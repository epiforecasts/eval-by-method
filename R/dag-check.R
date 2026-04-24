library(dagitty)
library(ggdag)
library(ggplot2)

dag <- dagitty('dag {
  structure [exposure]
  wis [outcome]
  team_resources [latent]
  policy [latent]
  immunity [latent]
  
  team_resources -> structure
  team_resources -> country_targets
  
  country_targets -> structure
  country_targets -> wis

  epi_target -> wis
  epi_target -> structure

  trend -> wis
  horizon -> wis

  location -> variant
  variant -> wis
  location -> wis

  structure -> wis
}')

cat("\n--- Minimal adjustment sets for TOTAL effect of structure on wis ---\n")
print(adjustmentSets(dag, effect = "total"))

cat("\n--- Minimal adjustment sets for DIRECT effect of structure on wis ---\n")
print(adjustmentSets(dag, effect = "direct"))

cat("\n--- Implied conditional independencies ---\n")
print(impliedConditionalIndependencies(dag))

cat("\n--- All paths from structure to wis (open/closed unconditional) ---\n")
print(paths(dag, from = "structure", to = "wis"))

cat("\n--- Paths given the total-effect adjustment set ---\n")
adj <- adjustmentSets(dag, effect = "total")
if (length(adj) > 0) {
  print(paths(dag, from = "structure", to = "wis", Z = adj[[1]]))
}

p <- ggdag_status(dag, text = FALSE, use_labels = "name") +
  theme_dag()

dir.create("report/supplement", recursive = TRUE, showWarnings = FALSE)
ggsave("report/supplement/dag.pdf", p, width = 8, height = 6)
cat("\nDAG figure saved to report/supplement/dag.pdf\n")
