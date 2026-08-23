

# Model WIS as a result of dispersion, underprediction, overprediction
# brms
# tidyverse convention: broom
# assume linear
library(brms)
library(broom.mixed)
library(bayesplot)

decompose_wis <- brm(
  formula = wis ~ dispersion + underprediction + overprediction + (1|Model),
  data = scores,
  family = Tweedie(),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 500
)
