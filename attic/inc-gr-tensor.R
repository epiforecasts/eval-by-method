# sandbox: does the difficulty surface differ in shape by outcome?
library(here)
library(tidyverse)
library(mgcv)
library(gratia)

source(here("R", "process-data.R"))

scores <- process_data(scoring_scale = "log") |>
  filter(!grepl("EuroCOVIDhub-ensemble", Model)) |>
  filter(Horizon == 1)

# Growth: weekly log change in incidence, within country and outcome

weekly <- scores |>
  distinct(Location, epi_target, target_end_date, Incidence, pop) |>
  arrange(Location, epi_target, target_end_date) |>
  group_by(Location, epi_target) |>
  mutate(Growth = log((Incidence + 0.5) / (lag(Incidence) + 0.5))) |>
  ungroup() |>
  select(Location, epi_target, target_end_date, Growth)

d <- scores |>
  left_join(weekly, by = c("Location", "epi_target", "target_end_date")) |>
  mutate(
    Incidence = log((Incidence + 0.5) / pop * 1e5),
    Epi_target = factor(epi_target),
    Epi_target_ord = factor(epi_target, ordered = TRUE),
    Model = factor(Model),
    Location = factor(Location)
  ) |>
  filter(is.finite(wis), is.finite(Incidence), is.finite(Growth))


# -------------------------------------------------------------------------


base <- ~ Epi_target +
  s(Method, Epi_target, bs = "re") +
  s(CountryTargets, bs = "re") +
  s(Location, bs = "re") +
  s(VariantPhase, bs = "re") +
  s(Model, bs = "re")

fit <- function(rhs) {
  bam(
    formula = update(rhs, wis ~ .),
    data = d,
    family = tw(link = "log"),
    discrete = TRUE,
    nthreads = 4
  )
}

# 1. one surface, outcome as intercept shift only
m_shared <- fit(update(base, ~ . + te(Incidence, Growth, k = c(5, 5))))

# 2. separate surface per outcome
m_by <- fit(update(base, ~ . + te(Incidence, Growth, by = Epi_target, k = c(5, 5))))

# 3. reference surface + explicit difference surface (deaths - cases)
m_diff <- fit(update(base, ~ . +
                       te(Incidence, Growth, k = c(5, 5)) +
                       te(Incidence, Growth, by = Epi_target_ord, k = c(5, 5))))

te(Incidence, Growth, by = Epi_target_ord, k = c(5, 5))

AIC(m_shared, m_by, m_diff)

# The difference surface in m_diff is the direct answer: if its EDF shrinks toward zero and the surface is flat at zero, the shapes are the same and m_shared wins.

summary(m_diff)          # EDF + p of the by-Epi_target_ord term
draw(m_by, dist = 0.05)  # dist masks bins with no nearby data
draw(m_diff, dist = 0.05)

# plot on the response scale
sm <- smooth_estimates(m_by) |>
  filter(str_detect(.smooth, "^te")) |>
  mutate(ratio = exp(.estimate))   # multiplicative effect on LWIS

ggplot(sm, aes(x = Incidence, y = Growth, fill = ratio)) +
  geom_raster() +
  geom_contour(aes(z = ratio), colour = "white", linewidth = 0.2) +
  scale_fill_gradient2(trans = "log10", midpoint = 0, name = "LWIS multiplier") +
  facet_wrap(~Epi_target) +
  labs(x = "log(incidence)", y = "Weekly log growth")


# -------------------------------------------------------------------------

m_target <- bam(
  wis ~
    Epi_target +
    te(Incidence, Growth, by = Epi_target, k = c(5, 5)) +
    # s(Incidence, by = Epi_target) + s(Trend, bs = "re") +
    # s(Horizon, by = Epi_target, k = 3) +
    s(VariantPhase, bs = "re") +
    s(Location, bs = "re"),
  data = d,
  family = tw(link = "log"),
  discrete = TRUE
)

summary(m_target)
draw(m_target, dist = 0.05)

m_plus_model <- update(m_target, . ~ . + s(Model, bs = "re"))
m_plus_method <- update(m_target, . ~ . + s(Method, Epi_target, bs = "re"))

map_dbl(
  list(target = m_target, plus_model = m_plus_model, plus_method = m_plus_method),
  \(m) summary(m)$dev.expl
)
