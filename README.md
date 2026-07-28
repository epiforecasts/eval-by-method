

[![Zenodo](https://img.shields.io/badge/Code%20DOI-10.5281/zenodo.14903161-blue)](https://doi.org/10.5281/zenodo.14903161)
[![medRxiv](https://img.shields.io/badge/medRxiv-10.1101/2025.04.10.25325611-blue)](https://doi.org/10.1101/2025.04.10.25325611)

## Using model-based evaluation to interpret variation in infectious disease forecast performance

Katharine Sherratt (1), Rok Grah (2), Bastian Prasse (2), Friederike
Becker (3), Jamie McLean (1), Sam Abbott (1), Sebastian Funk (1)

1)  Centre for Mathematical Modelling of Infectious Diseases, London
    School of Hygiene & Tropical Medicine
2)  European Centre for Disease Prevention and Control
3)  Institute of Statistics, Karlsruhe Institute of Technology

<details>

<summary>

Abstract
</summary>

Forecasters predicting infectious disease outbreaks have met with
varying success. Some of this variation in performance comes from the
method used to make a forecast, when different models are better or
worse at prediction. The rest comes from the target being forecast, when
some outbreaks are easier or harder to predict than others. However,
when many forecasters each predict many different targets, it becomes
difficult to trace the impact of these factors shaping performance. Here
we use a regression model to separate the effect of the forecasting
method, from the difficulty of the target, in forecast performance.

We evaluated forecasts of weekly COVID-19 cases and deaths over two
years across 32 European countries, scoring them against observed data
with the Weighted Interval Score (WIS). We expected a model’s structure
to shape how well it predicted, so we classified 48 models by structure
(agent-based, mechanistic, semi-mechanistic, statistical, or human
judgement) and estimated how much structure alone affected performance.
A generalised additive mixed model let us adjust for everything that
makes a target easier or harder to predict: the outcome being forecast,
its level and trend, the dominant variant, the country, the forecast
horizon, and differences between individual models.

Once we accounted for the difficulty of the target, no single type of
model performed best. That overall result concealed structures that
predicted one outcome relatively well and the other relatively poorly,
differences that cancelled out when we averaged across case and death
forecasts. Differences in European COVID-19 forecast performance were
driven more by which targets were hard to predict than by which
modelling approach a forecaster used.

This approach sits between informal and fully formal ways of handling
bias in evaluation studies. As infectious disease forecasting grows, we
encourage evaluators to choose from a wider range of study designs,
matching the formality of the method to the question, so they can
isolate the part of performance they actually want to measure.

</details>

------------------------------------------------------------------------

### Getting started

#### Read

Read the work as it stands:

- The full paper (background, methods, results, discussion) is in
  [report/manuscript.qmd](./report/manuscript.qmd), which assembles the
  section files in [report/quarto/](./report/quarto/).
- The supplement is in
  [report/quarto/supplement/\_supplement.qmd](./report/quarto/supplement/_supplement.qmd).
- The former pre-print is on
  [medRxiv](https://doi.org/10.1101/2025.04.10.25325611); note, this is
  out of date.

#### Reproduce

To re-run the analysis end to end, without editing anything:

1.  Restore the package environment
    ([renv](https://rstudio.github.io/renv/articles/renv.html)):

    ``` r
       renv::restore()
    ```

2.  Data is already in `data/`. To re-download it from public sources,
    see the [data/README](data/README.md).

3.  Score forecasts on the log and natural scales (writes to `data/`):

    ``` r
       source(here("R", "process-score.R"))
    ```

4.  Assemble scores with explanatory variables:

    ``` r
       source(here("R", "process-data.R"))
    ```

5.  Fit the GAMM to the weighted interval score (writes to `output/`):

    ``` r
       source(here("R", "analysis-model.R"))
    ```

6.  Render the manuscript (includes the results section and supplement):

    ``` r
       quarto::quarto_render("report/manuscript.qmd")
    ```

    `analysis-model.R` must be run before rendering — it is not sourced
    by the results section.

#### Explore

A guide to the codebase:

- `R/` — analysis and utility scripts:
  - `process-score.R` scores forecasts; `process-data.R` joins scores to
    explanatory variables; `analysis-model.R` fits the GAMM;
    `analysis-descriptive.R` builds summary tables;
    `plot-model-results.R` and `plot-model-flow.R` make the figures;
    `dag-check.R` defines the confounding DAG.
  - `utils-data.R`, `utils-metadata.R`, `utils-variants.R` — data
    access, model metadata, and variant-phase classification.
  - `R/sensitivity/` — robustness checks (autocorrelation, link
    function, log-response model, model-building notebook).
- `data/` — forecasts (`covid19-forecast-hub-europe.parquet`), observed
  incidence, populations, model classification, and computed scores.
- `report/quarto/` — the manuscript section files;
  `report/quarto/supplement/` the supplement.
- `output/` — fitted model results and diagnostic plots, under
  scale-named subdirectories (`log/`, `log-resp/`, `natural/`).

------------------------------------------------------------------------

### Project docs

- [Authorship](https://docs.google.com/spreadsheets/d/18mt6c47MCzLdMAKth6Bv2PC7b10KYgWieExHHWLnP3Q/edit?gid=0#gid=0)
- [Slide
  deck](https://docs.google.com/presentation/d/1BSdTEuZ_zKdU8tBFuRMmP7GwHht1D0oZSkaFWovz9ao/edit?slide=id.p#slide=id.p)
  — high-level context
