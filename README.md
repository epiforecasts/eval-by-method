

[![Zenodo](https://img.shields.io/badge/Code%20DOI-10.5281/zenodo.14903161-blue)](https://doi.org/10.5281/zenodo.14903161)
[![medRxiv](https://img.shields.io/badge/medRxiv-10.1101/2025.04.10.25325611-blue)](https://doi.org/10.1101/2025.04.10.25325611)

## Evaluating model structures among European COVID-19 forecasts

Katharine Sherratt (1), Rok Grah (2), Bastian Prasse (2), Friederike
Becker (3), Jamie McLean (1), Sam Abbott (1), Sebastian Funk (1)

1)  Centre for Mathematical Modelling of Infectious Diseases, London
    School of Hygiene & Tropical Medicine
2)  European Centre for Disease Prevention and Control
3)  Institute of Statistics, Karlsruhe Institute of Technology

#### Overview

- A [slide
  deck](https://docs.google.com/presentation/d/1BSdTEuZ_zKdU8tBFuRMmP7GwHht1D0oZSkaFWovz9ao/edit?slide=id.p#slide=id.p)
  offers high level context for what we were interested in, what we did,
  and what we found.

#### Summary

- Accurately predicting the spread of infectious disease is essential to
  supporting public health during outbreaks. However, comparing the
  accuracy of different forecasting models is challenging. Existing
  evaluations struggle to isolate the impact of model design choices
  (like model structure, or specificity to the forecast target) from the
  inherent difficulty of predicting complex outbreak dynamics. Our
  research moves towards a more principled approach to systematically
  adjusting for common factors affecting epidemiological forecasts,
  accounting for multi-layered and non-linear effects on predictive
  difficulty.

- We applied this approach to 181,851 probabilistic predictions from 47
  models submitted to the European COVID-19 Forecast Hub between March
  2021 and March 2023. We classified models by structure (agent-based,
  mechanistic, semi-mechanistic, statistical, or human judgement) and by
  target strategy (forecasting one or multiple countries). We adjusted
  for forecast horizon, epidemic trend, dominant variant phase, country
  location, and individual model variation, isolating the impact of
  model structure on predictive performance.

- Our findings suggest that after adjustment, apparent differences in
  performance between model structures became minimal. Models
  forecasting a single geographic target showed some indication of
  better performance than those forecasting multiple targets, though
  with overlapping uncertainty. Substantial residual variation between
  individual models remained unexplained by our adjustment. Our work
  highlights the importance of accounting for predictive difficulty when
  evaluating across forecasting models, and provides a framework for
  more robust evaluations of infectious disease predictions.

- Read the pre-print:
  [medRxiv](https://doi.org/10.1101/2025.04.10.25325611)

------------------------------------------------------------------------

### Project docs

- Current [working
  draft](https://docs.google.com/document/d/1OOVUHR_BGWcviSNxvaHvbXD16Bb3Y_zhw--7gAGBqMk/edit#)
- Submitted
  [manuscript](https://docs.google.com/document/d/1B_HviobjSIak4c1FKoSOqgFwRF_DxMrJM6zsdbdth4E/edit?tab=t.0)
- [Authorship](https://docs.google.com/spreadsheets/d/18mt6c47MCzLdMAKth6Bv2PC7b10KYgWieExHHWLnP3Q/edit?gid=0#gid=0)

------------------------------------------------------------------------

### Getting started

#### Code environment

Packages are managed using
[renv](https://rstudio.github.io/renv/articles/renv.html). In order to
install all the required packages, install the `renv` package and run

``` r
renv::restore()
```

#### Data

All the data used in the analysis is stored in the `data/` directory. It
has been obtained from public sources. To re-download the data in the
`data/` directory, see the [data/README](data/README.md).

#### Analyses

In order to re-generate the forecast scores, use

``` r
## Score forecasts & ensembles on the log and natural scales; save to data/
source(here("R", "process-score.R"))
```

In order to run the GAM on the scores, use

``` r
## Model the weighted interval score; save to data/
source(here("R", "analysis-model.R"))
```

#### Results

The full manuscript (background, methods, results, discussion) is
written in [report/manuscript.qmd](./report/manuscript.qmd), which
sources the R scripts and assembles section files from
[report/quarto/](./report/quarto/). The supplement is in
[report/quarto/supplement/\_supplement.qmd](./report/quarto/supplement/_supplement.qmd).

Re-render the manuscript:

``` r
quarto::quarto_render("report/manuscript.qmd")
```
