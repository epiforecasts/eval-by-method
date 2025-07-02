
[![Zenodo](https://img.shields.io/badge/Code%20DOI-10.5281/zenodo.14903161-blue)](https://doi.org/10.5281/zenodo.14903161)
[![medRxiv](https://img.shields.io/badge/medRxiv-10.1101/2025.04.10.25325611-blue)](https://doi.org/10.1101/2025.04.10.25325611)

## The influence of model structure and geographic specificity on forecast accuracy among European COVID-19 forecasts

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
  (like model structure or specificity to the forecast target) from the
  inherent difficulty of predicting complex outbreak dynamics. Our
  research introduces a novel approach to address this by systematically
  adjusting for common factors affecting epidemiological forecasts,
  accounting for multi-layered and non-linear effects on predictive
  difficulty.

- We applied this approach to a large dataset of forecasts from 47
  different models submitted to the European COVID-19 Forecast Hub. We
  adjusted for variation across epidemic dynamics, forecast horizon,
  location, time, and model-specific effects. This allowed us to isolate
  the impact of model structure and geographic specificity on predictive
  performance.

- Our findings suggest that after adjustment, apparent differences in
  performance between model structures became minimal, while models that
  were specific to a single location showed a slight performance
  advantage over multi-location models. Our work highlights the
  importance of considering predictive difficulty when evaluating across
  forecasting models, and provides a framework for more robust
  evaluations of infectious disease predictions.

#### Deep dive

- Read the pre-print:
  [medRxiv](https://doi.org/10.1101/2025.04.10.25325611)
- Current working draft:
  [Docs](https://docs.google.com/document/d/1OOVUHR_BGWcviSNxvaHvbXD16Bb3Y_zhw--7gAGBqMk/edit#)

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
has been obtained from public sources. In order to re-download the data
in the `data/` directory, use

``` r
## Get metadata from googlesheet; save to data/
source(here("R", "get-metadata.R"))

## Get observed data and all Hub forecasts; exclude forecasts; save to data/
source(here("R", "import-data.R"))
```

#### Analyses

In order to re-generate the forecast scores, use

``` r
## Score forecasts & ensembles on the log and natural scales; save to data/
source(here("R", "score.R"))
```

In order to run the GAM on the scores, use

``` r
## Model the weighted interval score; save to data/
source(here("R", "model-wis.R"))
```

#### Results

View results:

- [Results](./report/results.pdf)
- [Supplement](./report/supplement/supplement.pdf)

Re-generate results pdf:

- [results.Rmd](./report/results.Rmd)
- [supplement.Rmd](./report/supplement/supplement.Rmd)
