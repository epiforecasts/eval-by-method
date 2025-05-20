[![DOI](https://zenodo.org/badge/644420305.svg)](https://doi.org/10.5281/zenodo.14903161)

## The influence of model structure and geographic specificity on forecast accuracy among European COVID-19 forecasts

Katharine Sherratt (1), Rok Grah (2), Bastian Prasse (2), Friederike Becker (3), Jamie McLean (1), Sam Abbott (1), Sebastian Funk (1)

(1) Centre for Mathematical Modelling of Infectious Diseases, London School of Hygiene & Tropical Medicine
(2) European Centre for Disease Prevention and Control
(3) Institute of Statistics, Karlsruhe Institute of Technology


### Abstract

The predictive accuracy of infectious disease forecasts varies in time, space and between models. When building models for prediction, forecasters have the choice of a range of underlying model structures with more or less ability to tune a particular model to the forecasting target. However, it has been difficult to compare the effect of these choices due to a lack of standardised forecast reporting and evaluation. Here, we used prospectively collected, standardised, open data from the European COVID-19 Forecast Hub to investigate model-specific factors that might influence forecast performance. 

We evaluated 1-4 week ahead forecasts of COVID-19 cases and deaths for 32 countries between 2021 and 2023. We categorised 47 models by their structure: agent-based, mechanistic, semi-mechanistic, statistical or other; and by their specificity to a geographic location: whether a forecaster predicted outcomes for one country or many. We assessed forecast performance using the weighted interval score after log-transforming both forecasts and observations. We used a generalised additive mixed model to explore performance, additionally accounting for changes between countries over time, the epidemiological situation, the forecast horizon, and variation among models. 

We observed some small differences between model types, with statistical models slightly outperforming other types when forecasting deaths, but with widely overlapping confidence intervals. We further found that those that forecast for single countries outperformed those forecasting multiple targets, however again confidence intervals of the corresponding estimates overlapped widely. 

Whilst we found no clear effects, we showed that multi-model forecasting efforts are a useful source for more generalised model-based analysis of predictive performance. Our work was limited by a small sample size of independent models. We recommend that multi-model comparisons encourage methodological diversity to enable future studies of factors that drive predictive performance, ensuring that detailed information on methodology is collated alongside the numerical predictions.

Current working draft: -
<https://docs.google.com/document/d/1OOVUHR_BGWcviSNxvaHvbXD16Bb3Y_zhw--7gAGBqMk/edit#>

### Environment

Packages are managed using
[renv](https://rstudio.github.io/renv/articles/renv.html). In order to
install all the required packages, install the `renv` package and run

``` r
renv::restore()
```

### Data

All the data used in the analysis is stored in the `data/` directory. It
has been obtained from public sources. In order to re-download the data
in the `data/` directory, use

``` r
## Get metadata from googlesheet; save to data/
source(here("R", "get-metadata.R"))

## Get observed data and all Hub forecasts; exclude forecasts; save to data/
source(here("R", "import-data.R"))
```

### Analyses

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

### Results

View results:

- [Results](./report/results.pdf)
- [Supplement](./report/supplement/supplement.pdf)

Re-generate results pdf:

- [results.Rmd](./report/results.Rmd)
- [supplement.Rmd](./report/supplement/supplement.Rmd)
