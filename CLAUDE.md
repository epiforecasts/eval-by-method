# Project Overview

This is an R-based research project analysing COVID-19 forecast accuracy across European models.

## Abstract

Forecasters predicting infectious disease outbreaks have met with varying success.
Some of this variation in performance comes from the method used to make a forecast, when different models are better or worse at prediction.
The rest comes from the target being forecast, when some outbreaks are easier or harder to predict than others.
However, when many forecasters each predict many different targets, it becomes difficult to trace the impact of these factors shaping performance.
Here we use a regression model to separate the effect of the forecasting method, from the difficulty of the target, in forecast performance.

We evaluated forecasts of weekly COVID-19 cases and deaths over two years across 32 European countries, scoring them against observed data with the Weighted Interval Score (WIS).
We expected a model's structure to shape how well it predicted, so we classified 48 models by structure (agent-based, mechanistic, semi-mechanistic, statistical, or human judgement) and estimated how much structure alone affected performance.
A generalised additive mixed model let us adjust for everything that makes a target easier or harder to predict: the outcome being forecast, its level and trend, the dominant variant, the country, the forecast horizon, and differences between individual models.

Once we accounted for the difficulty of the target, no single type of model performed best.
Differences in European COVID-19 forecast performance were driven more by which targets were hard to predict than by which modelling approach a forecaster used.

This approach sits between informal and fully formal ways of handling bias in evaluation studies.
As infectious disease forecasting grows, we encourage evaluators to choose from a wider range of study designs, matching the formality of the method to the question, so they can isolate the part of performance they actually want to measure.


## Project Structure

### Core Analysis Scripts (R/)

- **process-score.R**: Computes forecast scores using the `scoringutils` package
  - Scores forecasts on both natural and log scales
  - Calculates weighted interval scores (WIS)
  - Outputs: `data/scores-raw-{case|death}.csv`

- **process-data.R**: Data preparation and integration
  - Combines scores with explanatory variables (model classification, variant phases, country targets)
  - Calls utility functions for metadata, variants, and location data

- **analysis-model.R**: Main statistical analysis using Generalized Additive Mixed Models (GAMM)
  - Models WIS adjusting for: trend, location, time, horizon, model-specific effects
  - Isolates impact of Method (model structure) and CountryTargets (geographic specificity)
  - Uses `mgcv`, `gammit`, and `gratia` packages
  - Outputs (per scale, under scale-named subdirs of `output/` — `log/`, `natural/`): `results.rds` (includes fitted `data`), `fit_obs.rds`, and diagnostic plots (`plots/check_joint.png`)
  - Defines `model_wis(scoring_scale, family_link = "log", output_dir, spec_label)`; sourcing alone fits nothing. Call it once per scale (`log`, `natural`) to write outputs. Must be run separately before rendering — it is **not** sourced by `report/quarto/_results.qmd`, which only reads `output/log/results.rds`
  - `spec_label` archives that fit's `appraise()` panel and residual/fit statistics under `output/diagnostics/`, upserting a row in `fit-summary.csv` keyed on (`spec_label`, `scale`) so successive model specifications stay comparable. Pass a new label whenever the specification changes.

- **analysis-descriptive.R**: Descriptive statistics and summary tables
  - Bootstrap confidence intervals
  - Score distributions by model characteristics

- **plot-model-results.R**: Visualization of GAMM model effects
  - Adjusted vs unadjusted effects by model
  - Supports anonymized output for peer review

- **plot-model-flow.R**: STROBE-style model-inclusion flowchart (`create_model_flow()` → `output/flowchart.png`)

- **dag-check.R**: Defines and visualises the DAG used to reason about confounding (`ggdag`)

### Sensitivity Scripts (R/sensitivity/)

- **check-autocorrelation.R**: Temporal autocorrelation diagnostic
- **check-link-robustness.R**: Robustness of results to the model link function
- **model-building.qmd**: Notebook documenting model specifications tried

### Utility Scripts (R/)

- **utils-data.R**: Access forecasts, observations, and population data; `download_obs()` / `download_pop()` also write CSVs to `data/`
- **utils-metadata.R**: Model names, submissions, and metadata classification helpers
- **utils-variants.R**: COVID-19 variant phase classification

### Data (data/)

- `covid19-forecast-hub-europe.parquet`: Raw forecast submissions
- `observed-{case|death}.csv`: Observed incidence data
- `model-classification.csv`: Model categorization by structure and specificity
- `populations.csv`: Population data by location
- `scores-raw-{case|death}.csv`: Computed forecast scores (generated)
- `variants/`: Raw variant-surveillance inputs (`ch-hosp.csv`, `ch-wgs.csv`, `eu-ecdc.csv`, `uk-ukhsa.csv`) used by `utils-variants.R`

### Manuscript text (prose and writing)

The manuscript prose lives in per-section Quarto files under `report/quarto/`, assembled by `report/manuscript.qmd`. **Edit the relevant section file for any writing change:**

- `report/quarto/_abstract.qmd` — abstract
- `report/quarto/_background.qmd` — background
- `report/quarto/_methods.qmd` — methods
- `report/quarto/_results.qmd` — results (also holds the analysis code chunks)
- `report/quarto/_discussion.qmd` — discussion
- `report/quarto/_references.qmd` — references
- `submission/Revision_reviews-response.md` — tracks reviewer suggestions and planned response; X marks completion. Consult when making revision-related changes.
- `submission/first/` — archived original submission (manuscript PDF/DOCX, cover letter, supplement, `reviews.md`, and the `results.rds` from that round).

### Rendered analysis (code and outputs)

- `report/manuscript.qmd` — assembles the `report/quarto/_*.qmd` sections. Not itself a render target: its includes are project-root-relative (`/report/quarto/…`), which only resolve when a file at the repo root is the top-level document. Render `index.qmd` instead (see below).
- `report/quarto/_results.qmd` — results section; sources R scripts and renders figures/tables.
- `report/supplement.qmd` — supplementary materials; self-contained, with its own setup chunk. Rendered as its own page, not included in the manuscript.
- Site build (`quarto render` uses `_quarto.yml` → `_site/`): renders `index.qmd` (a thin wrapper including `report/manuscript.qmd`) and `report/supplement.qmd` directly; two-page site with navbar. Bibliography `report/references.bib`, style `report/plos-computational-biology.csl`.
- Pre-print: [medRxiv 10.1101/2025.04.10.25325611](https://doi.org/10.1101/2025.04.10.25325611)

**Note**: manuscript prose and rendered analysis are separate. The section `.qmd` files are not auto-generated — changes to analysis code and changes to manuscript text must be coordinated manually.

## Reproducing the Analysis

### Setup Environment

```r
# Install renv if needed
install.packages("renv")

# Restore package environment
renv::restore()
```

### Run Analysis Pipeline

```r
# 1. Score forecasts on natural and log scales
source(here("R", "process-score.R"))

# 2. Prepare and integrate data
source(here("R", "process-data.R"))

# 3. Fit GAMM to weighted interval scores (run before rendering).
#    Sourcing only DEFINES model_wis(); call it per scale to write output/<scale>/.
source(here("R", "analysis-model.R"))
model_wis(scoring_scale = "log",     output_dir = here("output", "log"),
          spec_label = "tweedie-log")
model_wis(scoring_scale = "natural", output_dir = here("output", "natural"),
          spec_label = "tweedie-log")

# 4. Render the manuscript alone (supplement is a separate page)
#    Render index.qmd, NOT report/manuscript.qmd — the latter's includes are
#    project-root-relative and fail when it is the top-level document.
# quarto render index.qmd

# Or build the full two-page website (manuscript + supplement, with navbar):
# quarto render   # uses root _quarto.yml; renders index.qmd and report/supplement.qmd
```

## Making Changes

| Task | Where to edit |
|---|---|
| Change manuscript prose (wording, framing, conclusions) | Relevant `report/quarto/_*.qmd` section file |
| Change analysis, model, or figures | Relevant `R/` script. `_results.qmd` sources `process-data.R`, `analysis-descriptive.R`, `plot-model-results.R` at render. But `analysis-model.R` and `plot-model-flow.R` are **not** sourced — re-run `model_wis()` per scale (and regenerate the flowchart) to refresh `output/` before rendering |
| Respond to a reviewer comment | Check `submission/Revision_reviews-response.md`, update `R/` script if needed, then update the relevant `report/quarto/_*.qmd`, mark as completed in `submission/Revision_reviews-response.md`, and close the relevant Github Issue with a note |
| Add or change a supplementary figure | Relevant `R/` script + `report/supplement.qmd` |
| All changes | Update `NEWS.md` (change log; newest first) |

## Dependencies

Major R packages:
- `mgcv` - Generalized Additive Models
- `gammit` - GAMM utilities
- `gratia` - GAM plotting
- `scoringutils` - Forecast scoring
- `arrow` - Parquet file handling
- `tidyverse` ecosystem (dplyr, tidyr, ggplot2, readr, purrr)
- `here` - Path management
- `lubridate` - Date handling

## Task list

Outstanding issues.
Status: [ ] not started, [x] done.
- [x] Update manuscript text to clarify what the structure term reports. Superseded by the structure-by-outcome interaction (#158): the pooled effect is now a contrast across a structure's two cells and carries real estimates, so the text reports imprecision rather than a term shrunk to nothing.
- [ ] Model fitting outputs are labelled `primary-interaction` in `output/diagnostics/fit-summary.csv`. Pass that `spec_label` when refitting the primary specification, or the Methods chunk reading the Tweedie power parameter finds no row.
- [x] Untrack the Quarto freeze cache on branch `supplementary-descriptive`. Done on that branch only (commit 6553f40, not pushed): 46 files untracked, `/_freeze/` added to `.gitignore`, `NEWS.md` noted. `main` never tracked the cache.

### Verification
After changing `analysis-model.R` (or upstream scoring/data), regenerate the saved model outputs first — the manuscript reads `output/log/results.rds` and `fit_obs.rds`, it does not re-fit. Stale outputs render silently wrong, or break (e.g. the supplement density chunk needs `results$data`). Then run `quarto render index.qmd` for the manuscript alone, or `quarto render` for the full site, and check figures render correctly.
