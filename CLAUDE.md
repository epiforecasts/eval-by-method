# Project Overview

This is an R-based research project analyzing COVID-19 forecast accuracy across European models. The study examines how model structure and geographic specificity influence forecast performance using data from the European COVID-19 Forecast Hub.

## Research Question

How do model structure (mechanistic vs statistical) and geographic specificity (single-location vs multi-location models) affect forecast accuracy after adjusting for predictive difficulty?

See `report/Research-narrative.md` for additional project context.

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
  - Outputs: `output/results.rds`

- **analysis-descriptive.R**: Descriptive statistics and summary tables
  - Bootstrap confidence intervals
  - Score distributions by model characteristics

- **plot-model-results.R**: Visualization of GAMM model effects
  - Adjusted vs unadjusted effects by model
  - Supports anonymized output for peer review

- **plot-model-flow.R**: Workflow and flowchart visualizations

### Utility Scripts (R/)

- **utils-data.R**: Functions for accessing forecasts, observations, and population data
- **utils-metadata.R**: Model names, submissions, and metadata classification helpers
- **utils-variants.R**: COVID-19 variant phase classification

### Data (data/)

- `covid19-forecast-hub-europe.parquet`: Raw forecast submissions
- `observed-{case|death}.csv`: Observed incidence data
- `model-classification.csv`: Model categorization by structure and specificity
- `populations.csv`: Population data by location
- `scores-raw-{case|death}.csv`: Computed forecast scores (generated)

### Manuscript text (prose and writing)

- `report/Revision_manuscript.md` — full manuscript text (title, abstract, background, methods, results, discussion). **Edit this file for any writing changes.**
- `report/Research-narrative.md` — Overall narrative of the research, and paragraph-by-paragraph one-line summary of the manuscript text
- `submission/reviewer-response-analysis.md` — tracks reviewer suggestions and planned response; X marks completion. Consult when making revision-related changes.

### Rendered analysis (code and outputs)

- `report/results.qmd` — active Quarto document; sources R scripts and renders figures/tables for the results section
- `report/supplement/Supplement.Rmd` — supplementary materials; sources the same R scripts
- `report/results.Rmd` — legacy RMarkdown copy of results (inactive; use `.qmd`)
- Pre-print: [medRxiv 10.1101/2025.04.10.25325611](https://doi.org/10.1101/2025.04.10.25325611)

**Note**: manuscript prose and rendered analysis are separate. `Revision_manuscript.md` is not auto-generated — changes to analysis code and changes to Manuscript text must be coordinated manually.

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

# 3. Fit GAMM to weighted interval scores
source(here("R", "analysis-model.R"))

# 4. Generate reports
# Render report/results.qmd
# Knit report/supplement/Supplement.Rmd
```

## Making Changes

| Task | Where to edit |
|---|---|
| Change manuscript prose (wording, framing, conclusions) | `report/Revision_manuscript.md` + update in `Research-narrative.md` |
| Change analysis, model, or figures | Relevant `R/` script; outputs flow into `results.qmd` automatically |
| Respond to a reviewer comment | Check `report/Revision_reviews-response.md`, update `R/` script if needed, then update `report/Revision_manuscript.md`, mark as completed in `report/Revision_reviews-response.md`, and close the relevant Github Issue with a note |
| Add or change a supplementary figure | Relevant `R/` script + `report/supplement/Supplement.Rmd` |
| All changes | Update `Plan.md` |

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

## Publications

- **DOI**: [10.5281/zenodo.14903161](https://doi.org/10.5281/zenodo.14903161)
- **Pre-print**: [10.1101/2025.04.10.25325611](https://doi.org/10.1101/2025.04.10.25325611)
- **Slides**: [Google Slides](https://docs.google.com/presentation/d/1BSdTEuZ_zKdU8tBFuRMmP7GwHht1D0oZSkaFWovz9ao/edit?slide=id.p)
