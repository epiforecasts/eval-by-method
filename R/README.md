# R/

Analysis scripts for evaluating COVID-19 forecast accuracy across models. Scripts fall into two categories: pipeline scripts run in sequence to produce results, and utility scripts called by the pipeline.

## Pipeline scripts

Run in this order to reproduce the analysis:

| Script | Purpose | Inputs | Outputs |
|---|---|---|---|
| `process-score.R` | Score forecasts using WIS on natural and log scales; normalises predictions and observations to per-100,000 population | `data/covid19-forecast-hub-europe.parquet`, `data/observed-*.csv`, `data/populations.csv` | `data/scores-raw-case.csv`, `data/scores-raw-death.csv` |
| `process-data.R` | Join scores with covariates (model classification, variant phase, observed incidence, trend) into a single analysis-ready dataframe | `data/scores-raw-*.csv`, `data/observed-*.csv`, `data/model-classification.csv` | In-memory dataframe via `process_data()` |
| `analysis-descriptive.R` | Descriptive summaries: bootstrap confidence intervals, publication-ready Table 1, score distribution ridge plots, time-series plots | `process_data()` output | Plot and table objects |
| `analysis-model.R` | Fit GAMM to WIS isolating effects of model structure (`Method`) and geographic scope (`CountryTargets`) after adjusting for confounders | `process_data()` output | Per scale, under `output/{log,log-resp,natural}/`: `results.rds`, `fit_obs.rds`, diagnostic plots |
| `dag-check.R` | Define and visualise causal DAG; derive minimal adjustment set for the exposure–outcome relationship | — | Plot objects |
| `plot-model-flow.R` | Generate model inclusion/exclusion flowchart applying sequential eligibility criteria | `data/covid19-forecast-hub-europe.parquet` | `output/plots/flowchart.png` |
| `plot-model-results.R` | Forest plots of GAMM random effects (unadjusted and adjusted) by model, method, and target scope | `output/results.rds`, `process_data()` output | ggplot objects |

## Utility scripts

Called by pipeline scripts; not intended to be run directly.

| Script | Key functions | Purpose |
|---|---|---|
| `utils-data.R` | `get_forecasts()`, `download_obs()`, `download_pop()` | Read forecast parquet, download observed incidence and population data from JHU/ECDC sources |
| `utils-metadata.R` | `get_metadata_processed()`, `write_metadata()` | Fetch model metadata from GitHub; read and write qualitative model classifications to/from Google Sheets |
| `utils-variants.R` | `classify_variant_phases()` | Classify dominant SARS-CoV-2 variant per country-week using ECDC, UK UKHSA, and Swiss surveillance data |

## Sensitivity scripts (`R/sensitivity/`)

Robustness checks reported in the supplement.

| Script | Purpose |
|---|---|
| `check-autocorrelation.R` | Temporal autocorrelation diagnostic |
| `check-link-robustness.R` | Robustness of results to the model link function |
| `model-logresp.R` | Log-response sensitivity arm of the GAMM |
| `model-building.qmd` | Notebook documenting the model specifications tried |

## Data flow

```
covid19-forecast-hub-europe.parquet
        │
        ▼
utils-data.R ──────────────────────────────► data/observed-*.csv
        │                                    data/populations.csv
        │
        ▼
process-score.R ───────────────────────────► data/scores-raw-case.csv
        │                                    data/scores-raw-death.csv
        │
        ├── data/model-classification.csv  ◄── utils-metadata.R (Google Sheets)
        ├── data/observed-*.csv
        └── utils-variants.R (ECDC/UKHSA/CH)
                │
                ▼
        process-data.R ── process_data() ──► in-memory analysis dataframe
                │
                ├──► analysis-descriptive.R ── tables, plots
                │
                └──► analysis-model.R ────────► output/{log,log-resp,natural}/results.rds
                                                output/{log,log-resp,natural}/fit_obs.rds
                                                        │
                                                        ▼
                                              plot-model-results.R ── forest plots
```

## Model specification

The main model (`analysis-model.R`) fits a GAMM via `mgcv::bam()`:

- **Family:** Gaussian with log link
- **Fitting:** fREML with `discrete = TRUE`
- **Fixed effects of interest:** `Method` (model structure), `CountryTargets` (geographic scope)
- **Adjusting for:** `Incidence` (cubic spline), `Trend`, `Horizon`, `VariantPhase`, `Location`, `Model`
- **Random effects basis:** `re` for Location, VariantPhase, and Model
- **Outcomes:** cases and deaths scored separately on natural and log scales

Univariate models are fitted first (one term each) to obtain unadjusted estimates; a joint model containing all terms yields adjusted estimates. Both are saved per scale to `output/{log,log-resp,natural}/results.rds`.
