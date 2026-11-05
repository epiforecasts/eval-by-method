# R/

Analysis scripts for evaluating COVID-19 forecast accuracy across models. Scripts fall into two categories: pipeline scripts run in sequence to produce results, and utility scripts called by the pipeline.

## Pipeline scripts

Run in this order to reproduce the analysis:

| Script | Purpose | Inputs | Outputs |
|---|---|---|---|
| `process-score.R` | Score forecasts using WIS on natural and log scales; normalises predictions and observations to per-100,000 population | `data/covid19-forecast-hub-europe.parquet`, `data/observed-*.csv`, `data/populations.csv` | `data/scores-raw-case.csv`, `data/scores-raw-death.csv` |
| `process-data.R` | Join scores with covariates (model classification, variant phase, observed incidence, trend) into a single analysis-ready dataframe | `data/scores-raw-*.csv`, `data/observed-*.csv`, `data/model-classification.csv` | In-memory dataframe via `process_data()` |
| `analysis-descriptive.R` | Descriptive summaries: bootstrap confidence intervals, publication-ready Table 1, score distribution ridge plots, time-series plots | `process_data()` output | Plot and table objects |
| `analysis-model.R` | Defines `m.formula_joint` and `model_wis()`, which fits the GAMM to WIS (joint and univariate models) for one scoring scale. Sourcing fits nothing; call `model_wis()` once per scale | `process_data()` output | Per scale, under `output/{log,natural}/`: `results.rds`, `fit_obs.rds`, `plots/check_joint.png`. With `spec_label`, also a row in `output/diagnostics/fit-summary.csv` |
| `analysis-supplement.R` | Supplement tables and figures: confidence intervals, target counts, share of single-country models per method, WIS components | `process_data()` output, `output/diagnostics/fit-summary.csv` | Table and plot objects |
| `dag-check.R` | Define and visualise causal DAG; derive minimal adjustment set for the exposure–outcome relationship | — | Plot objects |
| `plot-model-flow.R` | Generate model inclusion/exclusion flowchart applying sequential eligibility criteria | `data/covid19-forecast-hub-europe.parquet` | `output/flowchart.png`, `output/model-flow-counts.csv` |
| `plot-model-results.R` | Forest plots of effects (unadjusted and adjusted) by method, target scope and model; model ranks | `output/{log,natural}/results.rds` | ggplot objects |

`results.rds` holds `data` (the fitted data), `effects` (adjusted and unadjusted effects), `method_by_target` (one row per Method x Epi_target cell), `model_h1` (model effects at horizon 1), `checks` (`k.check()`) and `formula`.

## Utility scripts

Called by pipeline scripts; not intended to be run directly.

| Script | Key functions | Purpose |
|---|---|---|
| `utils-data.R` | `get_forecasts()`, `download_obs()`, `download_pop()` | Read forecast parquet, download observed incidence and population data from JHU/ECDC sources |
| `utils-metadata.R` | `get_metadata_processed()`, `write_metadata()` | Fetch model metadata from GitHub; read and write qualitative model classifications to/from Google Sheets |
| `utils-variants.R` | `classify_variant_phases()` | Classify dominant SARS-CoV-2 variant per country-week using ECDC, UK UKHSA, and Swiss surveillance data |
| `utils-effects.R` | `extract_ranef_terms()`, `method_pooled_effects()`, `method_target_effects()`, `model_horizon_effects()` | Extract random effects and linear contrasts (with intervals from the full posterior covariance) from a fitted GAMM |

## Sensitivity scripts (`R/sensitivity/`)

Robustness checks reported in the supplement.

| Script | Purpose |
|---|---|
| `check-family.R` | Response family and link (Gaussian, Gamma, Tweedie) |
| `check-family-interaction.R` | Method x target estimates refitted under a Gaussian family; written to `output/diagnostics/method-by-target-gaussian.csv` |
| `check-horizon-spec.R`, `check-horizon-spec.qmd` | How each model's horizon curve is specified; fits cached to `output/sensitivity/horizon-spec/` |
| `check-link-robustness.R` | Log vs identity link |
| `check-autocorrelation.R` | Temporal autocorrelation diagnostic |

## Try a model spec

| File | Purpose |
|---|---|
| `diy-model.qmd` | Refit the GAMM under alternative specifications and compare effects. Not part of the site render |
| `utils-diy.R` | `load_diy_data()` subsamples the saved analysis data within each model; `fit_spec()` fits one formula and caches the result to `output/diy/`; `compare_specs()` plots effects across fits |

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
                └──► analysis-model.R ────────► output/{log,natural}/results.rds
                                                output/{log,natural}/fit_obs.rds
                                                        │
                                                        ├──► plot-model-results.R ── forest plots
                                                        │
                                                        └──► diy-model.qmd ── refits on a subsample
```

## Model specification

The main model (`analysis-model.R`) fits a GAMM to WIS via `mgcv::bam()`, with a Tweedie family and log link, fitted by fREML with `discrete = TRUE`. The joint formula `m.formula_joint` contains:

- `s(Method, Epi_target, bs = "re")`: model structure by epidemiological target; the pooled structure effect is a contrast across cells (`method_pooled_effects()`)
- `Epi_target`: cases vs deaths, fixed effect
- `s(CountryTargets, bs = "re")`: single- vs multi-country models
- `s(Incidence)`, `s(Trend, bs = "re")`, `s(Location, bs = "re")`, `s(VariantPhase, bs = "re")`: adjustment for epidemic setting
- `s(Horizon, k = 4)` plus `s(Horizon, Model, k = 4, bs = "fs")`: a shared horizon curve, with each model as a penalised deviation from it; a model's effect is that deviation averaged over the horizons it forecast (`model_horizon_effects()`)

The comments beside `m.formula_joint` in `analysis-model.R` give the reasoning for each term. Univariate models (one term each) give unadjusted estimates; the joint model gives adjusted estimates. Cases and deaths are fitted together; scoring scales (log, natural) are fitted separately.
