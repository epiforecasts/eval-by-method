# Plan: Port "eval-by-method" analysis to US COVID-19 Forecast Hub

## Context

The current project evaluates how model **structure** (mechanistic vs statistical, via `Method`) and **geographic specificity** (single- vs multi-location, via `CountryTargets`) affect COVID forecast accuracy across the **European** hub, after GAMM-adjusting for predictive difficulty. The goal here is to assess feasibility and scope the work to re-run the same analysis on the **US** hub over the same time period (~2021-03 to 2023-03).

Three questions drove this plan; answers up front:

1. **Accessing US forecasts** — Yes, feasible. The original `reichlab/covid19-forecast-hub` was archived (read-only) March 2026 and **re-published in hubverse format** at [`hubverse-org/covid19-forecast-hub-archive`](https://github.com/hubverse-org/covid19-forecast-hub-archive). Queryable via the `hubData` R package (or parquet on AWS S3 `covid19-forecast-hub-archive`). hubverse tooling directly helps. `covidHubUtils::load_forecasts(hub="US")` is the legacy alternative but hubData is the maintained path.
2. **Existing model classification** — **Already done.** `data/model-classification.csv` contains ~80 US models (rows ~66-145: `COVIDhub-*`, `CU-*`, `JHUAPL-*`, `MOBS-GLEAM`, `UMass-*`, `GT-DeepCOVID`, etc.) rated by raters **SF and JM** (2 votes each; KS/RB columns are `NA` for these). `classify_models()` majority vote works with 2 raters. **No re-rating needed**, though a tie-break policy for 2-rater disagreements should be confirmed.
3. **Feasibility** — Moderate. Forecast access + Method classification are essentially free. Main work is: hubData ingestion adapter, FIPS location/population handling, **US variant-phase re-implementation** (the one substantial rewrite), and a target-quantile decision (see Risks).

## Key compatibility findings (EU pipeline requirements vs US archive)

US archive schema (hubverse): `forecast_date, target, horizon, location, target_end_date, output_type, output_type_id, value`.
Targets: `inc death`, `cum death`, `inc case`, `inc hosp`. Locations: FIPS (`"US"`, `"01"–"78"` states/territories, 5-digit counties for cases). Range: 2020-03 to 2024-04.

| EU pipeline needs (`R/utils-data.R:34-56`, `R/process-score.R`) | US archive | Verdict |
|---|---|---|
| 23 quantiles per model/date/location (`utils-data.R:48-55`) | `inc death`/`inc hosp` = 23 quantiles; **`inc case` = only 7** | **Use `inc death` as primary.** Cases would be dropped wholesale by the 23-quantile filter. |
| cols: location, forecast_date, horizon, target_end_date, model, quantile, prediction | hubverse: location, forecast_date, target, horizon, target_end_date, output_type, output_type_id, value | Direct map: `output_type_id`→`quantile`, `value`→`prediction`, filter `output_type=="quantile"`, derive `model` from path/team |
| population per location (per-100k norm, `process-score.R:22`) | FIPS state populations | Swap source (US Census / hubverse `auxiliary-data` locations file) |
| observed/truth weekly (`download_obs`) | JHU truth (same origin as EU) or hubverse target-data | Reusable logic; new URL + FIPS |
| `trend` = 3-wk MA growth ratio (`utils-data.R:95-109`) | computed from observed | **Transfers unchanged** |
| `CountryTargets` = all forecasts single-location? (`process-data.R:56-69`) | computed from scores | **Transfers**, but semantics shift — see Decisions |
| `VariantPhase` (`utils-variants.R`) | EU sources (ECDC/UKHSA/CH) hardcoded | **Full rewrite for US** (CDC variant data) |

## Recommended approach

Port as a **parallel data path**, not a rewrite. Keep R scripts, GAMM formula (`R/analysis-model.R:28-37`), scoring, and `process-data.R` join logic intact. Introduce a `hub` switch so EU and US share one codebase.

**Spike ordering** (deaths only, no write-up): do steps 1→2→3→5 first to get a fitting GAMM with Method + CountryTargets. **Variant phases (step 4) are the long pole** — for the spike, stub `VariantPhase` (single national time covariate, or omit the `s(VariantPhase, bs="re")` term) so the model fits without the CDC rewrite. Build the real US variant source only if the spike succeeds and a full analysis is greenlit.

### 1. Forecast ingestion — new `get_forecasts_us()` (or `hub` arg in `R/utils-data.R`)
- Use `hubData::connect_hub()` / `collect_hub()` against the archive (or read S3 parquet via `arrow`).
- Filter `target == "inc death"`, `output_type == "quantile"`.
- Rename to the EU internal schema: `output_type_id → quantile`, `value → prediction`; derive `model` from the team-model path; compute `forecast_date` (hubverse `forecast_date` is the submission `reference_date`; reconcile with EU's `target_end_date - weeks(horizon) + days(1)` convention).
- Reuse the existing exclusions block unchanged (horizon ≤4, 23-quantile filter, dedup): `R/utils-data.R:42-64`.
- Date window: `2021-03-07`–`2023-03-10` to match EU period (parameterise the hardcoded dates at `utils-data.R:43-44`).

### 2. Truth + population — extend `download_obs()` and populations
- Point truth at US JHU/hubverse target data; keep weekly ISO aggregation and the trend calc (`utils-data.R:78-109`) as-is.
- Replace `populations.csv` source (`utils-data.R:117-120`) with US state FIPS populations.
- Output to `data/observed-death-us.csv`, `data/populations-us.csv`.

### 3. Method classification — reuse existing CSV
- No new file. `classify_models()` (`R/process-data.R:11-38`) already reads the shared `model-classification.csv` which contains the US models.
- **Confirm** the 2-rater (SF/JM) tie-break: current majority logic returns `NA` on ties. Decide whether to add a 3rd rating pass for the (few) US models where SF≠JM, or accept `NA`/document. (See Decisions.)

### 4. Variant phases — `utils-variants.R` US rewrite (largest task)
- Replace ECDC/UKHSA/CH ingestion (`utils-variants.R:171-247`) with **US CDC variant proportion data** (CDC "Variant Proportions" / Nowcast, national + optionally HHS-region/state).
- Keep the **phase-assignment logic** (`utils-variants.R:104-167`): first week each phase >50% dominant, chronological monotonicity, fill-forward grid. The Alpha→BQ/XBB phase taxonomy is broadly US-applicable.
- Drop the Hungary hardcoded override (`:140-153`). US is one country, so VariantPhase becomes near-constant across locations per week (it already is per-location in EU) — acceptable as a national time covariate.

### 5. `process-data.R` / `analysis-model.R` — minimal changes
- `process_data()` joins are schema-driven and transfer; only inputs change (US scores/obs/variants).
- Adjust the ensemble/baseline filter at `R/analysis-model.R:44`: EU uses `EuroCOVIDhub-`; US must exclude `COVIDhub-*` ensembles/baseline (`COVIDhub-ensemble`, `COVIDhub-baseline`, `COVIDhub-4_week_ensemble`, `COVIDhub_CDC-ensemble`, `COVIDhub-trained_ensemble`).
- GAMM formula unchanged. `Location` random effect now spans ~50 states instead of 32 countries — fine for `bs="re"`.

### Critical files
- `R/utils-data.R` — `get_forecasts()` (parquet→hubData), `download_obs()`, population URL, hardcoded dates
- `R/utils-variants.R` — US variant source rewrite (substantial)
- `R/process-data.R` — `classify_models()` reuse; join inputs; ensemble naming
- `R/analysis-model.R:44` — baseline/ensemble exclusion prefix
- `R/process-score.R` — likely unchanged (verify per-100k + log/natural still valid for deaths)
- `data/model-classification.csv` — already contains US models (reuse)
- New data outputs: `scores-raw-death-us.csv`, `observed-death-us.csv`, `populations-us.csv`, US variant CSVs

## Decisions (confirmed with user)
1. **Target: deaths only.** `inc death` (23 quantiles). Cases excluded (only 7 quantiles); hospitalisations out of scope. No branching in the quantile filter needed.
2. **CountryTargets: keep single- vs multi-location as-is.** `CountryTargets` logic transfers unchanged; "multi-location" = multi-**state** in the US. Relabel the concept as geographic scope in any prose, but the research contrast stands.
3. **Scope: feasibility spike.** Ingest → score → fit GAMM on US deaths; confirm it runs and produces sane results. **No manuscript, figures, tables, or supplement** in this pass. De-risk before any full parallel analysis.

## Verification
- Smoke test ingestion: `hubData` connect + collect a single date, confirm 23 quantiles for `inc death`, row counts sane.
- Run pipeline end-to-end on US deaths: `process-score.R` → `process-data.R` → `analysis-model.R`; confirm `output/results-us.rds` produced, GAMM converges, diagnostics (existing supplement `gam.check`/`appraise`) reasonable.
- Sanity-check Method/CountryTargets factor levels populated for US models (no all-`NA` Method column).
- Compare US `COVIDhub-baseline` vs ensemble WIS ordering against published US hub results as an external validity check.
