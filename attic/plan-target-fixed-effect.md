# Plan: data-revision covariate

- Review note: this would link well with the "forecast stability" piece planned separately;
ie. forecaster stability vs target stability (calibration of first report to final data)

**Goal (extension, PLAN ONLY — do not build):** Add a data-revision covariate
sourced from the Hub archive `anomalies.csv`. Scope this section as a documented,
ready-to-implement design; no code or data fetch in this pass.

## Data-revision covariate (PLAN ONLY)

**Source:** `anomalies.csv` in the Hub archive
(`european-modelling-hubs/covid19-forecast-hub-europe_archive/main/data-truth/anomalies/anomalies.csv`).
Verified schema:

```
target_end_date, target_variable, location, location_name, anomaly
```

`target_variable` ∈ {`inc case`, `inc death`, `inc hosp`}; `anomaly` is free text
with values incl. `large data revision`, `Negative case reporting`,
`Replaced data source`, `Removed double counting`, `No data reported`. Joins on
`(location, target_end_date, epi_target)` — same keys already used for the obs
join at [process-data.R:90](../../Documents/Github/eval-by-method/R/process-data.R#L90).

**Design:**

1. New loader in [R/utils-data.R](../../Documents/Github/eval-by-method/R/utils-data.R)
   (next to `download_obs`, [utils-data.R:70](../../Documents/Github/eval-by-method/R/utils-data.R#L70)):
   `download_anomalies()` → read CSV, filter `target_variable %in% c("inc case","inc death")`,
   map to `epi_target` (`"Cases"`/`"Deaths"`), write `data/anomalies.csv`.
2. In [process-data.R](../../Documents/Github/eval-by-method/R/process-data.R), derive a
   covariate. Two options for the flag — decide at build time:
   - **Any-anomaly**: `Anomaly = !is.na(anomaly)` (broad data-quality flag), or
   - **Revision-specific**: `Revised = grepl("revision", anomaly, ignore.case = TRUE)`
     (just `large data revision`; closest to the user's "data revision or not").
   `left_join` into `data` (after the obs join,
   [process-data.R:89-93](../../Documents/Github/eval-by-method/R/process-data.R#L89-L93));
   set `FALSE` where unmatched.
3. Add `Revised` to the formulas in `analysis-model.R`. Being binary (2 levels),
   use a **fixed factor** for the same reason as `Epi_target` — not a `bs="re"`
   random effect. Extract via the parametric-table helper, not `extract_ranef`.

**Feasibility:** Easy. One curated CSV, clean join keys already in the pipeline,
covariate slots into the existing fixed/parametric machinery. **Caveat:** anomaly
coverage may be uneven by country/period (the visible sample is heavy on
`inc hosp` and clusters in early 2021); check case/death coverage and date span
before trusting it as a study-wide covariate, and report missingness.
