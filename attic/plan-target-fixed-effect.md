# Plan: epi_target as in-model fixed factor + data-revision covariate

## Context

The forecast-evaluation GAMM in [R/analysis-model.R](../../Documents/Github/eval-by-method/R/analysis-model.R)
currently fits **two separate `bam()` models** — one for cases, one for deaths —
by `map()`-ing over `unique(m.data$epi_target)` and filtering the data inside the
loop ([analysis-model.R:69-88](../../Documents/Github/eval-by-method/R/analysis-model.R#L69-L88)).
Epidemiological target is therefore a *stratification*, not a model term.

**Goal 1 (primary):** Make `epi_target` a covariate *inside a single model*, as a
**fixed factor main effect** (`wis ~ Epi_target + ...`). One fit over the full
dataset replaces the two stratified fits. All other effects (Method, Trend,
Variant, Incidence, Horizon, Location, Model, CountryTargets) become **shared
across cases and deaths**.

> Why fixed, not random: `epi_target` has only 2 levels (Cases, Deaths). A random
> effect estimates a *variance across levels*, which needs ≥5–8 levels to be
> identified; at 2 levels the variance is near-degenerate (shrinks to the grand
> mean, ~0 edf) and buys nothing. A fixed factor gives one fully-identified,
> interpretable contrast (Deaths vs Cases on the log scale, with a CI) — the
> conventional choice at 2 levels.

Consequence: this intentionally drops the per-target contrasts. The Results text
currently reports separate cases/deaths estimates for Trend and Variant — those
collapse to single shared estimates and the prose must be updated. The
cases-vs-deaths difference is now a single fixed coefficient instead.

**Goal 2 (extension, PLAN ONLY — do not build):** Add a data-revision covariate
sourced from the Hub archive `anomalies.csv`. Scope this section as a documented,
ready-to-implement design; no code or data fetch in this pass.

---

## Goal 1 — Implementation

### 1. `R/analysis-model.R` — single combined fit

Replace the per-target `map()` machinery with one model over all rows.

- **Keep** the data prep ([analysis-model.R:28-41](../../Documents/Github/eval-by-method/R/analysis-model.R#L28-L41)).
  `epi_target` is already created as a `"Cases"`/`"Deaths"` string in
  [process-data.R:100](../../Documents/Github/eval-by-method/R/process-data.R#L100);
  add `Epi_target = as.factor(epi_target)` in the model-prep mutate (mirrors the
  scratch convention in [sensitivity/model-building.qmd:55](../../Documents/Github/eval-by-method/R/sensitivity/model-building.qmd#L55)).
- **Add** `Epi_target` as a fixed parametric term (`wis ~ Epi_target + s(...)`) to
  the joint formula
  ([analysis-model.R:57-65](../../Documents/Github/eval-by-method/R/analysis-model.R#L57-L65)),
  and add a `target = wis ~ Epi_target` univariate entry to the list
  ([analysis-model.R:45-54](../../Documents/Github/eval-by-method/R/analysis-model.R#L45-L54)).
  (`Cases` is the reference; coefficient = Deaths−Cases on the log scale.) The
  existing `s(CountryTargets, bs="re")` is unrelated — leave it.
- **Rewrite `m.fit`** ([analysis-model.R:69-82](../../Documents/Github/eval-by-method/R/analysis-model.R#L69-L82))
  to a single `bam()` call on the whole `m.data` (drop the `outcomes`/`set_names`/
  `map`/`filter(epi_target == outcome)` wrapper). Univariate fits become a single
  `map(m.formulas_uni, ~ bam(.x, ...))` over the formula list (no nested target map).

### 2. Output extraction — collapse the `epi_target` dimension

The current extraction keys everything by `epi_target`
([analysis-model.R:91-107](../../Documents/Github/eval-by-method/R/analysis-model.R#L91-L107)):
`map_df(..., .id = "epi_target")`, nested `map_depth`, and per-target `k.check`.
With one fit there is no target list.

- `random_effects_joint`: single `extract_ranef(m.fits_joint)`; **no** `epi_target`
  column from the fit. **Important:** `Epi_target` is a *fixed* term, so it will
  **not** appear in `extract_ranef()` (gammit only returns random terms). The
  target effect must be pulled separately from the parametric table:
  `summary(m.fits_joint)$p.table` (or `gammit::extract_fixef()` if available) →
  estimate, SE, CI for the `Epi_targetDeaths` row. Decide whether to (a) splice it
  into the `random_effects` table as a pseudo-`group_var = "Epi_target"` so the
  existing plotting/table code picks it up uniformly, or (b) report it in its own
  small fixed-effects table. **Recommend (a)** — minimal downstream churn; build a
  one-row tibble matching the ranef columns (`group_var, group, value, lower_2.5,
  upper_97.5, se`) and `bind_rows` it in.
- `random_effects_uni`: `map(m.fits_uni[...], extract_ranef) |> list_rbind()` —
  drop the `.depth = 2` / per-target `list_rbind`. The `target = wis ~ Epi_target`
  univariate fit also needs the parametric-table extraction (same helper as above),
  not `extract_ranef`.
- `checks <- k.check(m.fits_joint)` (single object, not `map`).
- `appraise()` / `ggsave` block
  ([analysis-model.R:117-120](../../Documents/Github/eval-by-method/R/analysis-model.R#L117-L120)):
  one plot, e.g. `check_joint.pdf`, instead of `iwalk` per target.

### 3. Downstream consumers that assume per-target output

- **`R/plot-model-results.R`**: both `plot_models`
  ([plot-model-results.R:51](../../Documents/Github/eval-by-method/R/plot-model-results.R#L51))
  and `plot_effects`
  ([plot-model-results.R:84](../../Documents/Github/eval-by-method/R/plot-model-results.R#L84))
  call `facet_wrap(~epi_target, ...)`. `epi_target` will no longer exist on the
  effects table → **remove these facets** (single panel each). With extraction
  approach (a), `Epi_target` rides in as a `group_var` and can be added to the
  `variables` shown by `plot_effects` alongside `Method`.
- **`report/quarto/_results.qmd`**: the table/prose at
  [_results.qmd:144-204](../../Documents/Github/eval-by-method/report/quarto/_results.qmd#L144-L204)
  builds row keys as `paste(epi_target, model, group, ...)`
  ([_results.qmd:158](../../Documents/Github/eval-by-method/report/quarto/_results.qmd#L158))
  and pulls per-target values like `table_effects["Cases_Adjusted_Stable", ...]`
  ([_results.qmd:198-204](../../Documents/Github/eval-by-method/report/quarto/_results.qmd#L198-L204)).
  These keys lose their `epi_target` prefix → **update the key construction and
  every `[...]` lookup**, and **rewrite the Trend/Variant prose** that currently
  reports separate cases vs deaths numbers into single shared estimates.
  Check `print_table2` in [R/analysis-descriptive.R](../../Documents/Github/eval-by-method/R/analysis-descriptive.R)
  for any `epi_target` grouping.

### 4. Methods prose — `report/quarto/_methods.qmd`

- [_methods.qmd:38](../../Documents/Github/eval-by-method/report/quarto/_methods.qmd#L38)
  ("We stratified all analysis by epidemiological target…") → replace with: target
  included as a fixed factor within a single model.
- [_methods.qmd:57](../../Documents/Github/eval-by-method/report/quarto/_methods.qmd#L57)
  ("fit separately for each epidemiological target (cases, deaths)") → single fit.
- Equation block ([_methods.qmd:64-78](../../Documents/Github/eval-by-method/report/quarto/_methods.qmd#L64-L78)):
  add a fixed term for target to the linear predictor $\eta_i$ (e.g.
  $\beta_{\text{target}}\,\mathbb{1}[\text{Deaths}]$). Do **not** add it to the
  random-effects set $\mathcal{G}$
  ([_methods.qmd:74](../../Documents/Github/eval-by-method/report/quarto/_methods.qmd#L74)) —
  it is parametric, not a zero-mean Gaussian RE.
- State why fixed not random (2 levels → variance unidentified); this is the
  conventional choice.

### 5. Re-run + save

`source("R/analysis-model.R"); model_wis(scoring_scale = "log", output_dir = "output/log")`
then natural scale. Overwrites `output/log/results.rds` consumed at
[_results.qmd:144](../../Documents/Github/eval-by-method/report/quarto/_results.qmd#L144).

---

## Goal 2 — Data-revision covariate (PLAN ONLY)

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

---

## Verification

- **Runs clean:** `model_wis()` completes for log + natural scales; inspect
  `output/.../plots/check_joint.pdf` (Q-Q via `gratia::appraise`) and `k.check`.
- **Target effect present:** `summary(m.fits_joint)$p.table` has an
  `Epi_targetDeaths` row with a finite estimate/SE; if using extraction approach
  (a), `results$effects` carries one `Epi_target` row (the Deaths−Cases contrast)
  and no leftover `epi_target` column; other `group_var`s unchanged in count.
- **Report renders:** `quarto::quarto_render("report/manuscript.qmd")` builds
  with no missing-key errors; Table 2, `fig-plot-coeffs`, `fig-plot-models`
  render single-panel; Trend/Variant prose now reports shared (not per-target)
  estimates.
- **Sanity vs old fit:** shared effect estimates should sit between the previous
  cases-only and deaths-only values; large divergence flags a spec/extraction bug.
