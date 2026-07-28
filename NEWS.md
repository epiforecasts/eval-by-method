# Change log

Notable changes to the analysis, manuscript, and repository.
Newest first.

## Unreleased — Reorganise supplement; drop double-log and log-response sensitivity arms

`report/quarto/supplement/_supplement.qmd`, `R/analysis-model.R`, `R/sensitivity/model-logresp.R`, `R/plot-model-flow.R`, `CLAUDE.md`

Presentation and repository cleanup; the primary model and its conclusions are unchanged.

Reorganised the supplement so each alternative model specification is reported in a consistent, comparable structure (covariate selection, fitting, diagnostics).
Dropped the double-log-transformed fit and its supplementary section: an additional log transform on the outcome (LWIS) violates propriety of the score, so it is no longer presented.
Removed the log-response reparameterisation arm — deleted `R/sensitivity/model-logresp.R` and its generated artifacts under `output/log-resp/` — as it is no longer referenced by the rendered manuscript.

`analysis-model.R` now stores the fitted `data` in `results`, so the supplement can plot the outcome (LWIS) distribution directly from the saved fit.
Corrected the flowchart export path in `plot-model-flow.R` to `output/flowchart.png`, the location the supplement reads via `include_graphics()`.
Moved stale planning docs and notebooks into `attic/`, removed `report/notebook.qmd`, and updated `CLAUDE.md` Project Structure to match the current tree.

## Unreleased — Fix population normalisation in scoring and regenerate scores

`R/process-score.R`, `data/scores-raw-case.csv`, `data/scores-raw-death.csv`

Fixed a column-name bug in `process-score.R`: the per-100k normalisation referenced a `pop` column, but `populations.csv` supplies `population` (renamed by `download_pop()`), so the script errored and had not run since the normalisation was added (commit `aca4fa7`, April 2026).
As a result the committed `scores-raw-*.csv` were stale — last generated December 2025, before normalisation — holding WIS computed on raw counts rather than incidence per 100,000.
Every downstream consumer (`process-data.R` reads these CSVs directly; then the GAMM in `analysis-model.R`, descriptive tables, and all manuscript figures) had therefore been fitting on un-normalised scores, which carry the population-size signal the normalisation exists to remove.
Renamed `population` to `pop` on read and regenerated both score files; they now hold per-100k-normalised WIS matching the current code.
Verified the manual log path (`log(pmax(prediction, 0) + 1)`) is numerically identical to the native `scoringutils::transform_forecasts(log_shift, offset = 1)` on these data (no negative predictions, so the `pmax` clamp never fires).

## Unreleased — Publish manuscript to GitHub Pages

`.github/workflows/render-report.yaml`

Reworked the render workflow to publish the rendered HTML manuscript to a GitHub Pages site instead of opening a no-op pull request.
Previously the workflow rendered `report/manuscript.qmd` (which is `format: html`) but tried to commit `report/manuscript.pdf` — a file that is never produced — and had no Pages deploy step.
The render now uses the Quarto CLI (via `quarto-dev/quarto-actions/setup`) rather than `quarto::render()` (the R `quarto` package is not in `renv.lock`), assembles a `_site/` with `index.html`, and deploys via `actions/upload-pages-artifact` + `actions/deploy-pages`.

Split the output into a two-page Quarto website (`_quarto.yml` at the repo root): the manuscript is the landing page (`index.qmd`) and the supplement is a separate page (`supplement.qmd`) reached via a navbar, replacing the single long page that inlined the supplement.
Both are thin wrappers that `{{< include >}}` the existing `report/` content (`report/manuscript.qmd` and the self-contained `report/quarto/supplement/_supplement.qmd`).
The project sits at the repo root so `here()` continues to anchor there; the workflow now runs `quarto render` for the whole project and uploads the generated `_site/`.

*Requires one-time manual setup:* GitHub → Settings → Pages → Source = "GitHub Actions".

## Unreleased — Joint-target refactor (PR #153)

The analysis previously fit two separate models, one for cases and one for deaths.
This refactor replaces them with a single joint model carrying the epidemiological target as a fixed effect, and then works through the consequences: how effects are reported, what diagnostics back them up, and how the manuscript is assembled.

**The substantive conclusion is unchanged.**
No structural approach — mechanistic or statistical, single-country or multi-country — systematically outperformed the others once predictive difficulty was adjusted for.
Every scale, link, and response variation tested below preserves that result.

### 1. One joint model instead of two stratified fits

`R/analysis-model.R`

*Why:* separate `bam()` fits for cases and deaths gave no direct deaths-vs-cases contrast, forced a facet through every downstream plot and table, and estimated each covariate effect twice on half the data.

| Change | Detail |
|---|---|
| Joint fit | `epi_target` is now a fixed parametric factor in one model over the full dataset, giving a direct Deaths-vs-Cases contrast |
| Shared formula | `m.formula_joint` hoisted to file scope, so the sensitivity arms fit an identical specification rather than a hand-copied one |
| Missing scores | Explicit `filter(!is.na(wis))` — `bam` had been dropping 260 unscored rows silently. Primary effect estimates verified unchanged (max absolute difference 0) |
| Family and link | New `family_link` argument; the log scale fits `gaussian(link)`, the natural scale `Gamma(link)` |
| Outputs | Reorganised into scale-named subdirectories: `output/log/`, `output/log-resp/`, `output/natural/`, each holding `results.rds`, `fit_obs.rds`, and `plots/` |

Downstream extraction, plots, tables, and prose all collapse to the single fit.

### 2. Main text reports performance ratios, not log coefficients

`R/plot-model-results.R`, `R/analysis-descriptive.R`, `report/quarto/_results.qmd`, supplement

*Why:* raw log-WIS coefficients are uninterpretable to an epidemiological audience.
The multiplicative ratio — 1 = the grand-mean model, below 1 = better than average — is the natural quantity and matches the risk-ratio convention readers already hold.

- Figures 2 and 3 now plot `exp(value)` with exponentiated confidence bounds, a reference line at 1.0, and a log-scaled y-axis labelled "Performance ratio (vs average model)".
- Table 2 drops its two raw log-scale columns and shows unadjusted and adjusted ratios only.
- The raw log-scale coefficients move to a supplement table, so nothing is lost for reviewers who want them.

### 3. New diagnostics and sensitivity arms

`R/sensitivity/` (new directory)

*Why:* the primary model uses `gaussian(link = "log")` on the raw WIS.
That models the score mean multiplicatively but assumes Gaussian errors on a right-skewed scale, leaving heavily skewed residuals (skew ≈ 5.5, kurtosis ≈ 62).
The question is whether that assumption bends the conclusions.
It does not.

| Check | Script | Finding |
|---|---|---|
| Log-response arm | `model-logresp.R` | Refits the joint model on `log(WIS)` with an identity link. Residual skew improves from ≈ 5.5 to ≈ −0.8. Focal effects correlate ≈ 0.96 with the primary fit; Method and CountryTargets effects are ≈ 0 under both. Trend and Variant magnitudes attenuate on this scale — reported honestly rather than buried. |
| Temporal autocorrelation | `check-autocorrelation.R` | Serial autocorrelation is modest and short-lived: median lag-1 residual ACF ≈ 0.33, ~0 by lags 3–4 across 3,353 series. Dependence is stronger *across horizons sharing a forecast origin* (residual r ≈ 0.39–0.76). Treated as a diagnostic only — no autoregressive structure added, and standard errors are read as a lower bound. An origin-level random effect would absorb the correlation but is collinear with the within-origin trend, variant, incidence, and location terms, so the correction belongs on the standard errors, not the model structure. |
| Link robustness | `check-link-robustness.R` | The log-scale response already sits on a log scale, and the production model applies a second log link. An identity-link refit gives near-identical Method and CountryTargets effects. |
| Model building | `model-building.qmd` | Notebook documenting the specifications tried and why the production one was chosen. |

Supporting additions: `plot_fit_obs()` observed-vs-fitted helper, and `fit_obs.rds` saved per scale with a row-alignment guard.

### 4. Figure and table code rewritten

`R/analysis-descriptive.R` (~410 lines changed)

*Why:* the joint fit removed the per-target facet that most descriptive figures were built around, and Table 1 needed model-composition columns.

- Added: `table_composition()`, `plot_error_vs_obs()`, `print_table2()`.
- Removed: `plot_over_time()`, `plot_ridges()`, `data_plot()`, `trends_plot()`, `table_metadata()`.

### 5. Manuscript restructured

`report/`

*Why:* the narrative needed a single organising frame, and the citations were plain-text links rather than real references.

- **Reframed** around the distinction between the *forecast-generating process* (modeller choices) and the *target-generating process* (epidemic dynamics, surveillance, data revisions).
  We adjust for the latter to isolate the former.
  "Controlled direct effect" is replaced throughout by "adjusted partial association", which is what the model actually estimates.
  The headline result is restated as overlapping confidence intervals around the grand mean: no single structural approach dominated.
- **Retitled**, render format switched from PDF to HTML, and the prose split into per-section files under `report/quarto/` (`_abstract`, `_background`, `_methods`, `_results`, `_discussion`).
- **Real bibliography**: `report/references.bib` (30 entries) plus a PLOS Computational Biology CSL; 28 in-text citations converted to `[@key]`; the approaches table converted to a native Quarto pipe table so its citations render (and one kableExtra dependency drops out of the main text).
- **Supplement now evaluates.** It had `eval=FALSE` set globally, so every chunk displayed code and produced no output — which had been masking three latent errors in chunks that had never run. With evaluation on and those errors fixed, the supplement gains sections on model diagnostics, natural-scale results, the log-response sensitivity arm, and temporal autocorrelation.

### 6. Repository hygiene

- **Diagnostic plots are PNG, not PDF.** `appraise()` plots ~150k residuals; as vector PDF that was ~20MB per scale, which was bloating the repo. They are now written at 300dpi PNG (~0.3MB, no loss of legibility), and `/output/**/plots/*.pdf` is gitignored so the vector versions cannot come back. The PNGs stay tracked because the supplement embeds them.
- **Rendered build artefacts untracked**: `README.html`, `README_files/`, `report/manuscript.html`, `report/manuscript_files/`. `README.Rmd` became `README.qmd`.
- **New directories**: `attic/` (parked scripts kept for reference), `planning/` (design notes, including an assessment of porting the GAMM to brms/Stan — verdict: an easy formula translation but expensive to run and not warranted), and `submission/first/` (archived first submission).

### Reproducing the analysis

Unchanged in shape, but note that the model must be fit before the manuscript is rendered:

```r
source(here("R", "process-score.R"))    # score forecasts
source(here("R", "process-data.R"))     # join scores to covariates
source(here("R", "analysis-model.R"))   # fit the joint GAMM; writes output/
quarto::quarto_render("report/manuscript.qmd")
```

*Supersedes `Plan.md`, whose dated entries are folded into the sections above.*
