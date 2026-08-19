# Change log

Notable changes to the analysis, manuscript, and repository.
Newest first.

## Unreleased — Drop the 1e-7 constant added to every score (#166 review)

`R/process-data.R`, `R/sensitivity/check-family.R`, `report/quarto/_methods.qmd`, `report/supplement.qmd`

`process-data.R` added 1e-7 to every score so that the 553 forecasts (0.27%) scoring exactly zero were representable on a log link.
The constant was needed for a Gamma family, which has no support at zero.
The primary model now uses a Tweedie family with power parameter between 1 and 2, which has a genuine point mass at zero, so the constant is no longer required and the exact zeros are retained.

The constant parked those forecasts at log(1e-7) = -16.1, around 11 log-units below the next smallest score, which is a statement about an arbitrary constant rather than about the forecasts.

`check-family.R` still fits the comparison arms with the constant added, because Gamma cannot be fitted without it, and the four arms in the supplementary family-comparison table are therefore fitted to a common response vector.
The `offset` flag in that script now records whether an arm adds the constant, the reverse of its earlier meaning; a `tweedie-offset` arm replaces `tweedie-nooffset` and gives the comparison against the primary specification.

All model outputs were refitted on the new response.
## Unreleased — Model structure crossed with epidemiological outcome (#158); DAG update (#162)

`R/analysis-model.R`, `R/utils-effects.R`, `R/plot-model-results.R`, `R/analysis-descriptive.R`, `R/dag-check.R`, `report/quarto/_methods.qmd`, `report/quarto/_results.qmd`, `report/supplement.qmd`

The model assumed each structure predicted cases and deaths equally well.
Replaced `s(Method, bs = "re")` with `s(Method, Epi_target, bs = "re")`, so a structure may predict one outcome relatively better than the other.

There is deliberately no separate structure main effect alongside it.
mgcv's `bs = "re"` interaction is an unconstrained zero-mean prior over all cells, so its average across outcomes is exactly what a main effect represents; with both penalised, the split between them follows the relative variance estimates rather than the data.
Fitted together, mgcv gave the main effect 0.001 effective degrees of freedom against 4.9 for the crossed term, and dropping it changed AIC, deviance explained and residuals by nothing on either scale.
The pooled per-structure effect is instead recovered as a contrast averaging a structure's two cells, which accounts for the covariance between them.

This materially improves what can be reported.
The old main effect was shrunk flat to 1.000 (0.994-1.007) for every structure; the pooled contrast gives real estimates with honest intervals, from 0.977 (0.878-1.088) for judgement models to 1.046 (0.946-1.155) for semi-mechanistic.
Adding the crossed term improves AIC by 93 on the log scale and by 16,724 on the natural scale, where it also reduces residual skew from 4.60 to 4.38.

Human judgement models perform relatively better on cases than deaths (0.96 against 0.99), the same direction as Bosse et al. (2022).
The widest separation is among agent-based models (cases 1.09, deaths 0.88), from only three models, and every interval spans the grand mean.
The direction of these contrasts is stable across error families but the magnitude is not: under a Gaussian family the judgement separation is larger and the agent-based separation vanishes.
Recorded as a supplementary sensitivity; including or excluding the Hub baseline makes no material difference.

New `R/utils-effects.R` replaces `gammit::extract_ranef()`, which cannot handle a factor-by-factor random effect.
It reads only the last variable name of an interaction and looks up that factor's levels, so it collects 5 labels for a 10-coefficient term and fails, taking down extraction for every term in the fit.
The replacement rebuilds each smooth's design matrix from the formula mgcv stores on the smooth object, mapping labels to coefficients exactly without assuming an ordering.
Validated against gammit on a no-interaction fit: identical on every column to the last decimal.

Brought the central interpretive point out through the text, in the abstract, results, methods and discussion.
Structural differences pointed in opposite directions for cases and deaths, so any term averaging over outcomes recovers close to zero: cell effects of ±0.04-0.12 average to ±0.001-0.045.
This explains both why a shared structure effect was always flat, in every specification tried, and why the crossed term carries signal.
In the discussion it is offered as a mechanism for null findings in earlier structure comparisons: a pooled null is consistent with either an absence of differences or with differences that offset across targets, and the two cannot be separated without letting the effect vary by target.
The text is explicit that this demonstrates the mechanism rather than establishing any particular contrast, since no per-outcome interval excludes the grand mean.
Confirmed the signal was not previously absorbed by `s(Model)`: individual-model effects are essentially unchanged by adding the interaction (correlation 0.995, largest change 0.049), and a per-model effect is constant across outcomes so cannot represent a within-model case/death difference for the 34 of 48 models forecasting both.

Documented why the epidemiological target stays a fixed effect.
The same aliasing applies as for the structure main effect, but only one of the two terms is penalised, so the unpenalised fixed effect takes the component common to all structures and the crossed term keeps only departures from it.
In the fitted model the crossed effects average to zero within each target to ~1e-13, so the whole deaths-versus-cases difference sits in the fixed coefficient (-1.03) and none leaks into the structure estimates (largest cell 0.12).
This is emergent rather than imposed: the smooth retains all ten coefficients, so no centring constraint was applied.

Discussion: corrected the claim that adjusted estimates "were no different from the overall average", which described a term since shown to be shrunk to nothing; they are imprecise rather than identical to the mean.
Added a paragraph identifying the structure-by-outcome interaction as the one specification choice that materially changed the results, against a model-based approach that is otherwise highly flexible and whose substantive conclusion proved robust to covariate selection, link function and error family.
Added a main-text pointer to the Gaussian sensitivity, with the detail kept in the supplement.

DAG (#162): the epidemiological outcome is added as a confounder rather than merely a covariate, since forecasters chose which outcomes to submit for and that choice is associated with structure.
Querying the updated diagram returns our exact covariate set as a minimal sufficient adjustment set for the direct effect, and returns no valid set for the total effect, because latent modeller strategy cannot be blocked.
This formally supports reporting a partial, direct association rather than a total effect.
The crossed term is effect modification, which a causal diagram does not encode, so it does not alter the adjustment set.

## Unreleased — Model WIS with a Tweedie family (#159)

`R/analysis-model.R`, `R/sensitivity/check-family.R`, `report/quarto/_methods.qmd`, `report/quarto/_results.qmd`, `report/supplement.qmd`

The primary model used `gaussian(link = "log")`, which left deviance residuals with skew 5.8 and kurtosis 77.
Modelling `log(WIS)` directly would fix the residuals but loses propriety of the score, so the fix had to come from the error family instead.

Compared Gaussian, Gamma and Tweedie families on the joint specification, holding formula and data fixed.
Both scales now use `tw(link = "log")`, replacing `gaussian(log)` on the log scale and `Gamma(log)` on the natural scale.

On the log scale this is a large improvement: residual skew falls from 5.84 to 0.58, kurtosis from 77.5 to 9.2, and deviance explained rises from 0.286 to 0.380.
Gamma fits the same data almost identically (skew 0.52, deviance explained 0.378) but does not converge on either scale, which is the reason for preferring Tweedie.
The Tweedie power parameter is estimated at 1.99, the upper limit `mgcv` permits, so the fitted family is a Gamma in all but numerical behaviour.

This also resolves the natural-scale non-convergence recorded in the previous entry: that was a Gamma problem, not a scale problem.
On the natural scale the change fixes convergence but not the fit — residual skew is unchanged at 4.59, because natural-scale WIS is skewed beyond what any Tweedie can absorb.
Nothing in the rendered manuscript or supplement reads `output/natural/`, so this affects no reported result.

Several adjusted estimates moved materially under the new family, most notably the deaths-versus-cases contrast (ratio 0.17 to 0.38).
All substantive conclusions hold: no model structure differs from the grand mean, stable trends remain the most predictable, increasing trends the least, and Omicron BA.1 the hardest variant phase.
Delta's interval now excludes 1, where previously it did not.

Investigated whether the `1e-7` constant that `process-data.R` adds to every score was driving the skew, since 553 forecasts (0.27%) score exactly zero and the constant parks them 11 log-units below the next smallest value.
It was not: refitting with the constant removed and the exact zeros retained changes residual skew by 0.01.
`process-data.R` is therefore unchanged, and the result is recorded in the supplement as a negative finding.

Fixed `archive_diagnostics()`, which reassigned its accumulator and so returned the whole `fit-summary.csv` rather than the row just written.
Fixed a non-standard-evaluation trap: `tw()` deparses its `link` argument, so passing a variable sent the literal string `"family_link"`.

## Unreleased — Include the Hub baseline model; archive fit diagnostics per specification

`R/analysis-model.R`, `R/plot-model-flow.R`, `R/sensitivity/check-autocorrelation.R`, `R/sensitivity/check-link-robustness.R`, `report/quarto/_abstract.qmd`, `report/quarto/_methods.qmd`, `report/quarto/_results.qmd`, `report/quarto/_discussion.qmd`, `report/supplement.qmd`, `CLAUDE.md`

`EuroCOVIDhub-baseline` is now included in the analysis; only `EuroCOVIDhub-ensemble` remains excluded.
The two had been dropped together by a single `grepl("EuroCOVIDhub-")` filter, but they are not equivalent: the ensemble is a function of the contributed forecasts and would double-count them, whereas the baseline is an independently specified statistical model and belongs in the sample on the same terms as any other participant.
Narrowed the filter at every remaining site, and relabelled the flow-diagram exclusion step from "Not created by Hub" to "Not the Hub ensemble".

The sample grows from 47 to 48 models across 38 teams, with the statistical structure group going from 12 to 13 models.
Because the baseline submitted for every country in almost every week, the statistical group's share of forecasts rises to roughly 40%, so the previous claim that mechanistic, semi-mechanistic and statistical models each contributed about a third no longer holds and has been replaced.
Model counts, rater-disagreement counts, and per-structure forecast shares in the results text are now computed inline from the data rather than hardcoded, so they track future changes to the sample.
Added a sentence to the Discussion noting that the baseline anchors the statistical group towards the performance achievable without epidemiological structure.

`model_wis()` gains a `spec_label` argument. When supplied it writes an archived copy of the `appraise()` panel plus a row of fit statistics (family, link, formula, n, AIC, deviance explained, deviance-residual skew and kurtosis) to `output/diagnostics/`, upserted on (`spec_label`, `scale`).
The supplement still reads the stable `output/<scale>/plots/check_joint.png` path.
This exists so the model specifications planned next — a skew-tolerant error family, and a method-by-target interaction — can be compared against this fit rather than silently overwriting it.

Corrected `CLAUDE.md`, which pointed at `report/quarto/supplement/_supplement.qmd` and described root render wrappers that do not exist; the supplement is at `report/supplement.qmd` and `_quarto.yml` renders it directly.

Note: `R/sensitivity/check-autocorrelation.R` sources `R/sensitivity/model-logresp.R`, deleted in the change below, so it does not currently run. Its filter was narrowed for consistency but the script needs rebasing onto another residual source before it is usable again.

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
