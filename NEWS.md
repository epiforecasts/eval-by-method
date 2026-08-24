# Change log

Notable changes to the analysis, manuscript, and repository.
Newest first.

## Unreleased — Results figures and tables fixed

`report/quarto/_results.qmd`, `R/plot-model-results.R`

Table 1 (`tbl-models`) rebuilt to match its own caption: adds a `Participation (%)` row (median, IQR of the percentage of available targets each model submitted for) and an `All included` total column via `gtsummary::add_overall()`, replacing a table that only showed model-structure counts split by which outcome(s) a model forecast. Table 2 (`tbl-structure`) caption was truncated mid-sentence in the source (cut off after "weekly incidence, trend,"); completed with the full covariate list and the dangling final clause removed.

Figure 3 (`fig-structure-effects`) is a single panel by design; two prose cross-references (`@fig-structure-effects A`, `@fig-structure-effects B`) wrongly implied a two-panel figure and are corrected to plain `@fig-structure-effects`. Separately, `plot_config` in `R/plot-model-results.R` conflated colour values and level order into one `ordered()` factor keyed by hex code, which silently sorted levels alphabetically by colour string and broke both `factor(..., levels = plot_config$method_levels)` and `scale_colour_manual(values = ...)` wherever they were used — this produced an all-`NA` y-axis in Figure 3 and an uncoloured legend in Figure 4 panel B (`plot_model_ranks()`), the second only surfacing because Figure 3's dead reference to an undefined `colour_key` had been masking the same bug there. `plot_config` is now split into `*_colours` (named vector, for `scale_*_manual`) and `*_levels` (plain ordered vector of names, for `factor(levels = )`), with both plotting functions updated to use the correct one; both figures re-rendered and verified against `output/log/results.rds`.

## Unreleased — Background and Discussion revision

`report/quarto/_background.qmd`, `report/quarto/_discussion.qmd`, `report/quarto/_methods.qmd`

Background: the aims sentence at the end of the opening paragraph is cut, so aims are stated once, in the final paragraph. The selective-participation passage now announces its two problems before listing them. The claim that "most variation" is associated with the target is replaced by the contrast the results support, matching the Discussion. The truncated final sentence is completed as designing and analysing comparative evaluations. The sentence introducing the approaches table no longer restates the caption's ordering, and the regression paragraph now identifies itself as the table's final row rather than an alternative to the table. `Covid-19` standardised to `COVID-19` throughout, including in `_methods.qmd`.

Prose only, no refit. Three claims corrected against what the analysis supports: the penalisation sentence is now indicative, since every categorical covariate is already a penalised random effect (`bs = "re"`, `R/analysis-model.R`); the "driven more by the target" claim is stated as the contrast the results carry, rather than as a variance share the analysis never computes; and `@scarpino2019` is cited as bounded predictability rather than inherent unpredictability.

Summary paragraph cut from five sentences to four, dropping the repeated "none distinguishable from the overall average". The ranking-instability sentence now says the instability did not relate to model structure, so the inference that follows it holds. Paragraph on target difficulty gains a closing sentence on the room left for methods to improve. A generalisability limitation is added (single pathogen, project, and pair of outcomes, over two years in Europe), and the conclusion ends on matching design formality to the question, as in the abstract. Remaining edits are wording and two grammar fixes (`limit`/`limits`, `specifing`).

## Unreleased — Regime standardisation sketch

`attic/regime-standardised-scores.qmd`

Exploratory note, held outside the manuscript. Treats the case-versus-death score comparison as a crude rate comparison and applies direct standardisation over epidemic regime, defined as the weekly log change in observed incidence. Includes a g-computation version using a target-only GAMM, so incidence level, country, and variant are held fixed while growth moves. Sourced from `attic/inc-gr-tensor.R`, which fits the same target-only specification.

Standardising over phase leaves the case-death ratio unchanged, so phase is ruled out as an explanation. Level cannot be standardised across outcomes, because a rate per 100,000 does not mean the same thing for cases and deaths, and level is where the difficulty gradient sits. A rate-by-count decomposition within deaths separates observation noise from epidemic severity, and shows that `process-score.R` applies its `log(x + 1)` offset after normalising to per 100,000 — so at death rates below one per 100,000 the log transform is close to the identity, and death scores are closer to an absolute error measure than case scores are.

## Unreleased — Results TODOs cleared

`R/analysis-descriptive.R`, `R/plot-model-results.R`, `R/plot-model-flow.R`, `report/quarto/_results.qmd`, `report/quarto/_discussion.qmd`, `report/supplement.qmd`

The ten inline TODOs in the Results section are resolved. No refit: all estimates still come from `output/log/results.rds`.

Table 1 (`print_table1()`) drops the geographic-scope rows and the single-country column, and gains a combined Models (%) column across both outcomes and a median participation column, as a percentage of the 26,624 available forecast targets.

`plot_models()` no longer shows model structure or geographic scope, both of which its estimates already adjust for. Models are labelled by their crude rank before adjustment and coloured by the same, so panel A of the model-variation figure shows how far adjustment scrambles the crude order. It takes the `ranks` object rather than `scores`, so `plot_model_variation()` no longer needs the score data. Panel B keeps structure colour and now carries its own key.

"Adjusted performance ratio" replaces the generic axis label wherever a panel shows adjusted estimates alone; the pooled structure panel keeps the old label because it overlays unadjusted estimates.

`plot_error_vs_obs()` converts observed incidence to per 100,000 population before plotting. Weighted interval scores were already computed on population-normalised forecasts (`R/process-score.R`), so the figure had been mixing units. `Incidence` in `process_data()` is unchanged, since it feeds the covariates the saved fit used.

Results gains: coverage of the target matrix in the participation paragraph; a crude comparison against the Hub ensemble, with the reason it is excluded from the fit; median LWIS by forecast horizon, reported descriptively because horizon enters the fit as a per-model smooth; the number of models and forecasts behind the widest case-death structure contrast; and adjusted ratios for single- and multi-country models.

The Spearman correlation is kept for the rank comparison: both columns rank the same models, so the rank changes sum to zero and a paired location test has nothing to detect. The sentence now also gives the largest single move.

Discussion links the limitations paragraph back to the sparse-stratum argument in Results, and says what penalised terms do about it.

`create_model_flow()` writes `output/model-flow-counts.csv` alongside the flowchart, and the Supplement reads the submitted and included model counts from it.


## Unreleased — Results figures reworked; discussion and supplement pass

`R/plot-model-results.R`, `report/quarto/_results.qmd`, `report/quarto/_discussion.qmd`, `report/quarto/_references.qmd`, `report/supplement.qmd`

Main text figures reduced from four to three, and reorganised around the two claims they support.

`plot_structure_effects()` combines the pooled structure effects and the structure-by-outcome effects into one figure, sharing an ordering of structures, replacing separate figures that carried the same axis and units.
Table 3 moves to the Supplement, since the figure carries the same estimates.

`plot_model_variation()` combines the per-model effects with the rank comparison: panel A gives adjusted effects ordered by effect, panel B plots unadjusted against adjusted rank against a diagonal marking no change.
The rank comparison was a slope chart in the Supplement, unreadable at 48 models, and is now the clearest statement of the paper's methodological claim.
The Supplement keeps a table of the ten models that move furthest.

`plot_effects()` coloured by `group_var` with the guide suppressed, so a single-variable call rendered every point in Set1 red; it now takes a fixed grey unless several variables are shown.
`plot_models()` orders models by adjusted effect and no longer clips its third legend.
`plot_error_vs_obs()` no longer overrides the document theme.

Discussion reordered so that limitations follow the interpretation rather than interrupting it.

Supplement: an orientation paragraph, corrected heading levels, captions for six figures that had none, and clearer names for the two fitting sections.
The Supporting Information list named five figures and one table that no longer exist in that form, and is now a single S1 Text entry.

## Unreleased — Reduce the Background table to one axis; state the study aim in Methods

`report/quarto/_background.qmd`, `report/quarto/_methods.qmd`, `report/quarto/_discussion.qmd`, `report/references.bib`

The Background table mixed three axes: the stage of the workflow, whether a tool controlled the forecast-generating or the target-generating process, and degree of formality.
Eight of its nine rows acted on the target-generating side, which the table did not show.

It is now restricted to that one axis, with six rows running from no control at all through to full adjustment: unadjusted comparison, inclusion criteria, matching, stratification, indirect standardisation, regression adjustment.
The "In observational research" column is dropped, since the row labels are the epidemiological names and the prose says where they come from.
468 words to 241.

The rows removed are prerequisites rather than alternatives: measurement of the exposure and the scale of the outcome are design choices described in Methods, and quantitative bias analysis is covered in the Discussion.
`@hernan2020` and `@vanderweele2017` appeared only in the removed quantitative bias analysis row, and move to the Discussion sentence on minimal sufficient adjustment sets and E-values.

Methods now opens with a Study aim subsection giving the estimand: the exposure is model structure, forecasters chose their own targets so a crude comparison of scores is confounded, adjustment is what makes the comparison possible across all targets at once, and the estimand is a direct rather than a total effect because modeller strategy is unmeasured.
The Background aims paragraph shrinks to the approach, the demonstration and the finding.
This closes reviewer comments 1.7 and 1.16 (#105, #114).

Adds `greenland2016` for sparse-data bias.

## Unreleased — Cut the manuscript prose by around a quarter (#100)

`report/quarto/_abstract.qmd`, `_background.qmd`, `_methods.qmd`, `_results.qmd`, `_discussion.qmd`, `report/supplement.qmd`

Rewrote the prose across all five sections, keeping every claim, number, citation and inline computation.
Prose word counts: background 1126 to 631, results 1527 to 1064, discussion 1676 to 1256, methods 1426 to 1054, abstract 316 to 254, 29% overall.

Cuts fall on sentences the tables and figures already carry, restatements of the preceding sentence, and signposting.
The bold pseudo-headers in Methods and Results become real headers, which also serves reviewer comment 1.1 asking for subheadings to guide readers.
The aliasing argument for fitting no structure main effect moves from Methods to the Supplement, which now states the effective degrees of freedom and the centring result in full.

Also corrects the Results sensitivity paragraph, which still said a Gaussian family preserved the direction of every structure-by-outcome contrast.

## Unreleased — Refit on the response without the constant

`output/log/`, `output/natural/`, `output/diagnostics/`

Both scales refitted under `spec_label = "primary-interaction"` after the 1e-7 constant was dropped.

The Tweedie power parameter falls from 1.99, the upper limit `mgcv` permits, to 1.93, so the fitted family is no longer pinned at the boundary.
Estimates are otherwise stable.
Structure-by-outcome ratios move by at most 0.01: agent-based 1.10 on cases and 0.87 on deaths, judgement 0.96 and 0.99, and the pooled per-structure estimates span 0.98 to 1.05.
Covariate effects are unchanged in direction and size: stable trends most predictable, increasing trends least, Alpha and Delta phases better and Omicron BA.1 worst, deaths scoring lower than cases.

The agent-based estimate for death forecasts now sits with its upper bound at the grand mean, which the Results text notes.

Re-running the family comparison against the offset-free response corrects two claims made in earlier entries.
All four families now converge on the log scale, including the Gamma, so non-convergence is no longer the reason for preferring the Tweedie; the reason is that a Tweedie admits the exact zeros while a Gamma requires displacing them by a constant.
The structure-by-outcome contrasts are also less stable across families than previously stated: the case-versus-death contrast keeps its sign under a Gaussian family for judgement, mechanistic and statistical models, but vanishes for agent-based models and reverses for semi-mechanistic ones.
Methods, Results, Discussion and the Supplement now say so.

## Unreleased — Compare model rankings before and after adjustment (#168)

`R/plot-model-results.R`, `report/supplement.qmd`

Ranks each individual model twice: by its partial effect from a univariate model containing only model identity, and by its partial effect from the fully adjusted model.
The first ranks models by observed performance, which mixes the method used with the difficulty of the targets each model chose to forecast; the second ranks them with the target covariates held fixed.

`rank_models()`, `summarise_ranks()` and `plot_model_ranks()` compute the two rankings, the Spearman correlation between them, and a paired rank plot.
Reported as a supplementary figure with the summary statistics inline, so they track the fit.

## Unreleased — Rebuild the Background study-design table from epidemiological principles

`report/quarto/_background.qmd`, `report/quarto/_discussion.qmd`, `report/references.bib`

The Background table listed four study designs for handling forecast target difficulty (restriction, stratification, transformation, matching), but never said it was drawing on observational epidemiology, and used several of the borrowed terms loosely.
Rebuilt it as a single table of study design elements, with columns for the epidemiological counterpart, its forecast-evaluation form, the threat it addresses, and its limitation.
The label `tbl-approaches` is unchanged.

Rows follow the sequence of a study rather than a single formality ranking: what enters the sample, how exposure and outcome are measured, how units are compared, how the estimate is computed, and what remains.
An informal-to-formal ordering does not hold across all nine rows, because complete ascertainment and exposure classification are prerequisites to the comparison rather than weaker forms of it.
The formality argument now applies only to the elements governing how units are compared — matching, stratification, standardisation, regression adjustment — which is where the Discussion's argument actually sits.

The design-versus-analysis division and the membership of each group are standard, and are now cited [@mcnamee2005; @kahlert2017] rather than asserted.
Ranking the analysis-phase methods by increasing assumptions is our framing, not a received ordering, and the prose says so.
Kahlert et al. also name propensity score methods.
Rather than add a tenth row, these are noted in the Discussion as an option not pursued: weighting each forecast by the probability that its model submitted for that target would address differential participation directly, but requires the determinants of participation to be measured.

The table is restricted to tools available retrospectively, to an evaluator working with forecasts already submitted.
The protocol-level option — requiring every model to forecast every target, which would remove differential participation by design — moves into the prose, because it is available to hub organisers rather than to anyone evaluating afterwards.
The prose also states that in this field the design-versus-analysis distinction largely collapses, since even restriction and matching are applied retrospectively here.

Added an unadjusted comparison row at the head of the comparison block, so the sequence runs from no control at all through to full adjustment.
It carries no threat addressed; its limitation is that it conflates the method used with the difficulty of the targets each model chose to forecast, which is the comparison the rest of the table exists to improve on.

Two row labels changed in `report/quarto/_background.qmd`.
"Classification of exposure" became "Measurement of exposure", and "Classification of outcome" became "Scale of outcome".
The second row covers per-100,000 normalisation and the log transform, which rescale the outcome rather than categorise it, so "classification" described the wrong operation.

Corrections to the epidemiological terms.
Baseline-relative skill was called an active-comparator design; it is indirect standardisation, the same construction as a standardised mortality ratio, which also explains why the choice of baseline determines the result [@stapper].
"Transformation" welded two operations together: per-100,000 normalisation and the log transform are now one row on the shared principle that both express the outcome so it does not depend on a nuisance quantity, rates rather than counts and ratios rather than differences.
That row states plainly that this removes the dependence by redefining what is measured, not by adjusting for it.

Three strategies added that the old table omitted: complete ascertainment at the protocol stage, classification of the exposure blind to the outcome, and quantitative bias analysis.
The first two address selection and information bias, which the old table did not cover at all despite the Background naming both as problems.
The table now ends with the approach used here and with what a fully formal analysis would add, matching the informal-to-formal argument the Discussion makes.

New bibliography entries `vanderweele2017` (E-value) and `hernan2020` (causal inference) for the bias-analysis row.
Added `bosse2023` to the outcome-measure row, the substantive reference for the log transform and already cited for it in the Methods, and `cramer2022` to the stratification row.
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

## Unreleased — Correct the documented manuscript render command

`CLAUDE.md`, `README.qmd`, `README.md`

`CLAUDE.md` and `README` both documented `quarto::quarto_render("report/manuscript.qmd")` as the way to render the manuscript alone.
That command has never worked.
`report/manuscript.qmd` includes its sections with project-root-relative paths (`/report/quarto/_abstract.qmd`), and rendering a single file directly makes Quarto treat that file's own directory as the root, so the path resolves to `report/report/quarto/_abstract.qmd` and the include fails.

The correct target is `quarto render index.qmd`, which renders the manuscript alone with `index.qmd` (at the repo root) as the top-level document.
`quarto render` still builds the full two-page site.
No source file changes: switching the includes to paths relative to `manuscript.qmd` does not help, because Quarto resolves relative includes against the top-level document rather than the file containing the directive, which then breaks the site build.

Also corrected stale paths in the same docs: `report/quarto/supplement/_supplement.qmd` no longer exists (the supplement is `report/supplement.qmd`, self-contained), and `manuscript.qmd` no longer includes the supplement.
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
