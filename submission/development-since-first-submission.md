# Development since the first submission

Internal record for co-authors, 25 August 2026.

- Baseline: commit `d89f813` (12 April 2025), submitted to PLOS Computational Biology as PCOMPBIOL-D-25-00717 and posted as [medRxiv 10.1101/2025.04.10.25325611](https://doi.org/10.1101/2025.04.10.25325611).
- Since then: 182 commits, a retitled and reframed manuscript, a different model specification, and a revision against three reviewers.
- Other records: archived submission in `submission/first/`; running change log in `NEWS.md`; session-level choices made without asking in `submission/decisions-to-review.md`.

## Contents

- [Development since the first submission](#development-since-the-first-submission)
  - [Contents](#contents)
  - [Conceptual development](#conceptual-development)
  - [Implementation](#implementation)
  - [Evaluation](#evaluation)
  - [Open before resubmission](#open-before-resubmission)
  - [Appendix A: reviewer comments](#appendix-a-reviewer-comments)
  - [Appendix B: specifications tried, and further work considered](#appendix-b-specifications-tried-and-further-work-considered)
    - [B1. Model specifications](#b1-model-specifications)
    - [B2. Further work considered](#b2-further-work-considered)

## Conceptual development

The paper moved from an empirical question to a methodological one.
The structure result is now the demonstration case, not the finding.

- Retitled, from "The influence of model structure and geographic specificity on predictive accuracy among European COVID-19 forecasts" to "Interpreting variation in infectious disease forecast performance with model-based evaluation" ([index.qmd:2](../index.qmd#L2)).
- New question: how should comparative evaluation be designed when forecasters choose their own targets. The old question was which model structures forecast better, answered with a null.
- Two processes separated: forecast-generating and target-generating ([_background.qmd:4](../report/quarto/_background.qmd#L4), with a new diagram). Named the two problems participatory hubs create — repeated forecasts from one model are not independent (pseudoreplication), and aggregating across targets conflates method with target difficulty.
- Study-design table ([_background.qmd:68-79](../report/quarto/_background.qmd#L68-L79)): forecast-evaluation practice on one axis of observational-study tools, from unadjusted comparison through inclusion criteria, matching, stratification and indirect standardisation to regression adjustment. Built at nine rows on two axes, cut to six on one.
- Discussion closes on the same axis, placing this work as a middle ground and naming what more formality would look like: propensity weighting, a fully specified causal estimand, quantitative bias analysis ([_discussion.qmd:42-46](../report/quarto/_discussion.qmd#L42-L46)).
- Sparse-data bias is now the stated reason stratification fails here, with Results quantifying how few forecasts survive cross-classification ([_results.qmd:204-208](../report/quarto/_results.qmd#L204-L208)).
- Geographic specificity demoted: a co-headline result in the submitted abstract, now one adjusted covariate reported descriptively. It left the title.
- The null on structure reread as offsetting: effects point in opposite directions for cases and deaths, so any term averaging over outcomes recovers approximately zero. Offered as an explanation for earlier null findings in the literature, not only for ours.
- New headline quantity: rank instability. Spearman 0.44 between crude and adjusted rankings, 23 of 48 models moving at least ten places. Promoted from supplement to main text.

## Implementation

| | Submitted (`d89f813`) | Current (`1ca9f45`) |
| --- | --- | --- |
| Fits | Two, stratified by outcome | One joint fit, `Epi_target` as unpenalised fixed effect (#153) |
| Family, link | `gaussian(link = "log")` | `tw(link = "log")`, p = 1.93 (#159) |
| Structure term | `s(Method, bs = "re")` | `s(Method, Epi_target, bs = "re")`, no separate main effect (#158) |
| Time and place | `s(time, by = location, k = 40)` | `s(VariantPhase, bs = "re")` + `s(Incidence)` |
| Zero scores | `wis + 1e-7` | Retained as exact zeros (#166) |
| Scale | Raw counts | Incidence per 100,000, log-transformed (LWIS) |
| Sample | 181,851 forecasts, 47 models | 207,713 forecasts, 48 models |
| Residual skew, kurtosis | 5.84, 77.5 | 0.57, 9.98 |
| Deviance explained | 0.286 | 0.382 |

Why each change:

- One joint fit: the stratified design gave no direct deaths-versus-cases contrast, forced a facet through every figure and table, and estimated each covariate effect twice on half the data.
- Structure crossed with outcome: fitted alongside a main effect, mgcv gave the main effect 0.001 effective degrees of freedom against 4.9 for the crossed term. The pooled per-structure effect is recovered as a contrast averaging a structure's two cells, accounting for their covariance.
- Constant dropped: 553 forecasts (0.27%) scored exactly zero and had been parked at log(1e-7), about eleven log-units below the next smallest score. The Tweedie's point mass at zero makes it unnecessary.

Two bugs invalidated intermediate work:

- Population normalisation was added in `aca4fa7` (April 2026) but referenced a `pop` column where `populations.csv` supplies `population`. `process-score.R` errored, the committed score files stayed stale from December 2025 holding WIS on raw counts, and every fit until `c0bd509` (27 July 2026) used un-normalised scores.
- `plot_config` conflated colour values and level order into one factor keyed by hex code, silently sorting levels alphabetically by colour string. Produced an all-NA axis in Figure 3 and an uncoloured legend in Figure 4B.

Code and infrastructure:

- `R/` reorganised on `process-` / `analysis-` / `plot-` / `utils-` prefixes: 24 files, +2,152 / −825, with new `utils-effects.R`, `utils-variants.R`, `dag-check.R` and `sensitivity/`.
- `utils-effects.R` exists because `gammit::extract_ranef()` cannot handle a factor-by-factor random effect — it reads only the last variable name of an interaction, collects five labels for a ten-coefficient term, and fails, taking down extraction for every term in the fit. The replacement rebuilds each smooth's design matrix from the formula mgcv stores on the smooth object, validated against gammit on a no-interaction fit.
- Manuscript prose split into `report/quarto/_*.qmd`; Zotero plain-text links replaced by BibTeX and a PLOS CSL; two-page Quarto site deployed to GitHub Pages.
- `archive_diagnostics()` writes one row per specification to `output/diagnostics/fit-summary.csv`, so successive fits stay comparable.

## Evaluation

- Error family: four fitted on the current specification, archived in `output/diagnostics/fit-summary.csv`. Gamma gives the best residual skew but does not converge and has no support at zero. Gaussian leaves the skew in the residuals. Tweedie with and without the zero-displacing constant differ in skew by 0.01, which ruled the constant out as the cause of the misfit.
- Link: refitting with an identity link gives near-identical structure and country-scope effects (`R/sensitivity/check-link-robustness.R`).
- Gaussian refit of the interaction, reported as a sensitivity: the case–death contrast holds for judgement, mechanistic and statistical structures, vanishes for agent-based, and reverses for semi-mechanistic. A Gaussian fit is dominated by the largest scores, which fall disproportionately among case forecasts.
- Temporal autocorrelation, diagnosed rather than modelled: median lag-1 residual autocorrelation about 0.33 across 3,353 weekly series, near zero by lags three and four, cross-horizon residual correlation 0.39 to 0.76. No AR term added — a forecast-origin random effect would absorb the correlation but is collinear with the within-origin trend, variant, incidence and location terms, so the correction belongs on the standard errors, which are read as a lower bound.
- DAG formalised in `R/dag-check.R` (#162). Adding the epidemiological outcome as a confounder rather than a plain covariate closes the backdoor from unmeasured modeller strategy. Querying the diagram returns exactly the fitted covariate set as a minimal sufficient adjustment set for the direct effect, and no valid set for the total effect — hence a direct, partial association, and the paper calling itself exploratory.

## Open before resubmission

- No Author Summary, which PLOS requires and the first submission carried.
- Abstract is 344 words against a 300-word limit.
- Five reviewer comments outstanding (Appendix A), one of them a reverted change.
- Main figures not exported to .tif or .eps (#135); figures are not written to `output/` at all (#173).
- `R/sensitivity/check-autocorrelation.R` does not run — it sources `model-logresp.R`, deleted in `c5a7c93`, and needs rebasing onto another residual source.
- `R/README.md:36-38` and `:96-104` and the project `CLAUDE.md` describe a specification and file layout that no longer exist.
- "Under development" callout still in `report/manuscript.qmd:12-14`.

## Appendix A: reviewer comments

Verified against the manuscript and code at `1ca9f45`, not against the tracker.
`submission/Revision_reviews-response.md` stays as the working file with the planned response text; its X marks are unreliable, and the discrepancies are listed after the table.

Key items:

- 5 comments still outstanding: 1.10, 1.12, 3.1, 3.5, Ed.2.
- 5 more were dissolved by the rescoping rather than answered: 1.17, 1.22, 1.23, 1.25, 1.27.
- The tracker cannot be trusted as the basis for the rebuttal letter.

| ID | Rev | Comment | Status | Evidence |
| --- | --- | --- | --- | --- |
| 1.1 | 1 | Results prose repetitive, loosely organised | Addressed | `953e0c8`, prose cut 29% |
| 1.2 | 1 | Use figures and tables better; add subheadings | Addressed | `_results.qmd:58,126,202,304,343`; `_methods.qmd:1,19,29,37,45` |
| 1.3 | 1 | Methods do not set up what Results will report | Partly | Estimand at `_methods.qmd:17,41-43`; decomposition follows Results order; no explicit signposting |
| 1.4 | 1 | Put the GAMM formula in the main text | Addressed | `_methods.qmd:63-84,92` |
| 1.5 | 1 | Standardise effect and covariate terminology | Addressed | LWIS naming pass (#165); `_methods.qmd:16`, `supplement.qmd:481` |
| 1.6 | 1 | Location-by-time RE oversoaks signal; use variant phases | Addressed | Time spline removed; `analysis-model.R:33`; `supplement.qmd:284-306` |
| 1.7 | 1 | Why smoothing splines at all? | Partly | Only two smooths remain (`analysis-model.R:27,35`), described at `_methods.qmd:92` but not justified |
| 1.8 | 1 | Model should nest within Method | Partly | Aliasing explained `supplement.qmd:356-366`; fit is crossed, not nested; branch `nest-model-in-method` unmerged |
| 1.9 | 1 | Unadjusted results likely spurious; adjustment is the point | Addressed | `_methods.qmd:17,41-43`; sparsity `_results.qmd:204-208` |
| 1.10 | 1 | Sub-national targets? Population size among single-country models? | Outstanding | No mention in `report/` or `R/`; #106 closed with no commit |
| 1.11 | 1 | Justify WIS over CRPS | Addressed, thin | `_methods.qmd:15`, one clause |
| 1.12 | 1 | State forecasts are of count data | Outstanding, regressed | Added `93145e4`, reverted `b1aa259`; #108 closed claiming completion |
| 1.13 | 1 | Why a log link when WIS is already on a log scale? | Addressed | `_methods.qmd:62-72`; `supplement.qmd:676-681`; `check-link-robustness.R` |
| 1.14 | 1 | QQ plot: dispersion looks off | Addressed | Tweedie family (#159); comparison `supplement.qmd:406-435` |
| 1.15 | 1 | Abstract vague on structure and target | Addressed | `_abstract.qmd:8` |
| 1.16 | 1 | Models are designed for different purposes | Addressed, brief | `_background.qmd:58` |
| 1.17 | 1 | Specificity is not the same as number of countries | No longer applicable | Claim removed with the rescoping; `supplement.qmd:310-316`, `_methods.qmd:26` |
| 1.18 | 1 | Matched-round analysis needs no confounder model | Addressed | Matching row in `tbl-approaches`, `_background.qmd:72,82` |
| 1.19 | 1 | Call "Other" models expert-judged | Addressed | "Judgement" throughout; `_methods.qmd:21`, `process-data.R:32` |
| 1.20 | 1 | Give R and package versions | Addressed, partial | `supplement.qmd:458` names mgcv 1.9-4 and R 4.5 only |
| 1.21 | 1 | Drop forecastsMean; explain bracketed numbers | Addressed | `831668b`; table rebuilt with gtsummary, `_results.qmd:62-65,105-113` |
| 1.22 | 1 | Figure 1 may mislead | No longer applicable | Per-structure descriptive figure replaced by LWIS bands vs incidence, `_results.qmd:128-140` |
| 1.23 | 1 | Distinguish panel colours; panel C unnecessary | No longer applicable | Panel C removed `831668b`; figure now single-panel |
| 1.24 | 1 | Highlight the ensembling implication | Partly | `_background.qmd:57`; crude comparison `_results.qmd:162-186`; no dedicated Discussion paragraph |
| 1.25 | 1 | Multi-country models may be poorly calibrated per country | No longer applicable | Both scope estimates shrink to the average, `_results.qmd:312-313` |
| 1.26 | 1 | Value of real-time then retrospective evaluation | Partly | `_discussion.qmd:22` covers revision, not iterative evaluation |
| 1.27 | 1 | The 4-week/WIS focus is a strength | No longer applicable | The limitation framing was deleted rather than reframed |
| 1.28 | 1 | Look at variant-dominated phases | Addressed, implemented | `analysis-model.R:33`; `_results.qmd:184,308` |
| 2.1 | 2 | Put the model in the main manuscript | Addressed | `_methods.qmd:63-92` |
| 2.2 | 2 | How was adjustment performed? | Addressed | `_methods.qmd:96`; `_results.qmd:208-209,242-251` |
| 2.3 | 2 | Figure 3 poorly described; legend incomplete | Addressed | `163b6f8`; `plot_config` bug fixed; figure now single-panel |
| 2.4 | 2 | Improve the model or add features | Addressed | VariantPhase, EpiTarget, Tweedie, structure-by-outcome interaction; `supplement.qmd:654-693` |
| 2.5 | 2 | Include the ensemble | Reasoned rejection, softened | Exclusion and reason `_methods.qmd:7`; crude comparison added `_results.qmd:162-186` |
| 3.1 | 3 | Reword the Author Summary on dataset size | Outstanding | No Author Summary exists in `report/manuscript.qmd` |
| 3.2 | 3 | Clarify who classified what on the first pass | Addressed | `_methods.qmd:23-24` |
| 3.3 | 3 | Where did classification disagreements fall? | Addressed | `_results.qmd:122`; per-model votes `supplement.qmd:219-234` |
| 3.4 | 3 | Natural-scale results in the Supplement | Partly | `supplement.qmd:639-652` gives natural-scale descriptives only; the fit exists in `output/natural/` but no effect table or figure is shown |
| 3.5 | 3 | Did multi-country models fit countries jointly? | Outstanding | No statement anywhere; #132 closed with no commit |
| 3.6 | 3 | What should Hub metadata improve? | Partly | `_discussion.qmd:26,29` give two of five planned recommendations |
| Ed.1 | Editor | Provide an editable manuscript source | Needs redoing | Done for the first submission; pipeline is now HTML-only |
| Ed.2 | Editor | Figures as .tif or .eps | Outstanding | #135 open; no export target in `_quarto.yml` |
| Ed.3 | Editor | Supply or remove Figures S4 and S5 | Addressed | Supporting Information collapsed to one S1 Text, `_references.qmd:4-7` |

Counts over 42 rows: 23 addressed, 7 partly addressed, 5 outstanding, 5 no longer applicable, 1 rejected with reasons (2.5), 1 to redo at resubmission (Ed.1).

Comments 1.17, 1.22, 1.23, 1.25 and 1.27 all concern geographic specificity or the descriptive structure figure, neither of which survives the rescoping.
The rebuttal letter should say so explicitly rather than claim these were addressed.

Tracker discrepancies to fix before the letter is written:

- Unticked but done: 1.4, 1.5, 1.13, 1.16, 1.20, 1.21, 2.3.
- Ticked or closed with no supporting change: 1.10 (#106), 1.12 (#108), 3.1 (#128), 3.5 (#132).
- Rows 1.7 and 1.16 claim Methods opens with a "Study aim" subsection. It opens with "Study design" (`_methods.qmd:1`); the subsection went in the prose cut.
- Row 1.1 names a subheading "Forecaster characteristics". The source reads "Participant characteristics".

## Appendix B: specifications tried, and further work considered

### B1. Model specifications

Fit statistics are in `output/diagnostics/fit-summary.csv`, keyed on (`spec_label`, `scale`); diagnostic panels are the matching PNGs in `output/diagnostics/`.

| Label | What it tested | Outcome |
| --- | --- | --- |
| — | Two fits stratified by outcome, Gaussian log link | Superseded by #153: no direct outcome contrast, faceted every float, each covariate estimated twice on half the data |
| `tweedie-log` | Joint fit with a structure main effect | Superseded by #158: assumes each structure predicts cases and deaths equally well |
| `tweedie-method-target` | Interaction and main effect together | Rejected, aliased. mgcv gave the main effect 0.001 EDF against 4.9 for the crossed term; dropping it changed nothing |
| `tweedie-interaction-only` | Interaction alone | Kept as the structural form. Numerically indistinguishable from the above, confirming the main effect carried nothing |
| — | `Epi_target` as a random rather than fixed effect | Rejected: two levels give no basis for estimating a variance. Left unpenalised, the fixed effect takes the component common to all structures, and the crossed effects then average to zero within outcome to numerical precision — emergent, with no centring constraint applied |
| `family-gaussian-log` | Gaussian log link on the current formula | Rejected. Leaves the skew in the residuals (skew 5.85, kurtosis 77.6, deviance explained 0.286) |
| `family-gamma-log` | Gamma log link | Rejected. Best residual skew of any arm (0.52) but does not converge on either scale, and has no support at zero |
| `family-tweedie-log` / `primary-interaction` | Tweedie, exact zeros retained | Kept. p = 1.93 on the log scale |
| `family-tweedie-offset` | Tweedie with the 1e-7 constant | Negative finding, reported: the constant was not driving the skew, which moves by 0.01 without it |
| — | Log-transformed response, identity link | Rejected on principle, not fit (`c5a7c93`). Residual skew improved and focal effects correlated 0.96 with the primary fit, but modelling log(score) loses propriety |
| — | Double-log fit | Dropped, same propriety argument (`0004dea`) |
| — | `te(Incidence, Growth)` difficulty surface, three arms | Abandoned, `attic/inc-gr-tensor.R`, committed as a dead end. Horizon 1 only, no results archived |
| — | Interaction at the level of individual model (#158, option 3) | Not built. Adding the structure interaction left individual model effects essentially unchanged (correlation 0.995), and a per-model effect is constant across outcomes so cannot represent a within-model difference |
| — | Reclassify judgement models as "Ensemble" (#158, option 4; #24) | Not built. Would allow the Hub ensemble back in; no code written |
| — | Nest Model within Method (reviewer 1.8) | Branch `nest-model-in-method`, two commits, never merged. Handled instead as crossed random effects |
| — | Horizon basis `bs="fs"` vs `bs="sz"` (#57); time-spline k (#58) | `bs="sz"` adopted to centre on zero given the by-model intercept; the k question resolved by removing the time spline |

`attic/model-building.qmd` was meant to be the record of this process. It is an incomplete stub with broken fences, a duplicated unadjusted fit, and a pre-interaction formula, and names three checks never built:

- participation rate against the US Hub;
- differential attrition by model type;
- misclassification sensitivity reallocating models by second-ranked rater vote.

### B2. Further work considered

`attic/future-work.qmd` lists eight causal-inference extensions, deferred on the stated grounds that the aim is exploratory rather than causal and that the reviewer comments were about specification, figures and terminology.

| Idea | Initial work | Status |
| --- | --- | --- |
| Formal DAG and algorithmic adjustment set | `R/dag-check.R` | Done (#162) |
| Precise estimand; marginal standardisation | Estimand paragraph in Methods | Partly done; G-computation only in the abandoned regime-standardisation notebook |
| Positivity and overlap cross-tabulation | `a046045` reports how sparse the sample becomes | Not done |
| Missingness by model and week; IPW for response | Median participation row in Table 1 | Not done |
| Effective sample size from the ICC | — | Not done |
| Explicit exchangeability and no-interference statement | Ensemble excluded from the fit for this reason | Not written as a Methods paragraph |
| E-value for unmeasured confounding | — | Not done; `@vanderweele2017` survives as a Discussion citation |
| AR(1) residual structure, block bootstrap | `R/sensitivity/check-autocorrelation.R` | Diagnosed, deliberately not modelled |
| Direct standardisation over epidemic regime | `attic/regime-standardised-scores.qmd`, 288 lines, rendered | Dead end with a usable negative result: standardising over phase leaves the case–death ratio unchanged, ruling phase out; level cannot be standardised across outcomes because a rate per 100,000 does not mean the same thing for cases and deaths, and level is where the difficulty gradient sits. Surfaced that `log(x+1)` after per-100,000 normalisation makes death scores closer to absolute error and case scores closer to relative error |
| Forecast stability (Cramer distance on revisions) | Branch `exploratory-stability`, `2bd13ae`, `attic/stability.R` and `.qmd` | Never merged |
| Lagged ensemble | Same branch, `attic/ensemble-lag.R` | Result: worse at every horizon on both outcomes. Not merged |
| Bayesian reimplementation in brms/Stan | `attic/reimplement-gamm-brms-stan-assessment.md`, term-by-term translation | Rejected on sampling time (hours per chain) and because `bs="sz"` does not map cleanly; rewriting effect extraction is as much work as the modelling |
| Data-revision covariate from Hub `anomalies.csv` | `attic/plan-target-fixed-effect.md`, marked plan only | Feasibility rated easy, but anomaly coverage skews to hospitalisations and early 2021; coverage needs checking first |
| Port to the US COVID-19 Forecast Hub | `attic/port-to-us-forecast-hub.md` | Not started. About 80 US models already classified in `data/model-classification.csv`; deaths only, since US `inc case` carries seven quantiles; the variant-phase rewrite for CDC data is the long pole |
| Rankings across the full ladder of evaluation designs, and a FluSight extension (#168) | `ffd5fc9`, `f7d0fa9` | Only the two-design comparison built (unadjusted vs adjusted, Spearman 0.44). The ladder and FluSight not attempted |
| Standalone DAG methodology paper | Branch `origin/paper/dag-reasoning`, `3a82d6d`, 118 lines | Not developed. Covers total vs direct vs controlled direct effects, a confounder taxonomy, and colliders from ensemble inclusion |
| Propensity matching on target similarity | `attic/notebook.qmd`, one line | Idea only. The only propensity reference in the repo |
| Contributor counts and publication records as a proxy for team resources | `attic/notebook.qmd` | Not started. The measurable version of the unmeasured confounder named in Methods |
| Why judgement forecasters do better on one target than another (#158) | — | Not started. Would need a policy tracker or data-revision history |
| Misclassification sensitivity by second-ranked rater vote | `attic/model-building.qmd:34` | Never built. #129 and #130 were answered descriptively instead |
| WIS decomposition into dispersion, over- and underprediction | `attic/decompose-wis.R`, 19 lines | Never run; brms and rstan are deliberately kept out of `renv.lock` |
| Ensemble-relative scoring scratch | `attic/correlations.R` | No conclusion recorded, no output saved, unreferenced |
