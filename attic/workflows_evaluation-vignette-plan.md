# Forecast evaluation workflow: vignette plan

## Purpose

A how-to guide for forecast evaluation, guiding evaluators through a workflow with how this could be implemented in scoringutils or elsewhere.

Also related: a piece on the design of evaluations (eval-by-method issue [#174](https://github.com/epiforecasts/eval-by-method/issues/174), sketch in `attic/evaluation-design-sketch.qmd`).

## Framing

We suggest defining the design and aim of the evaluation in order to identify an appropriate evaluation method.

1.  Evaluation design
    -   Experimental: the evaluator controls the forecasting model(s) and selects the forecast target(s). Every model can forecast every target, so the forecast set is balanced by design.
    -   Observational: secondary analysis of independent forecasts; the selection of forecasters for each target might be biased or missing.
2.  Evaluation aim
    -   Describe: how accurate are these forecasts?
    -   Compare: which model is better, for which targets?
    -   Explain: what drives differences in performance?

A sketch of how these choices interact could include:

+-----------------------+-----------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------+
|                       | Describe                                                                    | Compare                                                                                                  | Explain                                                                                 |
+=======================+=============================================================================+==========================================================================================================+=========================================================================================+
| Experimental designs  | Results show: Accuracy of own model against a baseline                      | Results show: Versions of a model under development; re-runs of several models with standardised choices | Results show: Which model component matters (removing one at a time, factorial designs) |
|                       |                                                                             |                                                                                                          |                                                                                         |
|                       | Risks: Tuning on the test period; data that were not available in real time | Risks: Overfitting to the evaluation set over many iterations                                            | Risks: Few independent data points to learn from                                        |
+-----------------------+-----------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------+
| Observational designs | Results show: Track record of a published model                             | Results show: a leaderboard-style comparison                                                             | Results show: drivers of performance across many models and targets                     |
|                       |                                                                             |                                                                                                          |                                                                                         |
|                       | Risks: Selective reporting, publication bias                                | Risks: confounding, e.g. by selection bias among many independent forecasters                            | Risks: residual confounding, e.g. among many independent targets                        |
+-----------------------+-----------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------+

## Flow

``` mermaid
flowchart TD
  A[What decision will the forecasts inform?] --> B{Who decided which models<br/>forecast which targets?}
  B -- Evaluator --> C[Experimental]
  B -- Forecasters --> D[Observational]
  D -. re-run with standardised choices .-> C
  C --> E{How deep?}
  D --> E
  E --> F[1. Before scoring]
  F --> G[2. Score]
  G --> H[3. Summarise]
  H --> I{Comparing models?}
  I -- Yes --> J[4. Compare]
  I -- No --> L
  J --> K{Explaining variation?}
  K -- Yes --> M[5. Explain]
  K -- No --> L[6. Report]
  M --> L
```

## Section outline

Each section notes where emphasis differs between experimental and observational set-ups.

### 0. Find your starting point

The two questions, the starting points table and the flow diagram.

### 1. Before scoring

+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Step                                                                   | Experimental            | Observational              | scoringutils                                      |
+========================================================================+=========================+============================+===================================================+
| Check the target fits the decision (e.g. 7-1-7 for outbreak detection) | Same                    | Same                       | None                                              |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Use data vintages available in real time, not truncated final data     | Build into the pipeline | Check what forecasters had | None                                              |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Hold out an evaluation period not used in development                  | Essential               | Usually given              | None                                              |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Pre-specify strata and why they might matter                           | Same                    | Same                       | None                                              |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Choose a baseline with justification                                   | Same                    | Same                       | None                                              |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Diagnose the forecast set                                              | Confirm it is complete  | Find missing forecasts     | `get_forecast_counts()`, `plot_forecast_counts()` |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+
| Choose the scale for scoring                                           | Same                    | Same                       | `transform_forecasts()`, `log_shift()`            |
+------------------------------------------------------------------------+-------------------------+----------------------------+---------------------------------------------------+

### 2. Score

-   Use proper scoring rules for probabilistic forecasts.
-   Assess calibration alongside accuracy: sharpness subject to calibration.
-   Consider more than one metric, chosen for what the forecast user needs (reliable coverage, or risk at the extremes).
-   scoringutils: `score()`, `get_metrics()`, `select_metrics()`, `get_coverage()`, `get_pit_histogram()`, `plot_interval_coverage()`, `plot_quantile_coverage()`.

### 3. Summarise

-   Overall average score, then by each pre-specified stratum. Always by horizon.
-   Report the number of forecasts behind each summary (scoringutils [#1220](https://github.com/epiforecasts/scoringutils/issues/1220)).
-   scoringutils: `summarise_scores()`, `plot_wis()`, `plot_heatmap()`, `get_correlations()`.

### 4. Compare

-   Experimental: compare directly on the complete forecast set.
-   Observational: handle incomplete forecast sets before averaging. Options are filtering, imputation and pairwise relative skill, each with its own assumption.
-   Present relative scores alongside absolute scores.
-   Compare ranks as well as scores; differences in rank may be small in practice (Li et al. 2017).
-   Quantify uncertainty in comparisons: bootstrap over forecast dates and locations; model confidence sets (scoringutils [#1055](https://github.com/epiforecasts/scoringutils/issues/1055)) as an option where the audience expects them. Avoid p-values.
-   scoringutils: `filter_scores()`, `impute_missing_scores()`, `get_pairwise_comparisons()`, `add_relative_skill()`, `plot_pairwise_comparisons()`. See the "Handling missing forecasts" vignette.

### 5. Explain variation

-   Separate the difficulty of the target from the performance of the forecasting method (eval-by-method, Figure 1).
-   Experimental: vary one factor at a time, or re-run with standardised choices (Brockhaus et al. 2023).
-   Observational: a sequence of increasing formality, from stratification to regression adjustment (eval-by-method) to propensity weighting and explicit causal estimands. Match the formality to the question.
-   Treat scores as data: repeated forecasts from one model are not independent.
-   scoringutils stops at scores; model-based evaluation uses other packages (e.g. `mgcv`).

### 6. Report

-   Choose which periods or targets to show without cherry-picking: policy-relevant points, periods of rapid change, points that test specific assumptions, and median and extreme performance.
-   Label post-hoc analyses as such.
-   Visualise the chosen metric in terms the forecast user understands.

### 7. Beyond this vignette

-   Temporal coherence: stability of successive forecasts for the same target (e.g. Cramér distance).
-   Evaluation relative to an ensemble.
-   Experimental design of evaluations (eval-by-method #174).
-   Reporting standards for forecast evaluation (EPIFORGE covers forecasting in general).

## Open questions for co-authors

1.  Worked example: a simulated data set that can show both experimental and observational set-ups (preferred so far), or the package's `example_quantile` (observational only)?
2.  One vignette, or a short overview vignette plus one article per starting point?
3.  Placement on the pkgdown site (scoringutils [#1159](https://github.com/epiforecasts/scoringutils/issues/1159) proposes grouping articles).
4.  Which signposted features, if any, should become scoringutils issues (counts in summaries, bootstrap intervals, temporal coherence)?

## References and links

-   Brockhaus et al. (2023). Why are different estimates of the effective reproductive number so different? PLOS Comput Biol 19(11): e1011653. <https://doi.org/10.1371/journal.pcbi.1011653>
-   Sherratt et al. eval-by-method. <https://epiforecasts.io/eval-by-method/>
-   Kim, Ray and Reich (2026). Missing forecasts in model importance metrics. Int J Forecast. <https://doi.org/10.1016/j.ijforecast.2025.12.006>
-   Bosse et al. (2023). Scoring epidemiological forecasts on transformed scales. PLOS Comput Biol 19(8): e1011393. <https://doi.org/10.1371/journal.pcbi.1011393>
-   Stapper and Funk. Mind the baseline (reference to confirm).
-   Li et al. (2017), multiple metrics and rank (reference to confirm).
-   7-1-7 framework. <https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(23)00133-X/fulltext>
-   scoringutils vignette: Handling missing forecasts. <https://epiforecasts.io/scoringutils/articles/handling-missing-forecasts.html>