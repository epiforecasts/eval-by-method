# Plan / change log

Running log of analysis and manuscript changes. Newest first.

## 2026-06-28 — Temporal autocorrelation diagnostic

**Problem:** the joint GAMM carries no temporal correlation structure, so it treats
weekly forecast scores (horizons 1–4) as conditionally independent given the random
effects. Needed to quantify within-model score autocorrelation to justify or qualify
that assumption.

**Changes:**
- `R/sensitivity/check-autocorrelation.R` (new): refits the log-response arm
  (`model_wis_logresp()`), takes its near-symmetric residuals, and reports (A) per-series
  lag-1..4 ACF on `log(WIS)`, (B) the same on residuals, (C) cross-horizon correlation at
  a fixed forecast origin. Auto-runs on the CLI; `CHECK_AUTOCORR_NORUN=1` sources the
  function only (for the supplement).
- `report/quarto/supplement/_supplement.qmd`: new "Temporal autocorrelation" subsection
  under Model diagnostics — ACF table, cross-horizon residual correlation matrix, and an
  honest SE caveat.

**Findings:** serial autocorrelation is modest and short-lived (median lag-1 ≈ 0.37 raw,
0.33 residual; ~0 by lag 3–4 across 3,353 series). Stronger dependence is across horizons
sharing a forecast origin (residual r ≈ 0.39–0.76). Treated as a diagnostic only — no AR
structure added to the production model; SEs read as a lower bound.

Added a spaghetti plot (`spaghetti_plot` in the returned list; 300 sampled origins,
residual across horizons 1–4, faceted by target) visualising the within-origin
dependence, and expanded the supplement limitation prose: an origin-level random effect
would absorb the correlation but is collinear with the within-origin trend/variant/
incidence/location terms, so the correction belongs on the SEs (block bootstrap over
origins) not the model structure. Noted that the between-origin covariates (trend,
variant) — not the focal Method/CountryTargets effects — are the ones whose precision is
most overstated, since the correlation lives within origins where those covariates are
constant.

## 2026-06-27 — Main text reports exp() ratios only; raw log effects → supplement

**Problem:** main-text figures, Table 2, and prose reported partial effects on both
the raw log-WIS scale and the exponentiated ratio scale side-by-side. The raw
log-scale coefficients are uninterpretable to an epi audience; the multiplicative
ratio (1 = grand-mean WIS) is the natural quantity and matches RR/IRR conventions.

**Changes:**
- `R/plot-model-results.R`: `plot_effects()` and `plot_models()` now plot
  `exp(value)` with `exp()` CI bounds, reference line at 1.0 (was 0), `scale_y_log10()`,
  and axis label "Performance ratio (vs average model)". Smooth terms (Incidence,
  Horizon) were already excluded from these plots, so no wiggly-curve issue.
- `R/analysis-descriptive.R` (`print_table2`): dropped the two raw `ci_text` columns;
  table now shows only Unadjusted/Adjusted ratio columns. Caption reworded to ratio
  framing (below 1 = better than average) and points to the Supplement for raw effects.
- `report/quarto/_results.qmd`: inline Trend/Variant/Deaths stats switched from
  `value_ci` → `ratio_ci` (already computed); Fig 2/3 captions and the methods
  interpretation paragraph reworded for ratios.
- `report/quarto/supplement/_supplement.qmd`: added a raw log-scale coefficient table
  (`supplement-raw-coefficients`) so the dropped main-text numbers remain available.

## 2026-06-27 — Model-fit diagnostics + log-response sensitivity arm

Commit `3cef9db` (and `21695b9` for the prerequisite include fix).

**Problem:** the primary GAMM uses `gaussian(link = "log")` on the raw WIS. A log
*link* models the score mean multiplicatively but assumes Gaussian errors on the raw,
right-skewed score scale, so observation-level residuals were heavily skewed (skew
≈ 5.5, kurtosis ≈ 62).

**Changes:**
- `R/analysis-model.R`: explicit `filter(!is.na(wis))` in preprocessing (`bam` was
  dropping these 260 rows silently); save observed-vs-fitted as `output/<dir>/fit_obs.rds`
  with a `stopifnot` row-alignment guard; hoisted the shared `m.formula_joint` to file
  scope. Primary effect estimates verified unchanged (max abs diff 0).
- `R/sensitivity/model-logresp.R` (new): refits the joint model on `log(WIS)` with an
  identity link (near-zero scores floored at 1e-4). Residual skew improves ≈ 5.5 → −0.8;
  effect directions preserved (focal correlation ≈ 0.96); model-structure conclusions
  identical (Method/CountryTargets effects ~0 under both). Trend/Variant magnitudes
  attenuate on the log-response scale — reported honestly.
- `R/plot-model-results.R`: added `plot_fit_obs()` observed-vs-fitted helper.
- `report/quarto/supplement/_supplement.qmd`: observed-vs-fitted figure, honest note on
  residual skew, new "Sensitivity: log-transformed response" sub-section.
- `report/quarto/_results.qmd`: note the residual skew and point to the sensitivity check.

Prerequisite fix (commit `21695b9`): repointed the supplement diagnostics include from
the orphaned per-target `check_Cases.pdf` / `check_Deaths.pdf` to the single
`check_joint.pdf` left by the joint-model refactor.

**Related (assessment only, no code):**
`~/.claude/plans/reimplement-gamm-brms-stan-assessment.md` — feasibility of porting the
GAMM to brms/Stan. Verdict: brms is an easy formula translation but expensive to run
(181k rows under NUTS) and requires rewriting the output-extraction layer; raw Stan not
warranted.

## (earlier) — Joint-target refactor (Goal 1)

Commits `50fa79a`, `7e31b8e`, `66fc5f5`. Replaced the two per-target stratified `bam()`
fits with a single joint model carrying `epi_target` as a fixed parametric factor
(Deaths vs Cases contrast). Collapsed downstream extraction, plots, tables, and prose
to the single fit. Plan: `~/.claude/plans/implemennt-goal-1-swift-teacup.md`.
