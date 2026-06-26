# Plan / change log

Running log of analysis and manuscript changes. Newest first.

## 2026-06-27 — Model-fit diagnostics + log-response sensitivity arm

Commit `3cef9db` (and `21695b9` for the prerequisite include fix).

**Problem:** the primary GAMM uses `gaussian(link = "log")` on the raw WIS. A log
*link* models the score mean multiplicatively but assumes Gaussian errors on the raw,
right-skewed score scale, so observation-level residuals were heavily skewed (skew
≈ 5.5, kurtosis ≈ 62) and the supplement QQ plot looked "wild".

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
