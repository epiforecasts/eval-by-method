# Feasibility: reimplement the WIS GAMM in brms or Stan

## Context

Evaluating how hard it would be to port the primary forecast-evaluation GAMM
([R/analysis-model.R](../../Documents/Github/eval-by-method/R/analysis-model.R), the
`m.formula_joint` object) from `mgcv::bam` (fREML, frequentist) to a Bayesian fit in
**brms**.

## Term-by-term translation (mgcv → brms)

The current RHS:
```r
wis ~ Epi_target
    + s(Method, bs="re") + s(CountryTargets, bs="re") + s(Trend, bs="re")
    + s(Location, bs="re") + s(VariantPhase, bs="re") + s(Model, bs="re")
    + s(Incidence)
    + s(Horizon, by = Model, k = 3, bs = "sz")
```

| mgcv term | brms equivalent | Notes |
|---|---|---|
| `Epi_target` (fixed factor) | `Epi_target` | identical |
| `s(X, bs="re")` ×6 | `(1 \| X)` | mgcv's `bs="re"` random intercept = brms varying intercept. Direct. |
| `s(Incidence)` (thin-plate) | `s(Incidence)` | brms supports mgcv smooths natively via `s()`; reuses mgcv's basis constructor. Direct. |
| `s(Horizon, by=Model, k=3, bs="sz")` | `s(Horizon, by=Model, k=3)` **or** `(Horizon \| Model)` / `(1 + poly(Horizon,2) \| Model)` | The one non-trivial term — see below. |
| `family = gaussian(link="log")` | `family = gaussian(link="log")` | Direct. (And for the planned log-response sensitivity arm: model `log(wis)` with `gaussian()` — trivial in brms too.) |

A first-cut brms formula:
```r
bf(wis ~ Epi_target
     + (1|Method) + (1|CountryTargets) + (1|Trend)
     + (1|Location) + (1|VariantPhase) + (1|Model)
     + s(Incidence)
     + s(Horizon, by = Model, k = 3),
   family = gaussian(link = "log"))
```

## Handling `s(Horizon, by=Model, k=3, bs="sz")`

This is a **factor-smooth-by interaction** — a separate horizon curve per model (47
of them), sum-to-zero constrained (`bs="sz"`) so they are deviations from a shared
mean horizon effect. Two ways to carry it into brms:

1. **Keep it as a smooth:** `s(Horizon, by=Model, k=3)`. brms accepts `by=` smooths.
   But `bs="sz"` (the sum-to-zero factor-smooth basis) does not map cleanly to brms's
   default smooth handling; the closest idiomatic brms construct is a factor-smooth
   `s(Horizon, Model, bs="fs", k=3)`, which has a slightly different penalty/centering
   than `bs="sz"`. Estimates will be close but **not identical** — flag this if exact
   reproduction matters.
2. **Reparameterise as a varying slope (recommended for brms):** Horizon has only 4
   distinct values, so a smooth with `k=3` is near-equivalent to a low-order
   polynomial. `(1 + Horizon | Model)` (or `+ poly(Horizon, 2)` random) captures
   per-model horizon variation in a fully Bayesian, well-identified way and sidesteps
   the `bs="sz"` mismatch entirely. Cleaner priors, easier diagnostics. This is the
   pragmatic choice and arguably *better*-specified than forcing a 3-basis spline onto
   4 points.

Either way: **not a blocker**, but the term that needs a deliberate decision and a
sentence in the methods if results are reported from a brms fit.

## Cost / risk

- **Writing the model:** ~1–2 h (formula + priors + a smoke-test fit on a 5–10k-row
  subset to confirm it compiles and samples).
- **Priors:** brms supplies weakly-informative defaults; with 181k rows the
  likelihood dominates, so priors barely move the posterior. Low effort, but should
  be stated explicitly (the current fREML fit has none to report).
- **Sampling time — the real cost.** 181k rows × 6 REs (Model 47, Location 32, etc.)
  + a smooth + a by-smooth, under NUTS, is heavy. Expect **hours per chain**, possibly
  longer; memory can also bite (brms builds a large design matrix). Mitigations:
  `backend="cmdstanr"` (already installed, faster compile + run), `threads=threading(k)`
  for within-chain parallelism, run chains in parallel, and prototype on a subset.
- **Output plumbing:** the downstream code
  ([analysis-model.R:96-135](../../Documents/Github/eval-by-method/R/analysis-model.R#L96-L135))
  uses `gammit::extract_ranef` / `extract_fixed` and a hand-built effects table
  consumed by [_results.qmd](../../Documents/Github/eval-by-method/report/quarto/_results.qmd)
  and `print_table2`. None of that works on a `brmsfit`. You would rewrite extraction
  to pull posterior summaries (`brms::ranef()`, `fixef()`, `as_draws_df()`, or
  `tidybayes`/`marginaleffects`). This is **as much work as the modelling itself** —
  budget for it. The `[[group_var, group, value, lower_2.5, upper_97.5, se, model]]`
  contract the report depends on must be reproduced from posterior draws (CIs become
  credible intervals).

## To proceed

1. Subset to ~10k rows; fit the brms formula above with `backend="cmdstanr"`,
   2 chains × 500 iters, to confirm it compiles + samples + the `by` smooth behaves.
2. Decide the Horizon term (smooth vs varying slope) and write the one-line methods
   justification.
3. Fit on full data with threading; check `pp_check`, Rhat, ESS.
4. Rewrite output extraction to the report's effects-table contract from posterior
   draws (the genuinely time-consuming step).
