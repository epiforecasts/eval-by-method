# Decisions to review

Choices made without asking, during the session of 19-20 August 2026. Each one is reversible; the commit is named so it can be found.

## Discussion restructured (commit: discussion pass)

Paragraph order changed from summary → limitations → specification → prior work → field → closing, to:

1. Summary
2. Prior work and the offsetting mechanism
3. Specification flexibility and the interaction
4. Limitations
5. Field-level recommendations
6. Closing

Reason: limitations came second, so the reader met the weaknesses of the sample before learning what the finding meant. No wording was changed in the moved paragraphs beyond splitting two long sentences.

Reverse by moving the limitations paragraph ("Our power to detect true differences...") back to immediately after the summary.

## Supplement rank figure replaced by a table

The unadjusted-versus-adjusted rank figure moved to the main text (Figure 3B). Rather than repeat it, the Supplement now carries a table of the ten models that move furthest under adjustment. If you would rather the Supplement kept the full figure, `plot_model_ranks()` still exists and takes the same `ranks` object.

## Anonymised model labels in the rank table

The Supplement movers table uses the anonymised labels ("Statistical Multi-country 3"), consistent with the figures. Real model names are available via `rank_models(effects, anonymise = FALSE)` if you would rather name them.

## Sensitivity claim about semi-mechanistic models

Results now states that a Gaussian family reverses the case-death contrast for semi-mechanistic models. This came from re-running the family comparison and is correct for the current fit, but it is a stronger caveat than the earlier text carried. Check you are comfortable stating it in the main text rather than only in the Supplement.

## Variant phase names in the descriptive sentence

The new sentence on unadjusted performance names the best and worst variant phases from the median LWIS. Those labels come straight from `VariantPhase` in the data, so they read as, for example, "Omicron-BA.1" rather than a prose form.

## Supporting Information listing replaced

`report/quarto/_references.qmd` listed Supplementary Figures 1-5 and Supplementary Table 1, which no longer matches the supplement: it now holds around fifteen figures and tables, and the "model diagnostics (cases)" and "(deaths)" entries describe per-outcome diagnostics that the joint model replaced with a single panel. Rather than renumber every float, the listing is now a single "S1 Text" entry describing what the supplement contains, which is also how PLoS treats a combined supplementary file.

If the journal wants each float listed separately, this needs an audit pass: every figure and table in the supplement given an S-number in order, and the main text updated to cite them.

## One stale S-number in Results

"Supplementary Table S2" in the classification sentence now reads "Supplement", since no S-numbering scheme survives. Same fix as above if numbering is reinstated.

## Supplement heading levels changed

"Epidemic trend identification" and "Variant phase identification" were fourth-level headings under "Model structure classification", which made them read as part of the classification procedure. They are siblings of it, so they are now third-level. "Why no separate structure main effect" moved from under "Covariate selection" to its own third-level section. The sensitivity subsection "Fitting" is renamed "Error family and link", to distinguish it from "Model fitting" earlier.

## Captions added to six supplement figures

Participation, LWIS density, residual diagnostics, observed against fitted, spatial effects, and temporal effects had no captions. Each now leads with what the figure shows, in the style used in the main text. The claims in them are descriptive and follow from the figure, but they are mine, so worth a read.

## Trend figures not merged

The supplement carried a TODO to panel the case and death trend figures side by side. Each is a 32-country facet grid, so merging them would give 64 panels in one figure and neither would be legible. They stay as two figures, with captions rewritten to name the trend classification rather than reading "Trends (cases)". The TODO is removed; say if you want them merged anyway.

## Participation TODO left in place

`report/supplement.qmd` still carries the TODO about writing participation counts to a CSV so they can be described from a single source. That is a code-organisation change rather than a writing one, so I have not touched it.

## Rank annotation placement

The Spearman annotation in the rank panel sits top-left and slightly overlaps one point. Moving it would need either a wider panel or a corner that is also occupied. Low stakes, but visible.

## Table numbering is manual and now off-by-one-corrected

The Background study-design table is cross-referenced (`@tbl-approaches`), so Quarto numbers it Table 1. The two Results tables are produced by `print_table1()` and `print_table2()` as plain `kable` output with no label, so Quarto does not number them, and the prose referred to them as "Table 1" and "Table 2" — colliding with the Background table. The prose now calls them Table 2 and Table 3, which matches the order a reader meets them.

This is a patch, not a fix. The durable fix is to give both Results tables chunk labels (`#| label: tbl-models`, `#| label: tbl-structure`) and captions as `#| tbl-cap`, then reference them as `@tbl-models` and `@tbl-structure` so the numbering maintains itself. That needs the captions moved out of `print_table1()` and `print_table2()` in `R/analysis-descriptive.R`, since a kable caption and a chunk `tbl-cap` would otherwise both render.
