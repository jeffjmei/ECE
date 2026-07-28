# Notes — Claude Context

## Style
Concise. Prefer adding content over removing it. When in doubt, leave existing text alone and append.

## Structure
Quarto book (`_quarto.yml`). Chapters cover theory, proofs, and diagnostics. Proofs and remarks go in collapsible callouts (`collapse="true"`).

## Diagnostics
- Analytical checks use `test_that` + `expect_equal` in collapsible callout-note chunks
- Long simulations (1000+ reps) get `#| cache: true`
- Load package at the top of each chunk with `devtools::load_all("~/Documents/Research/Code/ECE")`
- Give cached/expensive chunks explicit labels (e.g. ```{r my-sim-name}```), not the
  auto-assigned `unnamed-chunk-N`. Knitr's cache key includes the label, and
  `unnamed-chunk-N` is purely positional — inserting or removing any chunk earlier
  in the file reshuffles every later chunk's auto-label, silently invalidating the
  cache and forcing expensive simulations to rerun even when their code didn't change.
- Conversely, a cached chunk's key is based only on its OWN code text, not on
  helper functions it calls that are defined in an earlier (uncached) chunk.
  Editing such a helper without touching the cached chunk's own text will NOT
  invalidate its cache — it silently reuses stale results (can even error, e.g.
  a `distinct()` on a column a newly-added-to-the-function no longer matches
  the cached data). Fix: label the helper-defining chunk and add
  `#| dependson: ["helper-chunk-label"]` to every cached chunk that calls it.

## Workflow
- The user renders these `.qmd` files live themselves (e.g. `quarto preview`) while
  working. Do not run `quarto render` after edits — just save the file changes and
  let the user's live render pick them up.

## Plots
- ggplot2 with `theme_minimal()`
- p-value histograms: `breaks = seq(0, 1, by = 0.05)` for exactly 20 bars
