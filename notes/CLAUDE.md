# Notes — Claude Context

## Style
Concise. Prefer adding content over removing it. When in doubt, leave existing text alone and append.

## Structure
Quarto book (`_quarto.yml`). Chapters cover theory, proofs, and diagnostics. Proofs and remarks go in collapsible callouts (`collapse="true"`).

## Diagnostics
- Analytical checks use `test_that` + `expect_equal` in collapsible callout-note chunks
- Long simulations (1000+ reps) get `#| cache: true`
- Load package at the top of each chunk with `devtools::load_all("~/Documents/Research/Code/ECE")`

## Plots
- ggplot2 with `theme_minimal()`
- p-value histograms: `breaks = seq(0, 1, by = 0.05)` for exactly 20 bars
