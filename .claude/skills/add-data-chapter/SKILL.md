---
name: add-data-chapter
description: Find a real dataset for a topic, screen it for a viable ECE naive-vs-oracle-vs-ECE story, and write a notes/<name>.qmd chapter for the ECE book's Real Data part. Use when the user wants to explore a new domain/dataset for the ECE notes (e.g. "let's look at manufacturing data", "find a dataset about X").
---

Pipeline: source data, screen for a viable story, write the chapter, register it, commit. Don't write the `.qmd` before the screening step has found something worth writing about.

## 1. Source a dataset

Prefer real measured data over simulated/synthetic benchmarks. Prefer datasets with:
- A documented mean-shift mechanism: a known event date/log (GPS antenna swap, dam closure), an experimentally induced phase (CO2 inhalation), or a real categorical regime label recorded alongside the data (steel plant `Load_Type`). This gives a genuine oracle, far stronger than discovering shifts yourself.
- Static, scriptable bulk downloads (a CSV/zip you can `curl`), not interactive report tools.
- Multiple genuinely related but distinct variables/series, not just one series.

Download into `data/<name>/` (the whole `data/` dir is gitignored). Verify the download (row counts, column names, date range) before proceeding.

If no documented mechanism exists (e.g. anonymized sensors), you can still screen blindly, but say so upfront in the chapter.

## 2. Screen before writing anything

Compute, for every candidate variable: a shift score (e.g. `|mean(first third) - mean(last third)| / sd`) and pairwise correlation among the most-shifted variables. Then apply these filters. Each one has killed a candidate in past sessions.

- Near-duplicate check: drop pairs with `|r| > ~0.95`. Almost always redundant sensors reading the same physical quantity, not two things worth a naive-vs-ECE comparison.
- Degenerate variance check: if one side of a shift has ~zero variance (a sensor that was off, not just at a different level), that's "instrument was off," not "same relationship obscured by a mean shift."
- Censoring/mass-at-zero check: `mean(x == 0)`. If a large fraction of a variable is exactly at a boundary value, correlations against it are unstable. Swap for a better-behaved variable.
- Full-resolution ACF check: compute the ACF of the mean-removed residual at native sampling resolution. If lag-1 is severe (>0.7-ish), ECE will break at short minimum segment lengths. Plan on resampling.
- Aliasing/periodicity check, at multiple lags: if the process has any known or suspected periodic structure (daily, weekly, seasonal), check ACF well past lag 4-5. A fixed-step downsample that divides evenly into the period aliases: it doesn't reduce dependence, it resamples the same phase repeatedly. Prefer resampling to a real event (e.g. "the day's peak reading") over a fixed clock step, then re-check the ACF at the next coarser period too (a fix for a daily cycle can reveal a weekly one underneath). This can recurse. At some point the honest conclusion is that the dataset's periodicity defeats resampling, not another workaround.

Show the user a plot of the best 1-3 candidates before writing the chapter. Use `AskUserQuestion` at real branch points (which candidate, which downsample/resample strategy) rather than silently picking.

## 3. Write the chapter

Match the structure of recent chapters (`notes/air-pollution.qmd`, `notes/steel-industry.qmd`, `notes/gps-antenna-offset.qmd`) as templates:

- Frontmatter `title:`, then a `devtools::load_all()` / `library(tidyverse)` setup chunk.
- Intro prose: data source and access method, variables and their physical meaning, why a real relationship is plausible, what the mean-shift mechanism is (documented or discovered).
- Load chunk(s), clearly labeled (`#| label: ...`). Cache-relevant labeling rules from `notes/CLAUDE.md` apply.
- If native resolution breaks ECE, add a Full Record tier (document the breakdown honestly, including `NaN`/out-of-range results) plus a resampled tier.
- Per tier, nested `::: {.panel-tabset}`: Plot / Residuals / ACF / Trend / Correlation / P-Values. ACF and Trend each split into Naive-or-Oracle vs. Mean-Shift-Invariant sub-tabs (`plot_sip_acf()`, `plot_ece_terms()`, `plot_naive_terms()` from the package; hand-roll an "Oracle" version with `ave(x, grp)` when a real regime label exists).
- Correlation/P-Values tabs: three-way table (ECE via `ece.cor()`/`ece_pval()`, Oracle-or-Rolling via `cor()`/`cor_pval()` on residuals, Naive Pearson via `cor()`/`cor_pval()` on raw data), `knitr::kable(..., caption = ...)` with `#| layout-ncol: 3`.
- Closing "Bootstrap Methods" section: `ece.test(X_mat, type = "bs.multiplier")` and `"bs.parametric"`.
- A closing "Where this leaves us" paragraph with the honest conclusion, including "this is a dead end" if that's what the diagnostics show. Don't smooth over messy or negative results. Some chapters in this book (SECOM, Steel Industry) exist specifically to document why a dataset didn't work.

**Writing style**: concise and simple. Say less rather than more. No em-dashes. No AI-tics ("it's worth noting," "dive into," "unlock," etc.). Plain declarative sentences.

**Before writing any chunk into the `.qmd`, verify it runs via a standalone `Rscript -e '...'` call** (`devtools::load_all(quiet=TRUE)` first). This has caught every real bug in this workflow. Don't skip it.

Never run `quarto render`. The user runs `quarto preview` live (`notes/CLAUDE.md`).

## 4. Register and commit

Add `- <name>.qmd` to `notes/_quarto.yml` under `part: real-data.qmd`, after the existing chapters.

Clean up any stray `Rplots.pdf` (base-R plotting without an explicit device, run outside a chunk) before committing.

Per global commit conventions: show the proposed commit message and wait for explicit approval before running `git commit`. Keep commits atomic. One chapter per commit is the established pattern in this repo's history, even when multiple chapters were written in the same session.
