---
name: add-data-chapter
description: Find a real dataset for a topic, screen it for a viable ECE naive-vs-oracle-vs-ECE story, and write a notes/<name>.qmd chapter for the ECE book's Real Data part. Use when the user wants to explore a new domain/dataset for the ECE notes (e.g. "let's look at manufacturing data", "find a dataset about X").
---

Pipeline: source data, screen for a viable story, write the chapter, register it, commit. Don't write the `.qmd` before the screening step has found something worth writing about.

## 1. Source a dataset

**Data Preference**.
- Prefer real data over simulated/benchmark data.
- Data should have a mean non-stationarity (mean shifts are preferred).
- Data must be multivariate time series with known expected behavior. For example, `MANEMP` (manufacturing employment) is known to be the sum of two other variables `DMANEMP` (durable manufacturing employment) and `NDMANEMP` (non-durable manufacturing employment), therefore, we expect the correlation between `MANEMP-DMANEMP` and `MANEMP-NDMANEMP` to be positive.
- Static, scriptable bulk downloads (a CSV/zip you can `curl`), not interactive report tools.

Download into `data/real-data/<name>/` (the whole `data/` directory is gitignored). Verify the download (row counts, column names, date range) before proceeding.

## 2. Screen before writing anything

Compute, for every candidate variable: a shift score (e.g. `|mean(first third) - mean(last third)| / sd`) and pairwise correlation among the most-shifted variables. Then apply these filters. Each one has killed a candidate in past sessions.

- Near-duplicate check: drop pairs with `|r| > ~0.95`. Almost always redundant sensors reading the same physical quantity, not two things worth a naive-vs-ECE comparison.
- Degenerate variance check: if one side of a shift has ~zero variance (a sensor that was off, not just at a different level), that's "instrument was off," not "same relationship obscured by a mean shift."
- Censoring/mass-at-zero check: `mean(x == 0)`. If a large fraction of a variable is exactly at a boundary value, correlations against it are unstable. Swap for a better-behaved variable.
- Full-resolution ACF check: compute the ACF of the mean-removed residual at native sampling resolution. If lag-1 is severe (>0.7-ish), ECE will break at short minimum segment lengths. Plan on resampling.
- Aliasing/periodicity check, at multiple lags: if the process has any known or suspected periodic structure (daily, weekly, seasonal), check ACF well past lag 4-5. A fixed-step downsample that divides evenly into the period aliases: it doesn't reduce dependence, it resamples the same phase repeatedly. Prefer resampling to a real event (e.g. "the day's peak reading") over a fixed clock step, then re-check the ACF at the next coarser period too (a fix for a daily cycle can reveal a weekly one underneath). This can recurse. At some point the honest conclusion is that the dataset's periodicity defeats resampling, not another workaround.

Show the user a plot of the best 1-3 candidates before writing the chapter. Use `AskUserQuestion` at real branch points (which candidate, which downsample/resample strategy) rather than silently picking.

## 3. Data analysis

**Residuals after modeling should be stationary**. In most of these data applications, model the mean using rolling means or segmentation. The purpose is to isolate the error terms. Once the data is sufficiently residualized, Pearson correlation can be appropriately run.

**ACF should be minimal**. One of the requirements of both ECE and Pearson correlation is that there should be no autocorrelation. In reality, this is hard to accomplish. One technique to try is down-sampling.

## 4. Write the chapter

Match the structure of chapters `notes/co2-inhalation.qmd` and `notes/copy-number-variation.qmd` as templates:

- Explain why we expect two time series to be correlated or not. Explain the variables and what might cause the mean non-stationarity.
- Note where the data can be downloaded.
- Add a frontmatter `title:`.
- Open the document with a `devtools::load_all()` and `library(tidyverse)` setup chunk.
- Load chunks are clearly labeled (`#| label: ...`). Cache-relevant labeling rules from `notes/CLAUDE.md` apply.

**Diagnostics**. In one `::: {.panel-tabset}` block, show all the diagnostics: Plot, Residuals, ACF, Trend.

- In `Plot`, show relevant covariates, with the smoothed mean / segmentation in blue, and the noisy raw data in gray.
- In `Residuals`, take the residuals and plot them. The original data should display some non-stationarity, and the residuals should be roughly stationary.
- In `ACF`, split into `Naive` and `Mean-Shift-Invariant` sub-tabs. In `Naive`, take the residuals and apply ACF. In `Mean-Shift-Invariant`, apply `plot_sip_acf()`.
- In `Trend`, split into `Naive` and `Mean-Shift-Invariant` sub-tabs. In `Naive`, use `plot_naive_terms()`. In `Mean-Shift-Invariant`, use `plot_ece_terms()`.

**Results**. Apply these methods and functions:
- Pearson (Naive): `cor()` and `cor_pval()` naive.
- Pearson (Residualized): `cor()` and `cor_pval()` on residuals.
- ECE: `ece.cor()` and `ece_pval()`.
- Bootstrap Multiplier: `ece.test(X_mat, type = "bs.multiplier")`.
- Parametric Bootstrap: `ece.test(X_mat, type = "bs.parametric.lag1.diag.psd")`.
- Parametric Bootstrap (naive): `ece.test(X_mat, type = "bs.parametric.naive")`.
- Bartlett Test: `cortest.bartlett()`.

**Conclusion**. Provide an honest conclusion, evaluating the diagnostics. Don't smooth over negative results.

**Writing style**: concise and plain. Say less rather than more. No em-dashes. No AI-tics ("it's worth noting," "dive into," "unlock," etc.). Plain declarative sentences.

**Before writing any chunk into the `.qmd`, verify it runs via a standalone `Rscript -e '...'` call** (`devtools::load_all(quiet=TRUE)` first). This has caught every real bug in this workflow. Don't skip it.

Never run `quarto render`. The user runs `quarto preview` live (`notes/CLAUDE.md`).

## 5. Register and commit

Add `- <name>.qmd` to `notes/_quarto.yml` under `part: real-data.qmd`, after the existing chapters.

Clean up any stray `Rplots.pdf` (base-R plotting without an explicit device, run outside a chunk) before committing.

Per global commit conventions: show the proposed commit message and wait for explicit approval before running `git commit`. Keep commits atomic. One chapter per commit is the established pattern in this repo's history, even when multiple chapters were written in the same session.
