# ECE Package — Claude Context

## Project
R package implementing the Equivariant Covariance Estimator (ECE) for correlation
estimation under change points. See arxiv.org/abs/2108.09431. The `notes/` directory
is a Quarto book (`_quarto.yml`) documenting theory, proofs, and diagnostics.

## R Conventions
- Use `|>` not `%>%`
- Load package in scripts/qmd chunks with `devtools::load_all("~/Documents/Research/Code/ECE")`
- Tests live in `tests/testthat/`; run with `devtools::test()`

## Key Functions
- `scenario(scenario_num, n, r, L, err_type, ...)` — build a params list for a simulation scenario
- `generate_data(params)` — draw one dataset from a scenario
- `ece.test(X, type, params)` — main test; `type` is `"z.test"`, `"bs.multiplier"`, or `"bs.parametric"`
- `test.diag(X, method, params)` — thin wrapper around `ece.test` used in diagnostics; method names are `"z.test.gaussian"`, `"z.test.kappa"`, `"bs.multiplier"`, `"bs.parametric"`
- `ece.cor.se(X)` — plug-in SE estimate from data
- `ece.cor.se.formula(params)` — oracle SE from true params
- `ece.cor.se.matrix(params)` — SE via ∇g Σ_Z ∇g (should equal formula)

## Simulation Workflow
1. `scripts/make_grid.R` — builds `scripts/param_grid.csv`, a crossed grid of scenarios, sample sizes, methods, and error types
2. `run_sim(config_row, N)` — runs N replicates for one row of the grid, returns power at α = 0.10
3. `save_sim(result, file)` — appends one result row to a CSV
4. Typical pattern: read the grid, filter to rows of interest, `map` over rows with `run_sim`, accumulate with `save_sim`
