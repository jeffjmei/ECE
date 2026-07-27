# ECE: Equivariant Covariance Estimator

Correlation underlies many methods, but classical correlation estimation requires i.i.d. data. This is a problem, because time series data are often neither independent nor identically distributed. Applying classical correlation estimators on non-i.i.d. data will produce spurious correlation.

**ECE** is an R package that estimates correlation between two series in the presence of unknown, piecewise-constant mean shifts (change points) that can differ in timing and size between the two series, without first detecting or modeling those change points.

Click [here](https://jeffjmei.github.io/projects/2_equivariant-covariance-estimation/) to learn more.

## Installation

```r
devtools::install_github("jeffjmei/ECE")
```

For local development:

```r
devtools::load_all("~/Documents/Research/Code/ECE")
```

## Quickstart

```r
library(ECE)

# Build a simulation scenario: n observations, true correlation r,
# marginal SDs sigma = c(sigma_x, sigma_y)
params <- scenario(scenario_num = 1, n = 1000, r = 0.5, sigma = c(1.2, 1.5))
X <- generate_data(params)

# Test H0: rho = 0
ece.test(X, type = "z.test")
#> $estimate
#> [1] 0.528
#> $se
#> [1] 0.042
#> $conf.int
#> [1] 0.446 0.611
#> $p.value
#> [1] 5.1e-36
```

`type` selects the inference method: `"z.test"` (asymptotic normal), `"bs.multiplier"` (multiplier bootstrap), or `"bs.parametric"` (parametric bootstrap).

## Core API

| Function | Purpose |
|---|---|
| `scenario(scenario_num, n, r, sigma, ...)` | Build a parameter list for a simulation scenario |
| `generate_data(params)` | Draw one dataset from a scenario |
| `ece.test(X, type, ...)` | Hypothesis test for $\rho = 0$; `type` is `"z.test"`, `"bs.multiplier"`, or `"bs.parametric"` |
| `ece.cor(X)` / `ece.cov(X)` | Point estimate of correlation / covariance |
| `ece.cor.se(X)` | Plug-in standard error from data |
| `ece.kappa(X, method)` | Kurtosis-type nuisance parameter used by the z-test |
| `run_sim(config_row, N)` | Run `N` replicates of a simulation grid row; returns power at $\alpha = 0.10$ |

See `?ece.test` and the other function docs (`man/`) for full argument details.

## Documentation

- [Notes](https://jeffjmei.github.io/ECE/) — a Quarto book of raw research notes covering the estimation theory, test derivations, and diagnostics as they were worked out; not a polished writeup
- `vignettes/` — worked examples on real data (stock market, air quality, CO₂, gas prices, occupancy sensors, random walks)

## Testing

The package is covered by unit tests (`tests/testthat/`) validating the estimators, test statistics, and simulation helpers against known-correct cases, to guard against regressions as the estimation code evolves.

```r
devtools::test()
```

## Project structure

```
R/            package source
man/          generated documentation
tests/        testthat unit tests
notes/        Quarto book: theory, proofs, diagnostics
vignettes/    real-data application examples
scripts/      simulation grid + batch runners
```
