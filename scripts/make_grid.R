library(tidyr)
library(dplyr)
library(readr)

# --- Which results set to diff against: comment out the one you don't want ---
results_file <- "data/results.csv"
# results_file <- "data/results-t1.csv"

# --- Maximum Total Number of Submissions ---
max_submit <- 995

# --- Make Parameter Grid ---
base_params <- list(
  n        = c(200, 500, 1000, 2000, 3000, 5000),
  p        = c(2, 3, 5, 10, 15, 20, 25),
  # cov_type = c("compound", "ar1"),
  cov_type = c("compound"),
  # r        = c(0),
  r        = c(0, 0.1, 0.2, 0.3, 0.4),
  amp      = c(1),
  err_type = c("normal", "exponential"),
  seed     = 1,
  n_sims   = 10000
)

scenario_specs <- tribble(
  ~scenario, ~L,
  1, NA_real_,
  2, 4
)

method_specs <- tribble(
  ~method, ~B,
  # "bs.multiplier", 1000,
  # "bs.multiplier.nosplit", 1000,
  # "bs.parametric", 1000,
  # "bs.parametric.psd", 1000,
  # "bs.parametric.mask", 1000,
  # "bs.parametric.mask.psd", 1000,
  # "bs.parametric.lag1", 1000,
  # "bs.parametric.lag1.psd", 1000,
  # "bs.parametric.lag1.mask", 1000,
  # "bs.parametric.lag1.mask.psd", 1000,
  # "bs.parametric.lag1.diag", 1000,
  # "bs.parametric.lag1.diag.mask", 1000
  "bs.parametric.lag1.diag.psd", 1000
  # "bs.parametric.lag1.diag.mask.psd", 1000
  # "z.test", NA_real_,
  # "z.test.gaussian", NA_real_,
  # "z.test.null", NA_real_,
  # "z.test.null.gaussian", NA_real_,
  # "z.test.null.matrix", NA_real_,
  # "z.test.null.regress", NA_real_,
  # "z.test.oracle", NA_real_,
  # "z.test.null.oracle", NA_real_
)

# Final Changes
full_grid <- do.call(crossing, base_params) |>
  crossing(scenario_specs) |>
  crossing(method_specs) |>
  # z.test.* methods require a 2-column matrix (R/ece_test.R:164)
  filter(!(startsWith(method, "z.test") & p != 2)) |>
  # compound and ar1 are identical covariance structures when p == 2; keep only one
  filter(!(p == 2 & cov_type == "ar1"))

# --- Compare Against Existing Simulations ---
join_cols <- c("n", "p", "cov_type", "r", "amp", "err_type", "seed", "n_sims", "scenario", "method")
if (file.exists(results_file)) {
  existing <- read_csv(results_file, show_col_types = FALSE) |>
    rename(n_sims = n_sim) |>
    select(all_of(join_cols)) |>
    distinct()
} else {
  existing <- full_grid[0, join_cols]
}
missing_grid <- full_grid |>
  anti_join(existing, by = join_cols)

# Remove if too many submissions
config_grid <- missing_grid |> slice_head(n = max_submit)

# --- Write Parameter Grid ---
write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message(
  nrow(missing_grid), " configurations missing from ", results_file,
  "; wrote ", nrow(config_grid), " to scripts/param_grid.csv"
)
