library(tidyr)
library(dplyr)

base_params <- list(
  n        = c(200, 500, 1000, 2000),
  p        = c(5, 10),
  cov_type = c("compound", "ar1"),
  r        = c(0, 0.1, 0.2, 0.3, 0.4),
  amp      = c(1),
  err_type = c("normal", "exponential"),
  seed     = 1,
  n_sims   = 10000
)

scenario_specs <- tribble(
  ~scenario, ~L,
  1,         NA_real_,
  2,         4
)

method_specs <- tribble(
  ~method,           ~B,
  "bs.parametric",   1000,
  "z.test.gaussian", NA_real_
)

config_grid <- do.call(crossing, base_params) |>
  crossing(scenario_specs) |>
  crossing(method_specs)

write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message("Wrote ", nrow(config_grid), " configurations to scripts/param_grid.csv")
