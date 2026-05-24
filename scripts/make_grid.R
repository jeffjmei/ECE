library(tidyr)
library(dplyr)

base_params <- list(
  n        = c(200, 500, 1000, 2000),
  p        = c(2),
  cov_type = c("compound"),
  r        = c(0, 0.1, 0.2, 0.3, 0.4),
  amp      = c(1),
  err_type = c("normal", "exponential"),
  seed     = 1,
  n_sims   = 1000
)

bs_methods <- c("bs.multiplier", "bs.parametric")
zt_methods <- c("z.test.gaussian", "z.test.kappa")

make_scenario <- function(scenario_id, extra = list()) {
  bs <- do.call(crossing, c(
    list(scenario = scenario_id), base_params,
    list(method = bs_methods, B = 1000), extra
  ))
  zt <- do.call(crossing, c(
    list(scenario = scenario_id), base_params,
    list(method = zt_methods), extra
  ))
  bind_rows(bs, zt)
}

config_grid <- bind_rows(
  make_scenario(1),
  make_scenario(2, list(L = 4))
)

write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message("Wrote ", nrow(config_grid), " configurations to scripts/param_grid.csv")
