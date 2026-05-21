library(tidyr)

common <- list(
  n        = c(200, 500, 1000),
  p        = c(2, 3, 5, 10),
  cov_type = c("compound", "ar1"),
  r        = c(0, 0.1),
  amp      = c(1),
  err_type = c("normal", "exponential"),
  method   = c("bs.multiplier", "bs.parametric", "z.test.gaussian", "z.test.kappa"),
  seed     = 1,
  B        = 1000
)

scenario1 <- do.call(tidyr::crossing, c(list(scenario = 1), common))

scenario2 <- do.call(tidyr::crossing, c(list(scenario = 2), common, list(L = 4)))

config_grid <- dplyr::bind_rows(scenario1, scenario2)

write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message("Wrote ", nrow(config_grid), " configurations to scripts/param_grid.csv")
