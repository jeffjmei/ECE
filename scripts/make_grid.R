library(tidyr)

common <- list(
  # n        = c(200, 500, 1000),
  n        = c(2000),
  p        = c(2),
  # p        = c(3, 5, 10),
  # cov_type = c("compound", "ar1"),
  cov_type = c("compound"),
  r        = c(0),
  # r        = c(0.2, 0.3, 0.4),
  amp      = c(1),
  err_type = c("normal", "exponential"),
  method   = c("bs.multiplier", "bs.parametric", "z.test.gaussian", "z.test.kappa"),
  # method   = c("bs.multiplier", "bs.parametric"),
  seed     = 1,
  B        = 1000
)

scenario1 <- do.call(tidyr::crossing, c(list(scenario = 1), common))

scenario2 <- do.call(tidyr::crossing, c(list(scenario = 2), common, list(L = 4)))

config_grid <- dplyr::bind_rows(scenario1, scenario2)

write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message("Wrote ", nrow(config_grid), " configurations to scripts/param_grid.csv")
