library(tidyr)

config_grid <- tidyr::crossing(
  scenario = c(1, 2, 3),
  n        = c(200, 500, 1000),
  p        = c(2, 3, 5, 10),
  cov_type = c("compound", "ar1"),
  r        = c(0, 0.1, 0.5),
  amp      = c(0.5, 1),
  method   = c("bs.multiplier"),
  B        = 1000
)

write.csv(config_grid, "scripts/param_grid.csv", row.names = FALSE)
message("Wrote ", nrow(config_grid), " configurations to scripts/param_grid.csv")
