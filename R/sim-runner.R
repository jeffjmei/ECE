test.diag <- function(X, method = "bs.multiplier", ...) {
  switch(method,
    bs.multiplier = ece.test(X, type = "bs.multiplier", ...),
    z.test        = ece.test(X, type = "z.test", ...),
    stop("Unknown method: ", method)
  )
}

run_sim <- function(config_row, N = 1000) {
  standard_cols <- c("scenario", "n", "p", "cov_type", "r", "amp", "method", "B")
  extra_params  <- as.list(config_row[setdiff(names(config_row), standard_cols)])

  params <- do.call(scenario, c(
    list(
      scenario_num = config_row$scenario,
      n            = config_row$n,
      p            = config_row$p,
      r            = config_row$r,
      amp          = config_row$amp,
      cov_type     = config_row$cov_type
    ),
    extra_params
  ))

  p_vals <- replicate(N, {
    X <- generate_data(params)
    test.diag(X, method = config_row$method, B = config_row$B)$p.value
  })

  config_row |>
    dplyr::mutate(
      power           = mean(p_vals < 0.05),
      n_sim           = N,
      timestamp       = Sys.time(),
      scenario_params = jsonlite::toJSON(params$scenario_param),
      method_params   = jsonlite::toJSON(list(B = config_row$B))
    ) |>
    dplyr::select(scenario, n, p, cov_type, r, amp, method,
                  power, n_sim, timestamp, scenario_params, method_params)
}

save_sim <- function(result, file) {
  write.table(result, file,
    sep       = ",",
    row.names = FALSE,
    col.names = !file.exists(file),
    append    = TRUE
  )
}
