test.diag <- function(X, method = "bs.multiplier", B = NULL, params = NULL, ...) {
  b_arg <- if (!is.null(B)) list(B = B) else list()
  switch(method,
    bs.multiplier   = do.call(ece.test, c(list(X, type = "bs.multiplier"), b_arg, list(...))),
    bs.parametric   = do.call(ece.test, c(list(X, type = "bs.parametric"),  b_arg, list(...))),
    z.test.gaussian = ece.test(X, type = "z.test", ...),
    z.test.kappa    = ece.test(X, type = "z.test", params = params, ...),
    stop("Unknown method: ", method)
  )
}

run_sim <- function(config_row, N = 1000) {
  standard_cols <- c("scenario", "n", "p", "cov_type", "r", "amp", "err_type", "method", "B", "seed", "n_sims")
  extra_params <- as.list(config_row[setdiff(names(config_row), standard_cols)])
  extra_params <- extra_params[!is.na(extra_params)]

  params <- do.call(scenario, c(
    list(
      scenario_num = config_row$scenario,
      n            = config_row$n,
      p            = config_row$p,
      r            = config_row$r,
      amp          = config_row$amp,
      cov_type     = config_row$cov_type,
      err_type     = config_row$err_type
    ),
    extra_params
  ))

  set.seed(config_row$seed)
  start_time <- proc.time()
  p_vals <- replicate(N, {
    X <- generate_data(params)
    test.diag(X, method = config_row$method, B = config_row$B, params = params)$p.value
  })
  end_time <- proc.time()
  run_time <- (end_time - start_time)[["elapsed"]]

  config_row |>
    dplyr::mutate(
      metric          = "power",
      metric_val      = mean(p_vals < 0.10),
      n_sim           = N,
      run_time        = run_time,
      timestamp       = Sys.time(),
      scenario_params = jsonlite::toJSON(params$scenario_param),
      method_params   = if (config_row$method %in% c("bs.multiplier", "bs.parametric"))
        jsonlite::toJSON(list(B = config_row$B)) else jsonlite::toJSON(list())
    ) |>
    dplyr::select(
      scenario, n, p, cov_type, r, amp, err_type, method, seed,
      metric, metric_val, n_sim, run_time, timestamp, scenario_params, method_params
    )
}

save_sim <- function(result, file) {
  write.table(result, file,
    sep       = ",",
    row.names = FALSE,
    col.names = !file.exists(file),
    append    = TRUE
  )
}
