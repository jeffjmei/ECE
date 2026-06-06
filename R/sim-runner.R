test.diag <- function(X, method = "bs.multiplier", B = NULL, params = NULL, ...) {
  b_arg <- if (!is.null(B)) list(B = B) else list()
  switch(method,
    bs.multiplier        = do.call(ece.test, c(list(X, type = "bs.multiplier"),        b_arg, list(...))),
    bs.multiplier.nosplit = do.call(ece.test, c(list(X, type = "bs.multiplier.nosplit"), b_arg, list(...))),
    bs.parametric        = do.call(ece.test, c(list(X, type = "bs.parametric"),        b_arg, list(...))),
    bs.parametric.psd    = do.call(ece.test, c(list(X, type = "bs.parametric.psd"),    b_arg, list(...))),
    z.test.gaussian      = ece.test(X, type = "z.test", kappa = "gaussian", ...),
    z.test.oracle        = ece.test(X, type = "z.test", kappa = params$kappa, ...),
    z.test.null          = ece.test(X, type = "z.test", kappa = ece.kappa(X, rho = 0, method = "mom"),    rho = 0, ...),
    z.test.null.matrix   = ece.test(X, type = "z.test", kappa = ece.kappa(X, rho = 0, method = "matrix"), rho = 0, ...),
    z.test.null.gaussian = ece.test(X, type = "z.test", kappa = "gaussian",   rho = 0, ...),
    z.test.null.oracle   = ece.test(X, type = "z.test", kappa = params$kappa, rho = 0, ...),
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
    tryCatch({
      X <- generate_data(params)
      test.diag(X, method = config_row$method, B = config_row$B, params = params)$p.value
    }, error = function(e) {
      message("Replicate error: ", conditionMessage(e))
      NA_real_
    })
  })
  end_time <- proc.time()
  runtime <- (end_time - start_time)[["elapsed"]] / 60

  base_row <- config_row |>
    dplyr::mutate(
      n_sim           = N,
      runtime         = runtime,
      timestamp       = Sys.time(),
      scenario_params = as.character(jsonlite::toJSON(params$scenario_param)),
      method_params   = as.character(if (config_row$method %in% c("bs.multiplier", "bs.parametric"))
        jsonlite::toJSON(list(B = config_row$B)) else jsonlite::toJSON(list()))
    )

  dplyr::bind_rows(
    dplyr::mutate(base_row, metric = "power",    metric_val = mean(p_vals < 0.10, na.rm = TRUE)),
    dplyr::mutate(base_row, metric = "n_errors", metric_val = sum(is.na(p_vals)))
  ) |>
    dplyr::select(
      scenario, n, p, cov_type, r, amp, err_type, method, seed,
      metric, metric_val, n_sim, runtime, timestamp, scenario_params, method_params
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
