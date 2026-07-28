#' @export
test.diag <- function(X, method = "bs.multiplier", B = NULL, params = NULL, ...) {
  b_arg <- if (!is.null(B)) list(B = B) else list()
  switch(method,
    bs.multiplier = do.call(ece.test, c(list(X, type = "bs.multiplier"), b_arg, list(...))),
    bs.multiplier.center = do.call(ece.test, c(list(X, type = "bs.multiplier.center"), b_arg, list(...))),
    bs.multiplier.nosplit = do.call(ece.test, c(list(X, type = "bs.multiplier.nosplit"), b_arg, list(...))),
    bs.parametric = do.call(ece.test, c(list(X, type = "bs.parametric"), b_arg, list(...))),
    bs.parametric.psd = do.call(ece.test, c(list(X, type = "bs.parametric.psd"), b_arg, list(...))),
    bs.parametric.mask = do.call(ece.test, c(list(X, type = "bs.parametric.mask"), b_arg, list(...))),
    bs.parametric.mask.psd = do.call(ece.test, c(list(X, type = "bs.parametric.mask.psd"), b_arg, list(...))),
    bs.parametric.lag1 = do.call(ece.test, c(list(X, type = "bs.parametric.lag1"), b_arg, list(...))),
    bs.parametric.lag1.psd = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.psd"), b_arg, list(...))),
    bs.parametric.lag1.mask = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.mask"), b_arg, list(...))),
    bs.parametric.lag1.mask.psd = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.mask.psd"), b_arg, list(...))),
    bs.parametric.lag1.diag = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.diag"), b_arg, list(...))),
    bs.parametric.lag1.diag.mask = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.diag.mask"), b_arg, list(...))),
    bs.parametric.lag1.diag.psd = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.diag.psd"), b_arg, list(...))),
    bs.parametric.lag1.diag.mask.psd = do.call(ece.test, c(list(X, type = "bs.parametric.lag1.diag.mask.psd"), b_arg, list(...))),
    z.test = ece.test(X, type = "z.test", kappa = ece.kappa(X, method = "mom"), ...),
    z.test.gaussian = ece.test(X, type = "z.test", kappa = "gaussian", ...),
    z.test.oracle = ece.test(X, type = "z.test", kappa = params$kappa, ...),
    z.test.null = ece.test(X, type = "z.test", kappa = ece.kappa(X, rho = 0, method = "mom"), rho = 0, ...),
    z.test.null.matrix = ece.test(X, type = "z.test", kappa = ece.kappa(X, rho = 0, method = "matrix"), rho = 0, ...),
    z.test.null.regress = ece.test(X, type = "z.test", kappa = ece.kappa(X, rho = 0, method = "regress"), rho = 0, ...),
    z.test.null.gaussian = ece.test(X, type = "z.test", kappa = "gaussian", rho = 0, ...),
    z.test.null.oracle = ece.test(X, type = "z.test", kappa = params$kappa_null, rho = 0, ...),
    segment.oracle = segment_test(X, params, penalty = "Oracle", ...),
    segment.aic = segment_test(X, params, penalty = "AIC", ...),
    segment.bic = segment_test(X, params, penalty = "BIC", ...),
    stop("Unknown method: ", method)
  )
}

#' @export
run_sim <- function(config_row, N = 1000) {
  standard_cols <- c("scenario", "n", "p", "cov_type", "r", "amp", "err_type", "method", "B", "seed", "mean_seed", "n_sims")
  extra_params <- as.list(config_row[setdiff(names(config_row), standard_cols)])
  extra_params <- extra_params[!is.na(extra_params)]

  mean_seed <- if (is.null(config_row$mean_seed) || is.na(config_row$mean_seed)) 1 else config_row$mean_seed

  params <- do.call(scenario, c(
    list(
      scenario_num = config_row$scenario,
      n            = config_row$n,
      p            = config_row$p,
      r            = config_row$r,
      amp          = config_row$amp,
      cov_type     = config_row$cov_type,
      err_type     = config_row$err_type,
      seed         = mean_seed
    ),
    extra_params
  ))

  set.seed(config_row$seed)
  seeds <- sample.int(.Machine$integer.max, N)
  method <- config_row$method
  B <- config_row$B
  start_time <- proc.time()
  p_vals <- purrr::map_dbl(seeds, purrr::in_parallel(\(s) {
    set.seed(s)
    tryCatch(
      {
        X <- generate_data(params)
        test.diag(X, method = method, B = B, params = params)$p.value
      },
      error = function(e) {
        message("Replicate error: ", conditionMessage(e))
        NA_real_
      }
    )
  }, params = params, method = method, B = B))
  end_time <- proc.time()
  runtime <- (end_time - start_time)[["elapsed"]] / 60

  base_row <- config_row |>
    dplyr::mutate(
      n_sim = N,
      runtime = runtime,
      timestamp = Sys.time(),
      scenario_params = as.character(jsonlite::toJSON(c(
        params$scenario_param[setdiff(names(params$scenario_param), "cp")],
        list(mean_seed = mean_seed)
      ))),
      method_params = as.character(if (config_row$method %in% c("bs.multiplier", "bs.multiplier.center", "bs.parametric")) {
        jsonlite::toJSON(list(B = config_row$B))
      } else {
        jsonlite::toJSON(list())
      })
    )

  dplyr::bind_rows(
    dplyr::mutate(base_row, metric = "power", metric_val = mean(p_vals < 0.10, na.rm = TRUE)),
    dplyr::mutate(base_row, metric = "n_errors", metric_val = sum(is.na(p_vals)))
  ) |>
    dplyr::select(
      scenario, n, p, cov_type, r, amp, err_type, method, seed,
      metric, metric_val, n_sim, runtime, timestamp, scenario_params, method_params
    )
}

#' @export
run_sim_t1 <- function(config_row, N = 1000) {
  standard_cols <- c("scenario", "n", "p", "cov_type", "r", "amp", "err_type", "method", "B", "seed", "mean_seed", "n_sims")
  extra_params <- as.list(config_row[setdiff(names(config_row), standard_cols)])
  extra_params <- extra_params[!is.na(extra_params)]

  mean_seed <- if (is.null(config_row$mean_seed) || is.na(config_row$mean_seed)) 1 else config_row$mean_seed

  params <- do.call(scenario, c(
    list(
      scenario_num = config_row$scenario,
      n            = config_row$n,
      p            = config_row$p,
      r            = config_row$r,
      amp          = config_row$amp,
      cov_type     = config_row$cov_type,
      err_type     = config_row$err_type,
      seed         = mean_seed
    ),
    extra_params
  ))

  set.seed(config_row$seed)
  seeds <- sample.int(.Machine$integer.max, N)
  method <- config_row$method
  B <- config_row$B
  start_time <- proc.time()
  p_vals <- purrr::map_dbl(seeds, purrr::in_parallel(\(s) {
    set.seed(s)
    tryCatch(
      {
        X <- generate_data(params)
        test.diag(X, method = method, B = B, params = params)$p.value
      },
      error = function(e) {
        message("Replicate error: ", conditionMessage(e))
        NA_real_
      }
    )
  }, params = params, method = method, B = B))
  end_time <- proc.time()
  runtime <- (end_time - start_time)[["elapsed"]] / 60

  breaks <- seq(0, 1, by = 0.05)
  bucket_names <- sprintf("p%02d", seq(0, 95, by = 5))
  counts <- hist(p_vals[!is.na(p_vals)], breaks = breaks, plot = FALSE)$counts
  buckets <- as.list(counts / N)
  names(buckets) <- bucket_names

  base_row <- config_row |>
    dplyr::mutate(
      n_errors = sum(is.na(p_vals)),
      n_sim = N,
      runtime = runtime,
      timestamp = Sys.time(),
      scenario_params = as.character(jsonlite::toJSON(c(
        params$scenario_param[setdiff(names(params$scenario_param), "cp")],
        list(mean_seed = mean_seed)
      ))),
      method_params = as.character(if (config_row$method %in% c("bs.multiplier", "bs.multiplier.center", "bs.parametric")) {
        jsonlite::toJSON(list(B = config_row$B))
      } else {
        jsonlite::toJSON(list())
      })
    )

  dplyr::bind_cols(
    dplyr::select(
      base_row, scenario, n, p, cov_type, r, amp, err_type, method, seed,
      n_errors, n_sim, runtime, timestamp, scenario_params, method_params
    ),
    dplyr::as_tibble(buckets)
  )
}

#' @export
save_sim <- function(result, file) {
  write.table(result, file,
    sep       = ",",
    row.names = FALSE,
    col.names = !file.exists(file),
    append    = TRUE
  )
}
