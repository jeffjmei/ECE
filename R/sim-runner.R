test.diag <- function(X, method = "bs.multiplier", ...) {
  switch(method,
    bs.multiplier = ece.test(X, type = "bs.multiplier", ...),
    z.test        = ece.test(X, type = "z.test", ...),
    stop("Unknown method: ", method)
  )
}

run_sim <- function(config_row, N = 1000) {
  params <- scenario(
    scenario_num = config_row$scenario,
    n            = config_row$n,
    p            = config_row$p,
    r            = config_row$r,
    amp          = config_row$amp,
    cov_type     = config_row$cov_type
  )

  results <- replicate(N, {
    X <- generate_data(params)
    test.diag(X, method = config_row$method, B = config_row$B)
  }, simplify = FALSE)

  meta <- dplyr::mutate(
    config_row,
    n_sim           = N,
    timestamp       = Sys.time(),
    scenario_params = jsonlite::toJSON(params$scenario_param),
    method_params   = jsonlite::toJSON(list(B = config_row$B))
  )

  metrics <- list(
    data.frame(metric_type = "power",
               metric_val  = mean(sapply(results, `[[`, "p.value") < 0.05))
  )
  if (!is.null(results[[1]]$estimate)) {
    metrics <- c(metrics, list(
      data.frame(metric_type = "estimate",
                 metric_val  = mean(sapply(results, `[[`, "estimate")))
    ))
  }

  dplyr::bind_rows(lapply(metrics, \(m) dplyr::bind_cols(meta, m)))
}
