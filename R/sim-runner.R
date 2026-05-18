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
    )
}
