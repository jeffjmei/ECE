kappa_from_params <- function(params) {
  e  <- generate_err(modifyList(params, list(n = 1e7)))
  ex <- e[, 1] / sqrt(params$S[1, 1])
  ey <- e[, 2] / sqrt(params$S[2, 2])
  list(
    k40 = mean(ex^4), k04 = mean(ey^4),
    k22 = mean(ex^2 * ey^2),
    k31 = mean(ex^3 * ey), k13 = mean(ex * ey^3)
  )
}

mean_flat <- function(n, p, amp, ...) {
  list(h = matrix(0, n, p), scenario_param = list())
}

mean_square_wave <- function(n, p, amp, L = n / 2, ...) {
  list(h = matrix(rep(amp * square_wave(n, L), p), n, p), scenario_param = list(L = L))
}

mean_sin_wave <- function(n, p, amp, period = 365, ...) {
  list(h = matrix(rep(amp * sin_wave(n, period), p), n, p), scenario_param = list(period = period))
}

scenario <- function(scenario_num = 1, n = 1000, p = 2, r = 0, amp = 1,
                     cov_type = "compound", err_type = "normal", seed = 1, ...) {
  S <- make_cov(r, p, cov_type)

  ms <- switch(as.character(scenario_num),
    "1" = mean_flat(n, p, amp, ...),
    "2" = mean_square_wave(n, p, amp, ...),
    "3" = mean_sin_wave(n, p, amp, ...),
    stop("Unknown scenario: ", scenario_num)
  )

  params <- list(
    scenario = scenario_num, h = ms$h,
    n = n, p = p, r = r, cov_type = cov_type,
    S = S, S_cholesky = chol(S),
    amp = amp, seed = seed, err_type = err_type,
    scenario_param = ms$scenario_param
  )
  params$kappa <- kappa_from_params(params)
  params
}
