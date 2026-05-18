scenario1 <- function(n = 1000, p = 2, r = 0, amp = 1, cov_type = "compound", seed = 1) {
  # no change point
  list(
    scenario       = 1,
    h              = matrix(0, n, p),
    n = n, p = p, r = r, cov_type = cov_type,
    S              = make_cov(r, p, cov_type),
    amp            = amp,
    seed           = seed,
    scenario_param = list()
  )
}

scenario2 <- function(n = 1000, p = 2, r = 0, amp = 1, cov_type = "compound", seed = 1, L = n / 2) {
  # square wave (default: one change point)
  h <- amp * square_wave(n, L)
  list(
    scenario       = 2,
    h              = matrix(rep(h, p), n, p),
    n = n, p = p, r = r, cov_type = cov_type,
    S              = make_cov(r, p, cov_type),
    amp            = amp,
    seed           = seed,
    scenario_param = list(L = L)
  )
}

scenario3 <- function(n = 1000, p = 2, r = 0, amp = 1, cov_type = "compound", seed = 1, period = 365) {
  # sin wave
  h <- amp * sin_wave(n, period)
  list(
    scenario       = 3,
    h              = matrix(rep(h, p), n, p),
    n = n, p = p, r = r, cov_type = cov_type,
    S              = make_cov(r, p, cov_type),
    amp            = amp,
    seed           = seed,
    scenario_param = list(period = period)
  )
}

scenario <- function(scenario_num = 1, n = 1000, p = 2, r = 0, amp = 1, cov_type = "compound", seed = 1, ...) {
  switch(as.character(scenario_num),
    "1" = scenario1(n, p, r, amp, cov_type, seed),
    "2" = scenario2(n, p, r, amp, cov_type, seed, ...),
    "3" = scenario3(n, p, r, amp, cov_type, seed, ...),
    stop("Unknown scenario: ", scenario_num)
  )
}
