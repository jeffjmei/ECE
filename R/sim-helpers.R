square_wave <- function(n, L = n / 2) {
  # WARN: last segment may be truncated if n is not a multiple of 2*L
  rep(rep(c(-1, 1), each = L), length.out = n)
}

sin_wave <- function(n, period = 365) {
  sin(2 * pi * (1:n) / period)
}

generate_err <- function(params) {
  n <- params$n
  p <- nrow(params$S)
  Z <- switch(params$err_type,
    normal = matrix(rnorm(n * p), n, p),
    t = matrix(rt(n * p, df = 5) / sqrt(5 / 3), n, p),
    lognormal = {
      mu_ln <- exp(0.5)
      sd_ln <- sqrt((exp(1) - 1) * exp(1))
      matrix((rlnorm(n * p, 0, 1) - mu_ln) / sd_ln, n, p)
    },
    exponential = matrix(rexp(n * p, 1) - 1, n, p),
    cauchy      = matrix(rcauchy(n * p), n, p),
    stop("unknown err_type: ", params$err_type)
  )

  Z %*% params$S_cholesky
}

generate_data <- function(params) {
  e <- generate_err(params)
  X <- params$h + e
  colnames(X) <- paste0("x", 1:ncol(e))
  return(X)
}
