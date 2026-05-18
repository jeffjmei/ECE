square_wave <- function(n, L = n / 2) {
  # WARN: last segment may be truncated if n is not a multiple of 2*L
  rep(rep(c(-1, 1), each = L), length.out = n)
}

sin_wave <- function(n, period = 365) {
  sin(2 * pi * (1:n) / period)
}

generate_data <- function(params) {
  e <- MASS::mvrnorm(params$n, rep(0, params$p), params$S)
  X <- params$h + e
  colnames(X) <- paste0("x", 1:params$p)
  return(X)
}
