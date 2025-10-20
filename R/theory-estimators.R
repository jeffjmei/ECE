var_Tk_est <- function(n, sx, wx, k40, k = 1) {
  4 * n * k40 * sx^4 + 8 * n * k * sx^2 * wx
}
cov_Th_Tk_est <- function(n, sx, wx, k40, k = 1) {
  4 * n * (k40 - 1) * sx^4 + 8 * n * k * sx^2 * wx
}
cov_Tk_Rk_est <- function(n, sx, sy, sxy, wxy, k22, k = 1) {
  4 * n * sx^2 * sy^2 * (k22 - 1) +
    4 * n * sxy^2 +
    8 * sxy * n * k * wxy
}
cov_Th_Rk_est <- function(n, sx, sy, sxy, wxy, k22, k = 1) {
  4 * n * sx^2 * sy^2 * (k22 - 1) +
    8 * n * sxy * wxy
}
cov_Tk_Qk_est <- function(n, sx, sy, sxy, wx, wxy, k31, k = 1) {
  4 * n * sxy * k * wx +
    4 * n * sx^2 * k * wxy +
    4 * n * sx^3 * sy * k31
}
cov_Th_Qk_est <- function(n, sx, sy, sxy, wx, wxy, k31, k = 1) {
  4 * n * sx^3 * sy * k31 -
    4 * n * sx^2 * sxy +
    4 * n * sxy * wx +
    4 * n * sx^2 * wxy
}
cov_Qh_Qk_est <- function(n, sx, sy, sxy, wx, wy, wxy, k22, k = 1) {
  4 * n * (sx^2 * sy^2 * k22 - sxy^2) +
    2 * n * sy^2 * wx +
    2 * n * sx^2 * wy +
    4 * n * sxy * wxy
}
var_Qh_est <- function(n, sx, sy, sxy, wx, wy, wxy, k22, k = 1) {
  2 * n * sx^2 * sy^2 * (k22 + 1) +
    2 * n * (sx^2 * sy^2 * k22 - sxy^2) +
    2 * n * k * sy^2 * wx +
    2 * n * k * sx^2 * wy +
    4 * n * k * sxy * wxy
}

var_corr_est <- function(n, ece_obj) {
  sx <- sqrt(ece_obj$cov[1, 1])
  sy <- sqrt(ece_obj$cov[2, 2])
  sxy <- ece_obj$cov[1, 2]
  rxy <- sxy / (sx * sy)

  wx <- ece_obj$norm[1, 1]
  wy <- ece_obj$norm[2, 2]
  wxy <- ece_obj$norm[1, 2]

  k22 <- 1 + 2 * rxy^2 # NOTE: assuming normality
  k40 <- 3 # NOTE: assuming normality
  k04 <- 3 # NOTE: assuming normality
  k31 <- 3 * rxy # NOTE: assuming normality
  k13 <- 3 * rxy # NOTE: assuming normality

  S11 <- var_Tk_est(n, sx, wx, k40, k = 1)
  S12 <- cov_Th_Tk_est(n, sx, wx, k40)
  S13 <- cov_Tk_Rk_est(n, sx, sy, sxy, wxy, k22, k = 1)
  S14 <- cov_Th_Rk_est(n, sx, sy, sxy, wxy, k22)
  S15 <- cov_Tk_Qk_est(n, sx, sy, sxy, wx, wxy, k31, k = 1)
  S16 <- cov_Th_Qk_est(n, sx, sy, sxy, wx, wxy, k31)
  S22 <- var_Tk_est(n, sx, wx, k40, k = 2)
  S23 <- cov_Th_Rk_est(n, sx, sy, sxy, wxy, k22)
  S24 <- cov_Tk_Rk_est(n, sx, sy, sxy, wxy, k22, k = 2)
  S25 <- cov_Th_Qk_est(n, sx, sy, sxy, wx, wxy, k31)
  S26 <- cov_Tk_Qk_est(n, sx, sy, sxy, wx, wxy, k31, k = 2)
  S33 <- var_Tk_est(n, sy, wy, k04, k = 1)
  S34 <- cov_Th_Tk_est(n, sy, wy, k04)
  S35 <- cov_Tk_Qk_est(n, sy, sx, sxy, wy, wxy, k13, k = 1)
  S36 <- cov_Th_Qk_est(n, sy, sx, sxy, wy, wxy, k13)
  S44 <- var_Tk_est(n, sy, wy, k04, k = 2)
  S45 <- cov_Th_Qk_est(n, sy, sx, sxy, wy, wxy, k13)
  S46 <- cov_Tk_Qk_est(n, sy, sx, sxy, wy, wxy, k13, k = 2)
  S55 <- var_Qh_est(n, sx, sy, sxy, wx, wy, wxy, k22, k = 1)
  S56 <- cov_Qh_Qk_est(n, sx, sy, sxy, wx, wy, wxy, k22)
  S66 <- var_Qh_est(n, sx, sy, sxy, wx, wy, wxy, k22, k = 2)

  dg_mu <- c(
    -rxy * sx^(-2),
    0.5 * rxy * sx^(-2),
    -rxy * sy^(-2),
    0.5 * rxy * sy^(-2),
    2 * sx^(-1) * sy^(-1),
    -1 * sx^(-1) * sy^(-1)
  )

  S_u_upper <- (1 / (4 * n^2)) * c(
    S11,
    S12, S22,
    S13, S23, S33,
    S14, S24, S34, S44,
    S15, S25, S35, S45, S55,
    S16, S26, S36, S46, S56, S66
  )
  S_u <- matrix(0, nrow = 6, ncol = 6)
  S_u[upper.tri(S_u, diag = TRUE)] <- S_u_upper
  S_u <- S_u + t(S_u) - diag(diag(S_u))
  s_rho <- (t(dg_mu) %*% S_u %*% dg_mu)[1, 1]
  return(s_rho)
}
