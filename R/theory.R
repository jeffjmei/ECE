var_Tk <- function(n, sx, hx, k40, k = 1) {
  4 * n * k40 * sx^4 + 8 * k * sx^2 * lag_diff(hx)
}
cov_Th_Tk <- function(n, sx, hx, k40, k = 1) {
  4 * n * (k40 - 1) * sx^4 + 8 * k * sx^2 * lag_diff(hx)
}
cov_Tk_Rk <- function(n, sx, sy, sxy, hx, hy, k22, k = 1) {
  4 * n * sx^2 * sy^2 * (k22 - 1) +
    4 * n * sxy^2 + 8 * sxy * k * lag_diff(hx, hy)
}
cov_Th_Rk <- function(n, sx, sy, sxy, hx, hy, k22, k = 1) {
  4 * n * sx^2 * sy^2 * (k22 - 1) +
    8 * sxy * lag_diff(hx, hy)
}
cov_Tk_Qk <- function(n, sx, sy, sxy, hx, hy, k31, k = 1) {
  4 * sxy * k * lag_diff(hx) +
    4 * sx^2 * k * lag_diff(hx, hy) +
    4 * n * sx^3 * sy * k31
}
cov_Th_Qk <- function(n, sx, sy, sxy, hx, hy, k31, k = 1) {
  4 * n * sx^3 * sy * k31 -
    4 * n * sx^2 * sxy +
    4 * sxy * lag_diff(hx) +
    4 * sx^2 * lag_diff(hx, hy)
}
cov_Qh_Qk <- function(n, sx, sy, sxy, hx, hy, k22, k = 1) {
  4 * n * (sx^2 * sy^2 * k22 - sxy^2) +
    2 * sy^2 * lag_diff(hx) +
    2 * sx^2 * lag_diff(hy) +
    4 * sxy * lag_diff(hx, hy)
}
var_Qh <- function(n, sx, sy, sxy, hx, hy, k22, k = 1) {
  2 * n * sx^2 * sy^2 * (k22 + 1) +
    2 * n * (sx^2 * sy^2 * k22 - sxy^2) +
    2 * k * sy^2 * lag_diff(hx, hx) +
    2 * k * sx^2 * lag_diff(hy, hy) +
    4 * k * sxy * lag_diff(hx, hy)
}

dg <- function(u1, u2, u3, u4, u5, u6) {
  # Theoretical Variance
  dT1 <- -(2 * u3 - u4) * (2 * u5 - u6) /
    ((2 * u1 - u2) * (2 * u3 - u4))^(3 / 2)
  dT2 <- (1 / 2) * (2 * u3 - u4) * (2 * u5 - u6) /
    ((2 * u1 - u2) * (2 * u3 - u4))^(3 / 2)
  dR1 <- -(2 * u1 - u2) * (2 * u5 - u6) /
    ((2 * u1 - u2) * (2 * u3 - u4))^(3 / 2)
  dR2 <- (1 / 2) * (2 * u1 - u2) * (2 * u5 - u6) /
    ((2 * u1 - u2) * (2 * u3 - u4))^(3 / 2)
  dQ1 <- 2 / ((2 * u1 - u2) * (2 * u3 - u4))^(1 / 2)
  dQ2 <- -1 / ((2 * u1 - u2) * (2 * u3 - u4))^(1 / 2)
  return(matrix(c(dT1, dT2, dR1, dR2, dQ1, dQ2), ncol = 1))
}


ece.cor.asymp <- function(params) {
  n <- params$n
  sx <- sqrt(params$S[1, 1])
  sy <- sqrt(params$S[2, 2])
  sxy <- params$S[1, 2]
  rxy <- sxy / (sx * sy)
  hx <- params$h[, 1]
  hy <- params$h[, 2]

  # TODO: generalize to non-gaussian
  k40 <- 3
  k04 <- 3
  k31 <- 3 * rxy
  k13 <- 3 * rxy
  k22 <- (1 + 2 * rxy^2)

  # Populate Matrix
  T1_T1 <- var_Tk(n, sx, hx, k40, k = 1)
  T1_T2 <- cov_Th_Tk(n, sx, hx, k40)
  T1_R1 <- cov_Tk_Rk(n, sx, sy, sxy, hx, hy, k22, k = 1)
  T1_R2 <- cov_Th_Rk(n, sx, sy, sxy, hx, hy, k22)
  T1_Q1 <- cov_Tk_Qk(n, sx, sy, sxy, hx, hy, k31, k = 1)
  T1_Q2 <- cov_Th_Qk(n, sx, sy, sxy, hx, hy, k31)
  T2_T2 <- var_Tk(n, sx, hx, k40, k = 2)
  T2_R1 <- cov_Th_Rk(n, sx, sy, sxy, hx, hy, k22)
  T2_R2 <- cov_Tk_Rk(n, sx, sy, sxy, hx, hy, k22, k = 2)
  T2_Q1 <- cov_Th_Qk(n, sx, sy, sxy, hx, hy, k31)
  T2_Q2 <- cov_Tk_Qk(n, sx, sy, sxy, hx, hy, k31, k = 2)
  R1_R1 <- var_Tk(n, sy, hy, k04, k = 1)
  R1_R2 <- cov_Th_Tk(n, sy, hy, k04)
  R1_Q1 <- cov_Tk_Qk(n, sy, sx, sxy, hy, hx, k13, k = 1)
  R1_Q2 <- cov_Th_Qk(n, sy, sx, sxy, hy, hx, k13)
  R2_R2 <- var_Tk(n, sy, hy, k04, k = 2)
  R2_Q1 <- cov_Th_Qk(n, sy, sx, sxy, hy, hx, k13)
  R2_Q2 <- cov_Tk_Qk(n, sy, sx, sxy, hy, hx, k13, k = 2)
  Q1_Q1 <- var_Qh(n, sx, sy, sxy, hx, hy, k22, k = 1)
  Q1_Q2 <- cov_Qh_Qk(n, sx, sy, sxy, hx, hy, k22)
  Q2_Q2 <- var_Qh(n, sx, sy, sxy, hx, hy, k22, k = 2)
  S_u_upper <- c(
    T1_T1,
    T1_T2, T2_T2,
    T1_R1, T2_R1, R1_R1,
    T1_R2, T2_R2, R1_R2, R2_R2,
    T1_Q1, T2_Q1, R1_Q1, R2_Q1, Q1_Q1,
    T1_Q2, T2_Q2, R1_Q2, R2_Q2, Q1_Q2, Q2_Q2
  )
  S_u <- matrix(0, nrow = 6, ncol = 6)
  S_u[upper.tri(S_u, diag = TRUE)] <- S_u_upper
  S_u <- S_u + t(S_u) - diag(diag(S_u))

  u1 <- 2 * n * sx^2 + lag_diff(hx)
  u2 <- 2 * n * sx^2 + 2 * lag_diff(hx)
  u3 <- 2 * n * sy^2 + lag_diff(hy)
  u4 <- 2 * n * sy^2 + 2 * lag_diff(hy)
  u5 <- 2 * n * sxy + lag_diff(hx, hy)
  u6 <- 2 * n * sxy + 2 * lag_diff(hx, hy)
  rho_var <- (t(dg(u1, u2, u3, u4, u5, u6)) %*% S_u %*% dg(u1, u2, u3, u4, u5, u6))[1, 1]
  return(rho_var)
}
