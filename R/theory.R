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

var_corr <- function(n, sx, sy, sxy, hx, hy,
                     k13, k31, k22, k04, k40, w1, w2, w12) {
  rxy <- 0
  k22 <- 1 + 2 * rxy^2 # NOTE: assuming normality
  k40 <- 3 # NOTE: assuming normality
  k04 <- 3 # NOTE: assuming normality
  k31 <- 3 * rxy # NOTE: assuming normality
  k13 <- 3 * rxy # NOTE: assuming normality

  S11 <- var_Tk(n, sx, hx, k40, k = 1)
  S12 <- cov_Th_Tk(n, sx, hx, k40)
  S13 <- cov_Tk_Rk(n, sx, sy, sxy, hx, hy, k22, k = 1)
  S14 <- cov_Th_Rk(n, sx, sy, sxy, hx, hy, k22)
  S15 <- cov_Tk_Qk(n, sx, sy, sxy, hx, hy, k31, k = 1)
  S16 <- cov_Th_Qk(n, sx, sy, sxy, hx, hy, k31)
  S22 <- var_Tk(n, sx, hx, k40, k = 2)
  S23 <- cov_Th_Rk(n, sx, sy, sxy, hx, hy, k22)
  S24 <- cov_Tk_Rk(n, sx, sy, sxy, hx, hy, k22, k = 2)
  S25 <- cov_Th_Qk(n, sx, sy, sxy, hx, hy, k31)
  S26 <- cov_Tk_Qk(n, sx, sy, sxy, hx, hy, k31, k = 2)
  S33 <- var_Tk(n, sy, hy, k04, k = 1)
  S34 <- cov_Th_Tk(n, sy, hy, k04)
  S35 <- cov_Tk_Qk(n, sy, sx, sxy, hy, hx, k13, k = 1)
  S36 <- cov_Th_Qk(n, sy, sx, sxy, hy, hx, k13)
  S44 <- var_Tk(n, sy, hy, k04, k = 2)
  S45 <- cov_Th_Qk(n, sy, sx, sxy, hy, hx, k13)
  S46 <- cov_Tk_Qk(n, sy, sx, sxy, hy, hx, k13, k = 2)
  S55 <- var_Qh(n, sx, sy, sxy, hx, hy, k22, k = 1)
  S56 <- cov_Qh_Qk(n, sx, sy, sxy, hx, hy, k22)
  S66 <- var_Qh(n, sx, sy, sxy, hx, hy, k22, k = 2)

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
