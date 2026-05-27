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
