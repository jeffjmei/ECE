#' Standard Error of the ECE Correlation Estimator
#'
#' Computes the asymptotic standard error of the equivariant correlation estimator
#' for a bivariate time series.
#'
#' @param X An \eqn{n \times 2} numeric matrix.
#' @param L A positive integer giving the minimum segment length (default 2).
#'
#' @return A scalar standard error.
#'
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' ece.se(X)
#'
#' @export
ece.se <- function(X, L = 2) {
  X <- as.matrix(X)
  n <- nrow(X)
  ece_xx <- ece_pair(X[, 1], X[, 1], L)
  ece_yy <- ece_pair(X[, 2], X[, 2], L)
  ece_xy <- ece_pair(X[, 1], X[, 2], L)
  sqrt(var_corr_est(
    n,
    sx  = sqrt(ece_xx$cov),
    sy  = sqrt(ece_yy$cov),
    sxy = ece_xy$cov,
    wx  = ece_xx$complexity,
    wy  = ece_yy$complexity,
    wxy = ece_xy$complexity
  ))
}

#' Equivariant Correlation Test
#'
#' Tests for correlation between two time series in the presence of unknown mean
#' shifts, using an asymptotic z-test based on the equivariant correlation estimator.
#'
#' @param X An \eqn{n \times 2} numeric matrix with the two series in columns.
#' @param type A character string specifying the test type. Currently only
#'   \code{"z.test"} is supported.
#' @param alpha A numeric significance level (default 0.05).
#'
#' @return A list with elements:
#'   \item{estimate}{The estimated correlation.}
#'   \item{p.value}{The two-sided p-value.}
#'
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' ece.test(X)
#'
#' @export
ece.test <- function(X, type = "z.test", alpha = 0.05) {
  X <- as.matrix(X)
  if (type == "z.test") {
    if (ncol(X) != 2) stop("type = 'z.test' requires a 2-column matrix")
    cov_mat <- ece.cov(X)
    sx  <- sqrt(cov_mat[1, 1])
    sy  <- sqrt(cov_mat[2, 2])
    sxy <- cov_mat[1, 2]
    rxy <- sxy / (sx * sy)
    se  <- ece.se(X)
    list(
      estimate = rxy,
      p.value  = 2 * (1 - pnorm(abs(rxy / se)))
    )
  }
}
