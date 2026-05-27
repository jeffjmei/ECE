ece_pair <- function(x, y, L = 2) {
  n <- length(x)
  K <- matrix(c(rep(1, L), 1:L), ncol = 2)
  R <- sapply(1:L, function(k) lag_diff(x, y, k) / (2 * n))
  B <- solve(t(K) %*% K) %*% t(K) %*% R
  list(cov = as.numeric(B[1]), complexity = as.numeric(2 * B[2]))
}

#' Equivariant Covariance
#'
#' Computes an equivariant estimate of covariance based on lagged differences,
#' robust to unknown mean shifts. Mirrors the interface of \code{\link[stats]{cov}}.
#'
#' @param x A numeric vector or matrix with observations in rows and variables in columns.
#' @param y A numeric vector of the same length as \code{x}. If \code{NULL} and \code{x}
#'   is a vector, computes the variance of \code{x}.
#' @param L A positive integer giving the minimum segment length (default 2).
#'
#' @return A scalar if \code{x} and \code{y} are vectors; a \eqn{p \times p} matrix
#'   if \code{x} is a matrix.
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' ece.cov(x, y)
#'
#' X <- matrix(rnorm(300), ncol = 3)
#' ece.cov(X)
#'
#' @export
ece.cov <- function(x, y = NULL, L = 2) {
  if (is.matrix(x)) {
    p <- ncol(x)
    cov_mat <- matrix(0, p, p)
    for (i in seq_len(p)) {
      for (j in i:p) {
        cov_mat[i, j] <- cov_mat[j, i] <- ece_pair(x[, i], x[, j], L)$cov
      }
    }
    return(cov_mat)
  }
  if (is.null(y)) y <- x
  ece_pair(x, y, L)$cov
}

#' Equivariant Correlation
#'
#' Computes an equivariant estimate of correlation based on lagged differences,
#' robust to unknown mean shifts. Mirrors the interface of \code{\link[stats]{cor}}.
#'
#' @param x A numeric vector or matrix with observations in rows and variables in columns.
#' @param y A numeric vector of the same length as \code{x}. If \code{NULL} and \code{x}
#'   is a vector, computes the autocorrelation of \code{x}.
#' @param L A positive integer giving the minimum segment length (default 2).
#'
#' @return A scalar if \code{x} and \code{y} are vectors; a \eqn{p \times p} matrix
#'   if \code{x} is a matrix.
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' ece.cor(x, y)
#'
#' X <- matrix(rnorm(300), ncol = 3)
#' ece.cor(X)
#'
#' @export
ece.cor <- function(x, y = NULL, L = 2) {
  if (is.matrix(x)) {
    cov2cor(ece.cov(x, L = L))
  } else {
    if (is.null(y)) y <- x
    sxy <- ece_pair(x, y, L)$cov
    sxx <- ece_pair(x, x, L)$cov
    syy <- ece_pair(y, y, L)$cov
    sxy / sqrt(sxx * syy)
  }
}

#' Equivariant Complexity
#'
#' Estimates the mean heterogeneity (complexity) of a time series based on lagged differences.
#' Large values indicate large or frequent mean shifts. Mirrors the interface of
#' \code{\link{ece.cov}}.
#'
#' @param x A numeric vector or matrix with observations in rows and variables in columns.
#' @param y A numeric vector of the same length as \code{x}. If \code{NULL} and \code{x}
#'   is a vector, computes the complexity of \code{x} against itself.
#' @param L A positive integer giving the minimum segment length (default 2).
#'
#' @return A scalar if \code{x} and \code{y} are vectors; a \eqn{p \times p} matrix
#'   if \code{x} is a matrix.
#'
#' @examples
#' x <- c(rep(0, 50), rep(5, 50))
#' ece.complexity(x)
#'
#' X <- matrix(rnorm(300), ncol = 3)
#' ece.complexity(X)
#'
#' @export
#' Equivariant Kurtosis Estimator (kappa_22)
#'
#' Estimates the cross-kurtosis parameter \eqn{\kappa_{22}} from data by
#' inverting the variance formula for the ECE covariance estimator.
#'
#' @param X An \eqn{n \times 2} numeric matrix.
#' @param rho If \code{0}, the cross-covariance \eqn{\sigma_{xy}} is set to
#'   zero (null assumption). If \code{NULL} (default), \eqn{\sigma_{xy}} is
#'   estimated from data.
#'
#' @return A scalar estimate of \eqn{\kappa_{22}}.
#'
#' @seealso [ece.complexity()], [ece.cov()]
#'
#' @export
ece.k22 <- function(X, rho = NULL) {
  if (!is.matrix(X) || ncol(X) != 2) stop("X must be an n x 2 matrix")
  n <- nrow(X)
  S <- ece_terms(X)
  W <- (1 / n^2) * (
    t(S) %*% S +
      t(rotate(S)) %*% S + t(S) %*% rotate(S) +
      t(rotate(S, 2)) %*% S + t(S) %*% rotate(S, 2)
  )
  sx2 <- ece.cov(X[, 1])
  sy2 <- ece.cov(X[, 2])
  sxy <- if (!is.null(rho) && rho == 0) 0 else ece.cov(X[, 1], X[, 2])

  wx2 <- ece.complexity(X[, 1])
  wy2 <- ece.complexity(X[, 2])
  wxy <- ece.complexity(X[, 1], X[, 2])

  n * W[1, 1] / (sx2 * sy2) -
    (3 / 2) * sxy^2 / (sx2 * sy2) -
    2 * sxy * wxy / (sx2 * sy2) -
    wx2 / sx2 - wy2 / sy2 -
    5 / 2
}

ece.complexity <- function(x, y = NULL, L = 2) {
  if (is.matrix(x)) {
    p <- ncol(x)
    complexity_mat <- matrix(0, p, p)
    for (i in seq_len(p)) {
      for (j in i:p) {
        complexity_mat[i, j] <- complexity_mat[j, i] <- ece_pair(x[, i], x[, j], L)$complexity
      }
    }
    return(complexity_mat)
  }
  if (is.null(y)) y <- x
  ece_pair(x, y, L)$complexity
}
