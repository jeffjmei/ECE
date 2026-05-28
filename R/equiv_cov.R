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

ece.kappa <- function(x, y = NULL, rho = NULL) {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }

  n <- length(x)
  Sxx <- ece_terms(cbind(x, x))
  Syy <- ece_terms(cbind(y, y))
  Sxy <- ece_terms(cbind(x, y))

  # Get Covariance (Must Center)
  S <- cbind(Sxx, Syy, Sxy) |> scale(scale = FALSE) # center
  V <- (1 / n^2) * (
    t(S) %*% S +
      t(rotate(S)) %*% S + t(S) %*% rotate(S) +
      t(rotate(S, 2)) %*% S + t(S) %*% rotate(S, 2)
  )

  sx <- sqrt(mean(Sxx))
  sy <- sqrt(mean(Syy))
  sxy <- if (!is.null(rho) && rho == 0) 0 else mean(Sxy)

  wx <- sqrt(ece.complexity(x, x))
  wy <- sqrt(ece.complexity(y, y))
  wxy <- ece.complexity(x, y)

  # Calculate Kappa
  k40 <- n * V[1, 1] / sx^4 - 4 * wx^2 / sx^2 - 4
  k04 <- n * V[2, 2] / sy^4 - 4 * wy^2 / sy^2 - 4
  k22 <- n * V[3, 3] / (sx^2 * sy^2) -
    3 / 2 * sxy^2 / (sx^2 * sy^2) -
    2 * sxy * wxy / (sx^2 * sy^2) -
    wx^2 / sx^2 -
    wy^2 / sy^2 - 5 / 2
  k31 <- n * V[1, 3] / (sx^3 * sy) -
    4 * sxy / (sx * sy) -
    2 * sxy * wx^2 / (sx^3 * sy) -
    2 * wxy / (sx * sy)
  k13 <- n * V[2, 3] / (sy^3 * sx) -
    4 * sxy / (sx * sy) -
    2 * sxy * wy^2 / (sy^3 * sx) -
    2 * wxy / (sx * sy)

  list(
    k40 = k40,
    k31 = k31,
    k22 = k22,
    k13 = k13,
    k04 = k04
  )
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
