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
