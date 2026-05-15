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

#' Equivariant Covariance Estimator
#'
#' Computes an equivariant estimate of covariance and norm structure based on lagged differences.
#'
#' @param X A numeric vector or matrix. If a matrix, must have observations in rows and variables in columns.
#' @param Y Optional numeric vector of the same length as `X`. Ignored if `X` is a matrix.
#' @param L Integer. The maximum lag to use in the regression (default = 2).
#' @param return.norm Logical. If TRUE, also returns norm (slope) component.
#'
#' @return If X is a vector and `return.norm = FALSE`: a scalar covariance estimate.
#'         If X is a vector and `return.norm = TRUE`: a list with elements `cov` and `norm`.
#'         If X is a matrix: a covariance matrix, or a list of matrices if `return.norm = TRUE`.
#'
#' @examples
#' # Univariate
#' x <- rnorm(100)
#' equiv.cov(x)
#'
#' # Cross-covariance
#' y <- rnorm(100)
#' equiv.cov(x, y)
#'
#' # Multivariate
#' X <- matrix(rnorm(300), ncol = 3)
#' equiv.cov(X)
#'
#' @export
equiv.cov <- function(X, Y = NULL, L = 2, return.norm = FALSE) {
  # Helper for pairwise computation
  compute_equiv <- function(x1, x2, L, return.norm) {
    n <- length(x1)
    K <- matrix(c(rep(1, L), 1:L), ncol = 2)
    R <- sapply(1:L, function(k) lag_diff(x1, x2, k) / (2 * n))
    B <- solve(t(K) %*% K) %*% t(K) %*% R
    if (return.norm) {
      list(cov = as.numeric(B[1]), norm = as.numeric(2 * B[2]))
    } else {
      as.numeric(B[1])
    }
  }

  if (is.matrix(X)) {
    p <- ncol(X)
    if (return.norm) {
      cov_mat <- matrix(0, p, p)
      norm_mat <- matrix(0, p, p)
      for (i in 1:p) {
        for (j in i:p) {
          result <- compute_equiv(X[, i], X[, j], L, TRUE)
          cov_mat[i, j] <- cov_mat[j, i] <- result$cov
          norm_mat[i, j] <- norm_mat[j, i] <- result$norm
        }
      }
      return(list(cov = cov_mat, norm = norm_mat))
    } else {
      cov_mat <- matrix(0, p, p)
      for (i in 1:p) {
        for (j in i:p) {
          cov_ij <- compute_equiv(X[, i], X[, j], L, FALSE)
          cov_mat[i, j] <- cov_mat[j, i] <- cov_ij
        }
      }
      return(cov_mat)
    }
  } else {
    # Vector input
    if (is.null(Y)) {
      Y <- X
    } else {
      if (length(X) != length(Y)) {
        stop("X and Y must be the same length.")
      }
    }
    return(compute_equiv(X, Y, L, return.norm))
  }
}

equiv.var <- function(X, L = 2) {
  equiv.cov(X, X, L = L)
}
