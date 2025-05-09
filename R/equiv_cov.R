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
      list(cov = as.numeric(B[1]), norm = as.numeric(B[2]))
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
