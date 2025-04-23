#' mod_idx
#'
#' produces equivariant index with circular embedding
#'
#' @param i original index
#' @return modularized index
#' @examples
#' mod_idx(10, 10) # 10
#' mod_idx(11, 10) # 1
#' @export
mod_idx <- function(i, n) {
  (i - 1) %% n + 1
}

#' rotate
#'
#' rotates elements to the right by k
#'
#' @param X data sequence
#' @return rotated data sequence
#' @examples
#' X <- 1:3
#' rotate(X, k = 1) # 3 1 2
#'
#' X <- matrix(c(1:3, 1:3), ncol = 2)
#' rotate(X, k = 1)
#' @export
rotate <- function(X, k = 1) {
  if (is.matrix(X)) {
    n <- nrow(X)
    return(X[c((k + 1):n, 1:k), ])
  } else {
    n <- length(X)
    return(X[c((k + 1):n, 1:k)])
  }
}

#' lag_diff
#'
#' A brief description of what the function does.
#'
#' @param param1 Description of the first parameter.
#' @return Description of what the function returns.
#' @details (Optional) More detailed explanation of the function.
#' @examples
#' X <- 1:3
#' lag_diff(X) # 6
#' @export
lag_diff <- function(X, Y = NULL, k = 1) {
  # T_k
  n <- length(X)

  if (is.null(Y)) {
    sum((X - rotate(X, k))^2)
  } else {
    sum(
      (X - rotate(X, k)) *
        (Y - rotate(Y, k))
    )
  }
}

#' get_A
#'
#' Calculate circulant matrix
#'
#' @param n length of data
#' @param L length of shortest data segment
#' @return Description of what the function returns.
#' @details (Optional) More detailed explanation of the function.
#' @examples
#' @export
get_A <- function(n, L = 2) {
  # perform fast calculation

  # eigenvectors
  # eigenvalues
  w <- exp(2 * pi * 1i / n)
}

#' ece
#'
#' Outputs equivariant covariance estimator
#'
#' @param param1 Description of the first parameter.
#' @return Description of what the function returns.
#' @details (Optional) More detailed explanation of the function.
#' @examples
#' @export
equiv.cov <- function(X, Y = NULL, L = 2) {
  if (is.null(Y)) {
    n <- nrow(X)
  } else {
    n <- length(X)
  }
  K <- matrix(c(rep(1, L), 1:L), ncol = 2)
  R <- sapply(1:L, function(k) lag_diff(X, Y, k) / (2 * n))
  B <- solve(t(K) %*% K) %*% t(K) %*% R
  return(
    list(
      cov = B[1],
      norm = B[2]
    )
  )
}

# equiv.cov <- function(X, Y = NULL, L = 2) {
#  if (is.matrix(X)) {
#    stopifnot(ncol(X) == 2)
#    return(equiv.cov.matrix(X, L = L))
#  } else if (is.vector(X) && is.vector(Y)) {
#    return(equiv.cov.vector(X, Y, L = L))
#  } else {
#    stop("Invalid input: Provide either a matrix with 2 columns or two numeric vectors.")
#  }
# }
#
# equiv.cov.vector <- function() {
#
# }
#
# equiv.cov.matrix <- function() {
#
# }

equiv.corr <- function() {

}

ece <- function(X, L = 2, normalize = FALSE) {
  p <- ncol(X)
  cov_mat <- matrix(0, nrow = p, ncol = p)
  for (i in 1:p) {
    for (j in i:p) {
      cov_ij <- equiv.cov(X[, c(i, j)], L = L)
      cov_mat[i, j] <- cov_ij$cov
      cov_mat[j, i] <- cov_ij$cov # symmetry
    }
  }
  if (normalize) {
    return(cov2cor(cov_mat))
  }
  return(cov_mat)
}

ece.conf.int <- function() {

}
