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

ece.pval <- function(x, y = NULL, type = "z.test", ...) {
  if (is.matrix(x)) {
    p <- ncol(x)
    pval_mat <- matrix(0, p, p)
    for (i in seq_len(p)) {
      for (j in i:p) {
        pval_mat[i, j] <- pval_mat[j, i] <- ece.test(cbind(x[, i], x[, j]), type = type, ...)$p.value
      }
    }
    return(pval_mat)
  }
  if (is.null(y)) y <- x
  ece.test(cbind(x, y), type = type, ...)$p.value
}

ece.kappa <- function(x, y = NULL, rho = NULL, method = "mom") {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }
  switch(method,
    mom      = ece.kappa.mom(x, y, rho),
    matrix   = ece.kappa.matrix(x, y, rho),
    regress  = ece.kappa.regress(x, y, rho),
    stop("Unknown method: ", method)
  )
}

ece.kappa.mom <- function(x, y = NULL, rho = NULL) {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }
  if (is.null(y)) y <- x

  Sxx <- ece_terms(cbind(x, x))
  Syy <- ece_terms(cbind(y, y))
  Sxy <- ece_terms(cbind(x, y))

  sx2 <- mean(Sxx)
  sy2 <- mean(Syy)
  sxy <- if (!is.null(rho) && rho == 0) 0 else mean(Sxy)

  wxx <- ece.complexity(x, x)
  wyy <- ece.complexity(y, y)
  wxy <- ece.complexity(x, y)

  k40 <- (mean(Sxx^2) - 3 * sx2^2 - 4 * sx2 * wxx) / sx2^2
  k04 <- (mean(Syy^2) - 3 * sy2^2 - 4 * sy2 * wyy) / sy2^2
  k22 <- (mean(Sxx * Syy) - 3 * sxy^2 - 4 * sxy * wxy) / (sx2 * sy2)
  k31 <- (mean(Sxx * Sxy) - 3 * sx2 * sxy - 2 * sxy * wxx - 2 * sx2 * wxy) /
    (sx2^(3 / 2) * sy2^(1 / 2))
  k13 <- (mean(Syy * Sxy) - 3 * sy2 * sxy - 2 * sxy * wyy - 2 * sy2 * wxy) /
    (sy2^(3 / 2) * sx2^(1 / 2))

  list(k40 = k40, k31 = k31, k22 = k22, k13 = k13, k04 = k04)
}

ece.kappa.matrix <- function(x, y = NULL, rho = NULL) {
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

  sx2 <- mean(Sxx)
  sy2 <- mean(Syy)
  sxy <- if (!is.null(rho) && rho == 0) 0 else mean(Sxy)

  wx2 <- ece.complexity(x, x)
  wy2 <- ece.complexity(y, y)
  wxy <- ece.complexity(x, y)

  # Calculate Kappa
  k40 <- n * V[1, 1] / sx2^2 - 4 * wx2 / sx2 - 4
  k04 <- n * V[2, 2] / sy2^2 - 4 * wy2 / sy2 - 4
  k22 <- n * V[3, 3] / (sx2 * sy2) -
    3 / 2 * sxy^2 / (sx2 * sy2) -
    2 * sxy * wxy / (sx2 * sy2) -
    wx2 / sx2 -
    wy2 / sy2 - 5 / 2
  k31 <- n * V[1, 3] / (sx2^(3 / 2) * sqrt(sy2)) -
    4 * sxy / (sqrt(sx2) * sqrt(sy2)) -
    2 * sxy * wx2 / (sx2^(3 / 2) * sqrt(sy2)) -
    2 * wxy / (sqrt(sx2) * sqrt(sy2))
  k13 <- n * V[2, 3] / (sy2^(3 / 2) * sqrt(sx2)) -
    4 * sxy / (sqrt(sx2) * sqrt(sy2)) -
    2 * sxy * wy2 / (sy2^(3 / 2) * sqrt(sx2)) -
    2 * wxy / (sqrt(sx2) * sqrt(sy2))

  list(k40 = k40, k31 = k31, k22 = k22, k13 = k13, k04 = k04)
}

ece.kappa.regress <- function(x, y = NULL, rho = NULL) {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }

  n <- length(x)
  sx2 <- ece.cov(x, x)
  sy2 <- ece.cov(y, y)
  sxy <- if (!is.null(rho) && rho == 0) 0 else ece.cov(x, y)

  k22 <- (2 * lag_diff2(x, y, k = 1) - lag_diff2(x, y, k = 2)) /
    (2 * n * sx2 * sy2) - 2 * sxy^2 / (sx2 * sy2) - 1

  list(k40 = NULL, k31 = NULL, k22 = k22, k13 = NULL, k04 = NULL)
}
