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

# Simulates the null distribution of max|colMeans(Z * s)| via Gaussian multiplier
# bootstrap, where Z is an iid N(0,1) vector independent of s.
bs_multiplier <- function(s, B = 1000) {
  n <- nrow(s)
  map_dbl(1:B, ~ max(abs(colMeans(rnorm(n) * s))))
}

# Combines p-values via the Cauchy combination test (Liu & Xie, 2020).
# Robust to dependence among p-values.
cauchy_combine <- function(pvals) {
  T_stat <- mean(tan(pi * (1 / 2 - pvals)))
  pcauchy(T_stat, lower.tail = FALSE)
}

# Asymptotic z-test for zero correlation based on the equivariant correlation estimator.
z_test <- function(X) {
  if (ncol(X) != 2) stop("type = 'z.test' requires a 2-column matrix")
  rxy <- ece.cor(X)[1, 2]
  se <- ece.se(X)
  list(
    estimate = rxy,
    p.value  = 2 * (1 - pnorm(abs(rxy / se)))
  )
}

# Multiplier bootstrap test via independent subsequence splitting and Cauchy combination.
bs_multiplier_test <- function(X, B = 1000) {
  # Make Terms Mean Zero and Independent
  splits <- lag_terms(X) |> split_indep()

  # Apply Bootstrap Multiplier
  bs_pval <- splits |>
    map_dbl(function(s) {
      stat_s <- max(abs(colMeans(s)))
      mean(bs_multiplier(s, B) > stat_s)
    })

  # Combine p-Values
  list(
    p.value = cauchy_combine(bs_pval)
  )
}

# Parametric bootstrap test using a bandwidth-2 HAC variance estimate of the lag terms.
bs_parametric_test <- function(X, B = 1000) {
  # Calculate Variance of ECE Terms
  n <- nrow(X)
  S <- lag_terms(X)
  W <- (1 / n^2) * (
    t(S) %*% S +
      t(rotate(S)) %*% S + t(S) %*% rotate(S) +
      t(rotate(S, 2)) %*% S + t(S) %*% rotate(S, 2)
  )

  # Boostrap Test
  S_bs <- MASS::mvrnorm(n = B, mu = rep(0, ncol(W)), Sigma = W)
  Q_bs <- abs(S_bs) |> apply(1, max)
  Q_obs <- max(abs(colMeans(S)))

  # Return Object
  list(
    p.value = mean(Q_bs > Q_obs)
  )
}

#' Equivariant Correlation Test
#'
#' Tests for correlation among time series in the presence of unknown mean
#' shifts. Supports an asymptotic z-test for bivariate series and two bootstrap
#' tests for multivariate series.
#'
#' @param X A numeric matrix with series in columns (\eqn{n \times 2} for
#'   \code{"z.test"}, \eqn{n \times p} for bootstrap methods).
#' @param type A character string specifying the test type:
#'   \describe{
#'     \item{\code{"z.test"}}{Asymptotic z-test based on the equivariant
#'       correlation estimator. Requires a 2-column matrix.}
#'     \item{\code{"bs.multiplier"}}{Multiplier bootstrap test using pairwise
#'       lag terms and Cauchy combination of split p-values.}
#'     \item{\code{"bs.parametric"}}{Parametric bootstrap test using a
#'       bandwidth-2 variance estimate of the lag terms to approximate the
#'       null distribution.}
#'   }
#' @param B Number of bootstrap replicates for bootstrap methods (default 1000).
#'
#' @return A list with element \code{p.value}. For \code{"z.test"}, also
#'   includes \code{estimate} (the estimated correlation).
#'
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' ece.test(X)
#' ece.test(X, type = "bs.multiplier")
#' ece.test(X, type = "bs.parametric")
#'
#' @export
ece.test <- function(X, type = "z.test", B = 1000) {
  X <- as.matrix(X)
  if (type == "z.test") {
    z_test(X)
  } else if (type == "bs.multiplier") {
    bs_multiplier_test(X, B)
  } else if (type == "bs.parametric") {
    bs_parametric_test(X, B)
  }
}
