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

#' Equivariant Correlation Test
#'
#' Tests for correlation among time series in the presence of unknown mean
#' shifts. Supports an asymptotic z-test for bivariate series and a bootstrap
#' multiplier test for multivariate series.
#'
#' @param X A numeric matrix with series in columns (\eqn{n \times 2} for
#'   \code{"z.test"}, \eqn{n \times p} for \code{"bs.multiplier"}).
#' @param type A character string specifying the test type:
#'   \describe{
#'     \item{\code{"z.test"}}{Asymptotic z-test based on the equivariant
#'       correlation estimator. Requires a 2-column matrix.}
#'     \item{\code{"bs.multiplier"}}{Bootstrap multiplier test using pairwise
#'       lag terms and Cauchy combination of split p-values.}
#'   }
#' @param B Number of bootstrap replicates for \code{"bs.multiplier"} (default 1000).
#'
#' @return A list with element \code{p.value}. For \code{"z.test"}, also
#'   includes \code{estimate} (the estimated correlation).
#'
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' ece.test(X)
#' ece.test(X, type = "bs.multiplier")
#'
#' @export
ece.test <- function(X, type = "z.test", B = 1000) {
  X <- as.matrix(X)
  if (type == "z.test") {
    if (ncol(X) != 2) stop("type = 'z.test' requires a 2-column matrix")
    cov_mat <- ece.cov(X)
    sx <- sqrt(cov_mat[1, 1])
    sy <- sqrt(cov_mat[2, 2])
    sxy <- cov_mat[1, 2]
    rxy <- sxy / (sx * sy)
    se <- ece.se(X)
    list(
      estimate = rxy,
      p.value  = 2 * (1 - pnorm(abs(rxy / se)))
    )
  } else if (type == "bs.multiplier") {
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
}
