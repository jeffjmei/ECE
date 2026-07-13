# Delta-method SE via the full 6x6 Sigma_Z matrix. Kept only for agreement
# tests against the closed-form ece.cor.se. Not exported; named to avoid
# collision with ece.cor.se.matrix(params) in theory.R.
# nolint start
ece_cor_se_ref <- function(x, y = NULL, L = 2, params = NULL) {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }
  n <- length(x)
  ece_xx <- ece_pair(x, x, L)
  ece_yy <- ece_pair(y, y, L)
  ece_xy <- ece_pair(x, y, L)
  sx <- sqrt(ece_xx$cov)
  sy <- sqrt(ece_yy$cov)
  sxy <- ece_xy$cov
  wx <- ece_xx$complexity
  wy <- ece_yy$complexity
  wxy <- ece_xy$complexity
  rxy <- sxy / (sx * sy)

  if (is.null(params)) {
    k40 <- 3
    k04 <- 3
    k31 <- 3 * rxy
    k13 <- 3 * rxy
    k22 <- 1 + 2 * rxy^2
  } else {
    k <- params$kappa
    k40 <- k$k40
    k04 <- k$k04
    k31 <- k$k31
    k13 <- k$k13
    k22 <- k$k22
  }

  S11 <- var_Tk_est(n, sx, wx, k40, k = 1)
  S12 <- cov_Th_Tk_est(n, sx, wx, k40)
  S13 <- cov_Tk_Rk_est(n, sx, sy, sxy, wxy, k22, k = 1)
  S14 <- cov_Th_Rk_est(n, sx, sy, sxy, wxy, k22)
  S15 <- cov_Tk_Qk_est(n, sx, sy, sxy, wx, wxy, k31, k = 1)
  S16 <- cov_Th_Qk_est(n, sx, sy, sxy, wx, wxy, k31)
  S22 <- var_Tk_est(n, sx, wx, k40, k = 2)
  S23 <- cov_Th_Rk_est(n, sx, sy, sxy, wxy, k22)
  S24 <- cov_Tk_Rk_est(n, sx, sy, sxy, wxy, k22, k = 2)
  S25 <- cov_Th_Qk_est(n, sx, sy, sxy, wx, wxy, k31)
  S26 <- cov_Tk_Qk_est(n, sx, sy, sxy, wx, wxy, k31, k = 2)
  S33 <- var_Tk_est(n, sy, wy, k04, k = 1)
  S34 <- cov_Th_Tk_est(n, sy, wy, k04)
  S35 <- cov_Tk_Qk_est(n, sy, sx, sxy, wy, wxy, k13, k = 1)
  S36 <- cov_Th_Qk_est(n, sy, sx, sxy, wy, wxy, k13)
  S44 <- var_Tk_est(n, sy, wy, k04, k = 2)
  S45 <- cov_Th_Qk_est(n, sy, sx, sxy, wy, wxy, k13)
  S46 <- cov_Tk_Qk_est(n, sy, sx, sxy, wy, wxy, k13, k = 2)
  S55 <- var_Qh_est(n, sx, sy, sxy, wx, wy, wxy, k22, k = 1)
  S56 <- cov_Qh_Qk_est(n, sx, sy, sxy, wx, wy, wxy, k22)
  S66 <- var_Qh_est(n, sx, sy, sxy, wx, wy, wxy, k22, k = 2)

  S_u_upper <- (1 / (4 * n^2)) * c(
    S11,
    S12, S22,
    S13, S23, S33,
    S14, S24, S34, S44,
    S15, S25, S35, S45, S55,
    S16, S26, S36, S46, S56, S66
  )
  S_u <- matrix(0, nrow = 6, ncol = 6)
  S_u[upper.tri(S_u, diag = TRUE)] <- S_u_upper
  S_u <- S_u + t(S_u) - diag(diag(S_u))

  dg_mu <- c(
    -rxy * sx^(-2),
    0.5 * rxy * sx^(-2),
    -rxy * sy^(-2),
    0.5 * rxy * sy^(-2),
    2 * sx^(-1) * sy^(-1),
    -1 * sx^(-1) * sy^(-1)
  )
  sqrt(as.numeric(t(dg_mu) %*% S_u %*% dg_mu))
}

#' Standard Error of the ECE Correlation Estimator
#'
#' Computes the asymptotic standard error of the equivariant correlation
#' estimator for a bivariate time series using the closed-form delta-method
#' expression.
#'
#' @param x A numeric vector, or an \eqn{n \times 2} matrix with series in columns.
#' @param y A numeric vector of the same length as \code{x}. Ignored if \code{x} is a matrix.
#' @param L A positive integer giving the minimum segment length (default 2).
#' @param kappa Either the string \code{"gaussian"} to use Gaussian cumulants, or
#'   a named list with any of \code{k22}, \code{k40}, \code{k04}, \code{k31},
#'   \code{k13}. Missing list entries fall back to Gaussian values.
#' @param rho If \code{0}, forces \eqn{\sigma_{xy} = 0} in the SE formula (null
#'   assumption). If \code{NULL} (default), \eqn{\sigma_{xy}} is estimated from data.
#'
#' @return A scalar standard error.
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' ece.cor.se(x, y, kappa = "gaussian")
#'
#' X <- matrix(rnorm(200), ncol = 2)
#' ece.cor.se(X, kappa = "gaussian")
#'
#' @export
ece.cor.se <- function(x, y = NULL, L = 2, kappa, rho = NULL) {
  if (is.matrix(x)) {
    if (ncol(x) != 2) stop("matrix input must have exactly 2 columns")
    y <- x[, 2]
    x <- x[, 1]
  }
  n <- length(x)
  sx <- sqrt(ece.cov(x, L = L))
  sy <- sqrt(ece.cov(y, L = L))
  wx2 <- ece.complexity(x, L = L)
  wy2 <- ece.complexity(y, L = L)
  wxy <- ece.complexity(x, y, L = L)
  sxy <- if (!is.null(rho) && rho == 0) 0 else ece.cov(x, y, L = L)
  rxy <- sxy / (sx * sy)

  if (is.character(kappa) && kappa == "gaussian") {
    k22 <- 1 + 2 * rxy^2
    k40 <- 3
    k04 <- 3
    k31 <- 3 * rxy
    k13 <- 3 * rxy
  } else {
    k22 <- if (!is.null(kappa$k22)) kappa$k22 else 1 + 2 * rxy^2
    k40 <- if (!is.null(kappa$k40)) kappa$k40 else 3
    k04 <- if (!is.null(kappa$k04)) kappa$k04 else 3
    k31 <- if (!is.null(kappa$k31)) kappa$k31 else 3 * rxy
    k13 <- if (!is.null(kappa$k13)) kappa$k13 else 3 * rxy
  }

  sqrt(
    (1 / n) * (
      (1 - rxy^2) * (wx2 / sx^2 + wy2 / sy^2 - 2 * rxy * wxy / (sx * sy)) +
        rxy^2 / 4 * (k40 + 2 * k22 + k04) -
        rxy * (k31 + k13) + k22 +
        5 / 2 * (1 - rxy^2)^2
    )
  )
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
z_test <- function(X, conf.level = 0.95, kappa = "gaussian", rho = NULL) {
  if (ncol(X) != 2) stop("type = 'z.test' requires a 2-column matrix")
  rxy <- ece.cor(X)[1, 2]
  se <- ece.cor.se(X[, 1], X[, 2], kappa = kappa, rho = rho)
  ci <- rxy + c(-1, 1) * qnorm((1 + conf.level) / 2) * se
  list(
    estimate = rxy,
    se       = se,
    conf.int = structure(ci, conf.level = conf.level),
    p.value  = 2 * pnorm(abs(rxy / se), lower.tail = FALSE)
  )
}

# Naive multiplier bootstrap test with no subsequence splitting (expected to be invalid).
bs_multiplier_nosplit_test <- function(X, B = 1000) {
  s <- ece_terms(X)
  stat <- max(abs(colMeans(s)))
  list(p.value = mean(bs_multiplier(s, B) > stat))
}

# Multiplier bootstrap test via independent subsequence splitting and Cauchy combination.
bs_multiplier_test <- function(X, B = 1000) {
  # Make Terms Mean Zero and Independent
  splits <- ece_terms(X) |> split_indep()

  # Apply Bootstrap Multiplier
  bs_pval <- splits |>
    map_dbl(function(s) {
      stat_s <- max(abs(colMeans(s)))
      mean(bs_multiplier(s, B) > stat_s)
    })

  # Combine p-Values
  list(
    p.value = cauchy_combine(bs_pval),
    p.split = bs_pval
  )
}

# Parametric bootstrap test using a bandwidth-2 HAC variance estimate of the lag terms.
bs_parametric_test <- function(X, B = 1000, drop_lag2 = FALSE, mask_disjoint = FALSE, force_psd = FALSE) {
  # Calculate Variance of ECE Terms
  n <- nrow(X)
  p <- ncol(X)
  S <- ece_terms(X)
  W <- (1 / n^2) * (t(S) %*% S + t(rotate(S)) %*% S + t(S) %*% rotate(S))
  if (!drop_lag2) {
    W <- W + (1 / n^2) * (t(rotate(S, 2)) %*% S + t(S) %*% rotate(S, 2))
  }

  # Apply 0-Masking for Mutually Exclusive Pairs
  O <- pair_overlap(p)
  stopifnot(identical(colnames(W), colnames(O))) # check
  if (mask_disjoint) W[O == 0] <- 0
  if (force_psd) W <- make_psd(W)

  # Boostrap Test
  S_bs <- MASS::mvrnorm(n = B, mu = rep(0, ncol(W)), Sigma = W)
  Q_bs <- abs(S_bs) |> apply(1, max)
  Q_obs <- max(abs(colMeans(S)))

  # Return Object
  list(
    p.value = mean(Q_bs > Q_obs)
  )
}

bs_parametric_naive_test <- function(X, B = 1000) {
  vectorize_cov <- function(x) {
    p <- ncol(x)
    idx <- combn(p, 2)
    do.call(
      cbind,
      map2(idx[1, ], idx[2, ], \(i, j)
      (x[, i] - mean(x[, i])) * (x[, j] - mean(x[, j])))
    )
  }

  # Calculate Naive Variance of ECE Terms
  n <- nrow(X)
  S <- vectorize_cov(X)
  W <- cov(S) / n

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
ece.test <- function(X, type = "z.test", B = 1000, conf.level = 0.95,
                     kappa = "gaussian", rho = NULL) {
  X <- as.matrix(X)
  if (type == "z.test") {
    z_test(X, conf.level = conf.level, kappa = kappa, rho = rho)
  } else if (type == "bs.multiplier") {
    bs_multiplier_test(X, B)
  } else if (type == "bs.multiplier.nosplit") {
    bs_multiplier_nosplit_test(X, B)
  } else if (type == "bs.parametric") {
    bs_parametric_test(X, B)
  } else if (type == "bs.parametric.psd") {
    bs_parametric_test(X, B, force_psd = TRUE)
  } else if (type == "bs.parametric.mask") {
    bs_parametric_test(X, B, mask_disjoint = TRUE)
  } else if (type == "bs.parametric.mask.psd") {
    bs_parametric_test(X, B, mask_disjoint = TRUE, force_psd = TRUE)
  } else if (type == "bs.parametric.lag1") {
    bs_parametric_test(X, B, drop_lag2 = TRUE)
  } else if (type == "bs.parametric.lag1.psd") {
    bs_parametric_test(X, B, drop_lag2 = TRUE, force_psd = TRUE)
  } else if (type == "bs.parametric.lag1.mask") {
    bs_parametric_test(X, B, drop_lag2 = TRUE, mask_disjoint = TRUE)
  } else if (type == "bs.parametric.lag1.mask.psd") {
    bs_parametric_test(X, B, drop_lag2 = TRUE, mask_disjoint = TRUE, force_psd = TRUE)
  } else if (type == "bs.parametric.naive") {
    bs_parametric_naive_test(X, B)
  }
}
