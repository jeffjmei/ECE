test_that("formula and matrix SE agree (Gaussian kappa, nonzero rho)", {
  set.seed(1)
  params <- scenario(2, n = 200, r = 0.3, L = 4)
  X <- generate_data(params)
  expect_equal(
    ece.cor.se(X, kappa = "gaussian"),
    ece_cor_se_ref(X, params = NULL),
    tolerance = 1e-10
  )
})

test_that("formula and matrix SE agree (oracle kappa)", {
  set.seed(1)
  params <- scenario(2, n = 200, r = 0.3, L = 4)
  X <- generate_data(params)
  expect_equal(
    ece.cor.se(X, kappa = params$kappa),
    ece_cor_se_ref(X, params = params),
    tolerance = 1e-10
  )
})

test_that("p-value is a valid probability", {
  params <- scenario(2, n = 1000, r = 0, L = 4)

  results <- map_lgl(1:100, ~ {
    X <- generate_data(params)
    pval <- ece.test(X[, 1:2])$p.value
    pval >= 0 && pval <= 1
  })

  expect_true(all(results))
})

test_that("estimator is unbiased under null", {
  params <- scenario(2, n = 1000, r = 0, L = 4)

  set.seed(123)
  rxy_est_sim <- map_dbl(1:1000, ~ {
    X <- generate_data(params)
    ece.test(X[, 1:2])$estimate
  })

  ci <- t.test(rxy_est_sim, conf.level = 0.99)$conf.int
  rxy <- params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2])
  expect_true((ci[1] < rxy) & (rxy < ci[2]))
})

test_that("bs_multiplier returns vector of length B", {
  s <- matrix(rnorm(90), ncol = 3)
  expect_length(bs_multiplier(s, B = 50), 50)
})

test_that("bs_multiplier returns non-negative values", {
  s <- matrix(rnorm(90), ncol = 3)
  expect_true(all(bs_multiplier(s, B = 50) >= 0))
})

test_that("type I error rate is controlled", {
  params <- scenario(2, n = 1000, r = 0, L = 4)

  set.seed(123)
  pvals <- map_dbl(1:1000, ~ {
    X <- generate_data(params)
    ece.test(X[, 1:2])$p.value
  })

  type1_error_rate <- mean(pvals < 0.05)
  expect_lt(abs(type1_error_rate - 0.05), 0.015)
})

# ---- normalized parametric bootstrap ----------------------------------------

test_that("normalized parametric bootstrap returns a valid p-value", {
  set.seed(1)
  X <- generate_data(scenario(2, n = 500, r = 0, L = 4))
  pval <- ece.test(X, type = "bs.parametric.norm", B = 200)$p.value
  expect_true(pval >= 0 && pval <= 1)
})

test_that("every .norm type dispatches and returns a p-value", {
  set.seed(1)
  X <- generate_data(scenario(2, n = 500, r = 0, L = 4))
  suffixes <- c(
    "", ".psd", ".mask", ".mask.psd", ".lag1", ".lag1.psd", ".lag1.mask",
    ".lag1.mask.psd", ".lag1.diag", ".lag1.diag.mask", ".lag1.diag.psd",
    ".lag1.diag.mask.psd"
  )
  for (suf in suffixes) {
    res <- ece.test(X, type = paste0("bs.parametric", suf, ".norm"), B = 100)
    expect_true(is.numeric(res$p.value), info = suf)
  }
})

test_that("normalized test is invariant to rescaling a series", {
  set.seed(2)
  X <- generate_data(scenario(2, n = 500, r = 0.3, L = 4))
  X_scaled <- X
  X_scaled[, 1] <- 10 * X_scaled[, 1]

  set.seed(10)
  p_norm <- ece.test(X, type = "bs.parametric.norm", B = 500)$p.value
  set.seed(10)
  p_norm_scaled <- ece.test(X_scaled, type = "bs.parametric.norm", B = 500)$p.value
  expect_equal(p_norm, p_norm_scaled)
})

test_that("normalization is a no-op for a single pair", {
  set.seed(3)
  X <- generate_data(scenario(2, n = 500, r = 0.2, L = 4))[, 1:2]

  set.seed(10)
  p_raw <- ece.test(X, type = "bs.parametric", B = 500)$p.value
  set.seed(10)
  p_norm <- ece.test(X, type = "bs.parametric.norm", B = 500)$p.value
  expect_equal(p_raw, p_norm)
})

test_that("normalized test is undefined with a warning when a variance is non-positive", {
  set.seed(4)
  X <- matrix(rnorm(300), ncol = 3)
  local_mocked_bindings(ece.cov = function(...) -1)
  expect_warning(
    res <- bs_parametric_test(X, B = 50, normalize = TRUE),
    "non-positive"
  )
  expect_true(is.na(res$p.value))
})

test_that("normalized test has controlled type I error", {
  params <- scenario(2, n = 1000, r = 0, L = 4)
  set.seed(5)
  pvals <- map_dbl(1:200, ~ {
    X <- generate_data(params)
    ece.test(X, type = "bs.parametric.lag1.diag.norm", B = 300)$p.value
  })
  # rejection rate at 0.10 should be near 0.10 (200 reps: SE ~ 0.02)
  expect_lt(abs(mean(pvals < 0.10) - 0.10), 0.07)
})

# ---- centered parametric bootstrap ------------------------------------------

# Runs bs_parametric_test with MASS::mvrnorm mocked out, returning the Sigma it
# was handed so the variance estimate W can be inspected directly.
capture_W <- function(X, ...) {
  W <- NULL
  local_mocked_bindings(
    mvrnorm = function(n, mu, Sigma) {
      W <<- Sigma
      matrix(0, n, length(mu))
    },
    .package = "MASS"
  )
  bs_parametric_test(X, B = 5, ...)
  W
}

test_that("every .center type dispatches and returns a p-value", {
  set.seed(1)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0, L = 4))
  suffixes <- c(
    "", ".psd", ".mask", ".mask.psd", ".lag1", ".lag1.psd", ".lag1.mask",
    ".lag1.mask.psd", ".lag1.diag", ".lag1.diag.mask", ".lag1.diag.psd",
    ".lag1.diag.mask.psd"
  )
  for (suf in suffixes) {
    pval <- ece.test(X, type = paste0("bs.parametric", suf, ".center"), B = 100)$p.value
    expect_true(pval >= 0 && pval <= 1, info = suf)
  }
})

test_that("test.diag dispatches every .center method", {
  set.seed(1)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0, L = 4))
  suffixes <- c(
    "", ".psd", ".mask", ".mask.psd", ".lag1", ".lag1.psd", ".lag1.mask",
    ".lag1.mask.psd", ".lag1.diag", ".lag1.diag.mask", ".lag1.diag.psd",
    ".lag1.diag.mask.psd"
  )
  for (suf in suffixes) {
    res <- test.diag(X, method = paste0("bs.parametric", suf, ".center"), B = 100)
    expect_true(is.numeric(res$p.value), info = suf)
  }
})

test_that("centering subtracts the rank-one term (5/n) sbar sbar' from the full W", {
  set.seed(2)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0.3, L = 4))
  n <- nrow(X)
  s_bar <- colMeans(ece_terms(X))
  expect_equal(
    capture_W(X, center = TRUE),
    capture_W(X, center = FALSE) - (5 / n) * tcrossprod(s_bar),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("centering subtracts the rank-one term (3/n) sbar sbar' from the lag-1 W", {
  set.seed(2)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0.3, L = 4))
  n <- nrow(X)
  s_bar <- colMeans(ece_terms(X))
  expect_equal(
    capture_W(X, drop_lag2 = TRUE, center = TRUE),
    capture_W(X, drop_lag2 = TRUE, center = FALSE) - (3 / n) * tcrossprod(s_bar),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("centering never increases the variance estimate", {
  set.seed(3)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0.5, L = 4))
  expect_true(all(diag(capture_W(X, center = TRUE)) <= diag(capture_W(X, center = FALSE))))
})

test_that("the observed statistic is computed from the raw terms", {
  set.seed(4)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0.3, L = 4))
  q_obs <- max(abs(colMeans(ece_terms(X))))

  # Every bootstrap draw sits just below Q_obs, so p = 0 unless Q_obs was centered to 0.
  local_mocked_bindings(
    mvrnorm = function(n, mu, Sigma) matrix(0.999 * q_obs, n, length(mu)),
    .package = "MASS"
  )
  expect_equal(bs_parametric_test(X, B = 10, center = TRUE)$p.value, 0)
})

test_that("centering is a no-op for the p-value when the terms already have mean zero", {
  set.seed(5)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0, L = 4))
  s_bar <- colMeans(ece_terms(X))
  # With s_bar ~ 0 the correction (c/n) sbar sbar' is negligible relative to W.
  W_raw <- capture_W(X, center = FALSE)
  W_ctr <- capture_W(X, center = TRUE)
  expect_lt(max(abs(W_raw - W_ctr)) / max(abs(W_raw)), 0.05)
})

test_that("centered test has controlled type I error", {
  params <- scenario(2, n = 1000, r = 0, L = 4)
  set.seed(6)
  pvals <- map_dbl(1:200, ~ {
    X <- generate_data(params)
    ece.test(X, type = "bs.parametric.lag1.diag.center", B = 300)$p.value
  })
  # rejection rate at 0.10 should be near 0.10 (200 reps: SE ~ 0.02)
  expect_lt(abs(mean(pvals < 0.10) - 0.10), 0.07)
})

test_that("normalization rescales W to the ECE correlation scale", {
  set.seed(6)
  X <- generate_data(scenario(2, n = 500, p = 3, r = 0.3, L = 4))
  sd_x <- sqrt(diag(ece.cov(X)))
  idx <- combn(3, 2)
  d <- sd_x[idx[1, ]] * sd_x[idx[2, ]]

  W_raw <- capture_W(X)
  W_norm <- capture_W(X, normalize = TRUE)
  expect_equal(W_norm, W_raw / outer(d, d), ignore_attr = TRUE)
})
