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
  local_mocked_bindings(make_psd = function(S) -diag(nrow(S)))
  expect_warning(
    res <- bs_parametric_test(X, B = 50, force_psd = TRUE, normalize = TRUE),
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
