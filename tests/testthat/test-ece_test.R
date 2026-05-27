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
