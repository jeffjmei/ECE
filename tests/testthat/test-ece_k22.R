test_that("ece.k22 errors on non-matrix or wrong dimensions", {
  expect_error(ece.k22(rnorm(100)))
  expect_error(ece.k22(matrix(rnorm(300), ncol = 3)))
})

test_that("ece.k22 returns a scalar", {
  X <- matrix(rnorm(200), ncol = 2)
  expect_length(ece.k22(X), 1)
  expect_length(ece.k22(X, rho = 0), 1)
})

test_that("rho = 0 drops sxy terms (equals null formula)", {
  X <- matrix(rnorm(200), ncol = 2)
  n   <- nrow(X)
  S   <- ece_terms(X)
  W   <- (1 / n^2) * (
    t(S) %*% S +
      t(rotate(S)) %*% S + t(S) %*% rotate(S) +
      t(rotate(S, 2)) %*% S + t(S) %*% rotate(S, 2)
  )
  sx2 <- ece.cov(X[, 1])
  sy2 <- ece.cov(X[, 2])
  wx2 <- ece.complexity(X[, 1])
  wy2 <- ece.complexity(X[, 2])
  null_formula <- n * W[1, 1] / (sx2 * sy2) - wx2 / sx2 - wy2 / sy2 - 5 / 2
  expect_equal(ece.k22(X, rho = 0), null_formula)
})

test_that("ece.k22 and ece.k22(rho=0) differ when correlation is nonzero", {
  params <- scenario(2, n = 500, r = 0.5, L = 4)
  set.seed(1)
  X <- generate_data(params)
  expect_false(isTRUE(all.equal(ece.k22(X), ece.k22(X, rho = 0))))
})

test_that("ece.k22(rho=0) is approximately unbiased for k22 under independence", {
  params <- scenario(2, n = 500, r = 0, L = 4, err_type = "normal")
  true_k22 <- params$kappa$k22

  set.seed(123)
  estimates <- map_dbl(1:500, ~ ece.k22(generate_data(params), rho = 0))
  ci <- t.test(estimates, conf.level = 0.99)$conf.int
  expect_true(ci[1] < true_k22 && true_k22 < ci[2])
})
