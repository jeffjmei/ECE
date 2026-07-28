test_that("S_cholesky is cholesky of S", {
  params <- scenario(1, r = 0.5)
  expect_equal(t(params$S_cholesky) %*% params$S_cholesky, params$S)
})

test_that("unknown scenario raises error", {
  expect_error(scenario(99), "Unknown scenario")
})

test_that("unknown err_type raises error", {
  expect_error(scenario(1, err_type = "uniform"), "unknown err_type")
})

test_that("sigma default leaves S unchanged", {
  params_default <- scenario(1, r = 0.5)
  params_explicit <- scenario(1, r = 0.5, sigma = c(1, 1))
  expect_equal(params_default$S, params_explicit$S)
})

test_that("sigma scales diagonal of S correctly", {
  sigma <- c(2, 0.5)
  params <- scenario(1, r = 0, sigma = sigma)
  expect_equal(diag(params$S), sigma^2)
})

test_that("sigma scales off-diagonal of S correctly", {
  sigma <- c(2, 0.5)
  r <- 0.6
  params <- scenario(1, r = r, sigma = sigma)
  expect_equal(params$S[1, 2], sigma[1] * sigma[2] * r)
})

test_that("S_cholesky is Cholesky of S when sigma != 1", {
  params <- scenario(1, r = 0.5, sigma = c(2, 0.5))
  expect_equal(t(params$S_cholesky) %*% params$S_cholesky, params$S)
})

test_that("kappa$k40 equals 3 for normal errors regardless of sigma", {
  params <- scenario(1, r = 0, sigma = c(3, 0.5), err_type = "normal")
  expect_equal(params$kappa$k40, 3, tolerance = 0.01)
  expect_equal(params$kappa$k04, 3, tolerance = 0.01)
})

test_that("kappa$k22 equals 1 for independent normal errors regardless of sigma", {
  params <- scenario(1, r = 0, sigma = c(3, 0.5), err_type = "normal")
  expect_equal(params$kappa$k22, 1, tolerance = 0.01)
})

test_that("params$kappa equals params$kappa_null when r = 0", {
  for (err in c("normal", "exponential")) {
    params <- scenario(1, r = 0, err_type = err)
    expect_equal(params$kappa, params$kappa_null, label = err)
  }
})

test_that("params$kappa differs from params$kappa_null when r = 0.5", {
  for (err in c("normal", "exponential")) {
    params <- scenario(1, r = 0.5, err_type = err)
    expect_false(isTRUE(all.equal(params$kappa, params$kappa_null)), label = err)
  }
})

test_that("same seed reproduces the same semi-random-walk change points", {
  params1 <- scenario(5, n = 500, L = 4, prob = 0.1, seed = 42)
  params2 <- scenario(5, n = 500, L = 4, prob = 0.1, seed = 42)
  expect_identical(params1$scenario_param$cp, params2$scenario_param$cp)
})

test_that("different seeds give different semi-random-walk change points", {
  params1 <- scenario(5, n = 500, L = 4, prob = 0.1, seed = 42)
  params2 <- scenario(5, n = 500, L = 4, prob = 0.1, seed = 7)
  expect_false(identical(params1$scenario_param$cp, params2$scenario_param$cp))
})

test_that("generate_err produces correct covariance structure", {
  S <- diag(2); S[1, 2] <- S[2, 1] <- 0.5
  for (err in c("normal", "t", "lognormal", "exponential")) {
    set.seed(42)
    params <- scenario(1, n = 50000, p = 2, r = 0.5, err_type = err)
    params$S <- S; params$S_cholesky <- chol(S)
    e <- generate_err(params)
    expect_lt(abs(mean(e)), 0.05, label = err)
    expect_true(max(abs(cov(e) - S)) < 0.05, label = err)
  }
})
