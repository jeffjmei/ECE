test_that("scenario returns required fields", {
  params <- scenario(1)
  expected <- c("scenario", "h", "n", "p", "r", "cov_type", "S", "S_cholesky",
                "amp", "seed", "err_type", "scenario_param")
  expect_true(all(expected %in% names(params)))
})

test_that("scenario 1 has zero mean structure", {
  params <- scenario(1, n = 100, p = 3)
  expect_true(all(params$h == 0))
  expect_equal(dim(params$h), c(100, 3))
})

test_that("scenario 2 mean structure has correct dimensions", {
  params <- scenario(2, n = 100, p = 3)
  expect_equal(dim(params$h), c(100, 3))
})

test_that("scenario 2 respects L parameter", {
  params <- scenario(2, n = 100, p = 2, L = 25)
  expect_equal(params$scenario_param$L, 25)
})

test_that("scenario 3 mean structure has correct dimensions", {
  params <- scenario(3, n = 100, p = 3)
  expect_equal(dim(params$h), c(100, 3))
})

test_that("scenario 3 respects period parameter", {
  params <- scenario(3, n = 100, p = 2, period = 200)
  expect_equal(params$scenario_param$period, 200)
})

test_that("S_cholesky is cholesky of S", {
  params <- scenario(1, r = 0.5)
  expect_equal(t(params$S_cholesky) %*% params$S_cholesky, params$S)
})

test_that("err_type defaults to normal", {
  params <- scenario(1)
  expect_equal(params$err_type, "normal")
})

test_that("err_type is stored correctly", {
  params <- scenario(1, err_type = "t")
  expect_equal(params$err_type, "t")
})

test_that("unknown scenario raises error", {
  expect_error(scenario(99), "Unknown scenario")
})

# generate_err tests

test_that("generate_err returns matrix of correct dimensions", {
  for (err in c("normal", "t", "lognormal", "exponential", "cauchy")) {
    params <- scenario(1, n = 200, p = 4, err_type = err)
    e <- generate_err(params)
    expect_equal(dim(e), c(200, 4), label = err)
  }
})

test_that("generate_err normal errors have mean ~0 and correct covariance", {
  set.seed(42)
  S <- diag(2); S[1, 2] <- S[2, 1] <- 0.5
  params <- scenario(1, n = 50000, p = 2, r = 0.5, err_type = "normal")
  params$S <- S; params$S_cholesky <- chol(S)
  e <- generate_err(params)
  expect_lt(abs(mean(e)), 0.05)
  expect_true(max(abs(cov(e) - S)) < 0.05)
})

test_that("generate_err t errors have mean ~0 and correct covariance", {
  set.seed(42)
  S <- diag(2); S[1, 2] <- S[2, 1] <- 0.5
  params <- scenario(1, n = 50000, p = 2, r = 0.5, err_type = "t")
  params$S <- S; params$S_cholesky <- chol(S)
  e <- generate_err(params)
  expect_lt(abs(mean(e)), 0.05)
  expect_true(max(abs(cov(e) - S)) < 0.05)
})

test_that("generate_err lognormal errors have mean ~0 and correct covariance", {
  set.seed(42)
  S <- diag(2); S[1, 2] <- S[2, 1] <- 0.5
  params <- scenario(1, n = 50000, p = 2, r = 0.5, err_type = "lognormal")
  params$S <- S; params$S_cholesky <- chol(S)
  e <- generate_err(params)
  expect_lt(abs(mean(e)), 0.05)
  expect_true(max(abs(cov(e) - S)) < 0.05)
})

test_that("generate_err exponential errors have mean ~0 and correct covariance", {
  set.seed(42)
  S <- diag(2); S[1, 2] <- S[2, 1] <- 0.5
  params <- scenario(1, n = 50000, p = 2, r = 0.5, err_type = "exponential")
  params$S <- S; params$S_cholesky <- chol(S)
  e <- generate_err(params)
  expect_lt(abs(mean(e)), 0.05)
  expect_true(max(abs(cov(e) - S)) < 0.05)
})

test_that("unknown err_type raises error", {
  params <- scenario(1, err_type = "uniform")
  expect_error(generate_err(params), "unknown err_type")
})
