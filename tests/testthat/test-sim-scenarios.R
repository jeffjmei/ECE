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
