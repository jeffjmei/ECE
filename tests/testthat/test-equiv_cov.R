test_that("equiv.cov returns correct structure for matrix input", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  out <- equiv.cov(X, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
  expect_true(is.numeric(out$cov))
  expect_true(is.numeric(out$norm))
})

test_that("equiv.cov handles univariate input with Y", {
  X <- c(-1, -1, 1, 1)
  Y <- c(1, 1, -1, -1)
  out <- equiv.cov(X, Y, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
  expect_equal(out$cov, 0)
  expect_equal(out$norm, -1)
})

test_that("equiv.cov errors with mismatched X and Y", {
  expect_error(equiv.cov(1:10, 1:5), "X and Y must be the same length")
})
