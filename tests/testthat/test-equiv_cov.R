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

test_that("equiv.cov accepts matrix inputs", {
  # FIX: test value
  n <- 10
  S_ind <- matrix(c(2, 0, 0, 3), nrow = 2)
  S_dep <- matrix(c(2, 1, 1, 3), nrow = 2)
  e <- mvrnorm(n = n, mu = c(0, 0), Sigma = S_ind)
  hx <- c(rep(-1, n / 2), rep(1, n / 2))
  hy <- c(rep(1, n / 2), rep(-1, n / 2))
  X <- hx + e[, 1]
  Y <- hy + e[, 2]
  out <- equiv.cov(X, Y, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
})

test_that("equiv.cov errors with mismatched X and Y", {
  expect_error(equiv.cov(1:10, 1:5), "X and Y must be the same length")
})
