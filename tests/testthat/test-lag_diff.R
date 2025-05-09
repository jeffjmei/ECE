test_that("lag_diff returns squared difference when Y is NULL", {
  X <- c(-1, -1, 1, 1)
  expect_equal(lag_diff(X, k = 1), 8) # standard use
  expect_equal(lag_diff(X, k = 0), 0) # no rotation
})

test_that("lag_diff returns lagged cross-product when Y is provided", {
  X <- c(-1, -1, 1, 1)
  Y <- c(1, 1, -1, -1)
  expect_equal(lag_diff(X, Y, k = 1), -8)
})

test_that("edge cases", {
  X <- c(-1, -1, 1, 1)
  expect_equal(lag_diff(X, k = 2), -1) # FIX: k > n/2:
  expect_equal(lag_diff(X, k = 2), -1) # FIX: k < 0:
})

# TODO: matrix input
