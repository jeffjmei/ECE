test_that("lag_diff returns squared difference with one argument", {
  x <- c(-1, -1, 1, 1)
  expect_equal(lag_diff(x, k = 1), 8) # standard use
  expect_equal(lag_diff(x, k = 0), 0) # no rotation
})

test_that("lag_diff returns lagged cross-product with two arguments", {
  x <- c(-1, -1, 1, 1)
  y <- c(1, 1, -1, -1)
  expect_equal(lag_diff(x, y, k = 1), -8)
})

test_that("lag_diff agrees with matrix multiplication via lag_mat", {
  x <- c(-1, -1, 1, 1)
  y <- c(1, 1, -1, -1)
  n <- length(x)
  expect_equal(lag_diff(x, k = 1), as.numeric(t(x) %*% lag_mat(n, 1) %*% x))
  expect_equal(lag_diff(x, y, k = 1), as.numeric(t(x) %*% lag_mat(n, 1) %*% y))
})

