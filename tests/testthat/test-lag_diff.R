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

test_that("mean of lag_term equals ece.cov", {
  x <- rnorm(50)
  y <- rnorm(50)
  expect_equal(mean(lag_term(x, y)), ece.cov(x, y))
})

test_that("lag_term has mean zero under independence", {
  set.seed(123)
  means <- map_dbl(1:500, ~ mean(lag_term(rnorm(200), rnorm(200))))
  expect_lt(abs(mean(means)), 0.05)
})

test_that("ece_terms returns C(p, 2) columns", {
  expect_equal(ncol(ece_terms(matrix(rnorm(300), ncol = 3))), 3)
  expect_equal(ncol(ece_terms(matrix(rnorm(400), ncol = 4))), 6)
})

test_that("ece_terms columns agree with lag_term on each pair", {
  X <- matrix(rnorm(300), ncol = 3)
  lt <- ece_terms(X)
  expect_equal(lt[, 1], lag_term(X[, 1], X[, 2]))
  expect_equal(lt[, 2], lag_term(X[, 1], X[, 3]))
  expect_equal(lt[, 3], lag_term(X[, 2], X[, 3]))
})

test_that("split_indep preserves all values", {
  x <- 1:99
  expect_equal(split_indep(x) |> unlist() |> sort() |> unname(), x)
})

test_that("split_indep returns stride sublists", {
  X <- matrix(rnorm(90), ncol = 2)
  expect_length(split_indep(X, stride = 3), 3)
  expect_length(split_indep(X, stride = 4), 4)
})

test_that("split_indep preserves matrix structure", {
  X <- matrix(rnorm(90), ncol = 2)
  expect_true(all(map_lgl(split_indep(X), is.matrix)))
})

