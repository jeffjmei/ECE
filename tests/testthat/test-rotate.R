test_that("rotate handles vector input", {
  expect_equal(rotate(1:5, k = 1), c(2:5, 1))
  expect_equal(rotate(1:5, k = 2), c(3:5, 1:2))
  expect_equal(rotate(1:5, k = 5), 1:5)
  expect_equal(rotate(1:5, k = 0), 1:5)
  expect_equal(rotate(1:5, k = 6), rotate(1:5, k = k %% 5)) # FIX: for k > n
})

test_that("rotate handles matrix input", {
  X <- matrix(1:6, ncol = 2)
  expect_equal(rotate(X, k = 1), matrix(c(2, 3, 1, 5, 6, 4), ncol = 2))
  expect_equal(rotate(X, k = 2), matrix(c(3, 1, 2, 6, 4, 5), ncol = 2))
  expect_equal(rotate(X, k = 3), X)
  expect_equal(rotate(X, k = 0), X)
  expect_equal(rotate(X, k = 4), rotate(X, k = k %% 3)) # FIX: for k > n
})
