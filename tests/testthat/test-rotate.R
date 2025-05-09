test_that("rotate handles vector input", {
  expect_equal(rotate(1:5, k = 1), c(5, 1:4))
  expect_equal(rotate(1:5, k = 2), c(4:5, 1:3))
  expect_equal(rotate(1:5, k = 5), 1:5) # full rotation
})

test_that("rotate handles matrix input", {
  X <- matrix(1:6, ncol = 2)
  expect_equal(rotate(X, k = 1), X[c(3, 1, 2), ])
  expect_equal(rotate(X, k = 2), X[c(2:3, 1), ])
  expect_equal(rotate(X, k = 3), X) # full rotation
})

test_that("rotate handles k = 0", {
  expect_equal(rotate(1:5, k = 0), 1:5)
  X <- matrix(1:6, ncol = 2)
  expect_equal(rotate(X, k = 0), X)
})
