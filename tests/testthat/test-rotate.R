test_that("rotate handles vector input", {
  expect_equal(rotate(1:5, k = 1), c(2:5, 1))
  expect_equal(rotate(1:5, k = 2), c(3:5, 1:2))
  expect_equal(rotate(1:5, k = 5), 1:5) # FIX: full rotation
  expect_equal(rotate(1:5, k = 0), 1:5) # FIX: no rotation
})
