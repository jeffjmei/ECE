test_that("mod_idx handles positive indices", {
  expect_equal(mod_idx(1, 10), 1)
  expect_equal(mod_idx(10, 10), 10)
  expect_equal(mod_idx(11, 10), 1)
  expect_equal(mod_idx(12, 10), 2)
})

test_that("mod_idx wraps around zero and negative indices", {
  expect_equal(mod_idx(0, 10), 10)
  expect_equal(mod_idx(-1, 10), 9)
  expect_equal(mod_idx(-9, 10), 1)
  expect_equal(mod_idx(-10, 10), 10)
})

test_that("mod_idx works with vector input", {
  expect_equal(mod_idx(1:12, 10), c(1:10, 1, 2))
  expect_equal(mod_idx(c(0, -1, 21), 10), c(10, 9, 1))
})
