test_that("ece.cor returns 1 for identical series", {
  set.seed(1)
  x <- rnorm(100)
  expect_equal(ece.cor(x, x), 1)
})

test_that("ece.cor returns ~0 for independent series", {
  set.seed(42)
  cors <- replicate(500, ece.cor(rnorm(200), rnorm(200)))
  ci <- t.test(cors, conf.level = 0.99)$conf.int
  expect_true(ci[1] < 0 && 0 < ci[2])
})

test_that("ece.cor matrix diagonal is 1", {
  X <- matrix(rnorm(300), ncol = 3)
  expect_equal(diag(ece.cor(X)), rep(1, 3))
})

test_that("ece.cor matrix is symmetric", {
  X <- matrix(rnorm(300), ncol = 3)
  R <- ece.cor(X)
  expect_equal(R, t(R))
})
