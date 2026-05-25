test_that("ece.cor.se.formula matches ece.cor.se.matrix", {
  params <- scenario(2, r = 0.5, L = 4)
  expect_equal(ece.cor.se.formula(params), ece.cor.se.matrix(params))

  params <- scenario(1, r = 0)
  expect_equal(ece.cor.se.formula(params), ece.cor.se.matrix(params))
})

test_that("ece.cor.se.formula is scale-invariant", {
  params <- scenario(2, r = 0.5, L = 4)
  params_scale <- params
  a <- 3; b <- 5
  params_scale$S[1, 1] <- a^2 * params$S[1, 1]
  params_scale$S[2, 2] <- b^2 * params$S[2, 2]
  params_scale$S[1, 2] <- params_scale$S[2, 1] <- a * b * params$S[1, 2]
  params_scale$h[, 1] <- a * params$h[, 1]
  params_scale$h[, 2] <- b * params$h[, 2]
  expect_equal(ece.cor.se.formula(params), ece.cor.se.formula(params_scale))
})
