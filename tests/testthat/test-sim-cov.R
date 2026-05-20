test_that("cov_cs has unit diagonal and off-diagonal r", {
  S <- cov_cs(0.4, 4)
  expect_equal(diag(S), rep(1, 4))
  expect_equal(S[upper.tri(S)], rep(0.4, 6))
})

test_that("cov_ar1 satisfies S[i,j] = r^|i-j|", {
  S <- cov_ar1(0.5, 4)
  for (i in 1:4) for (j in 1:4) expect_equal(S[i, j], 0.5^abs(i - j))
})

test_that("make_cov unknown cov_type raises error", {
  expect_error(make_cov(0.5, 3, "toeplitz"), "Unknown cov_type")
})
