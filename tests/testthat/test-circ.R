test_that("expand_sym reconstructs full first row by symmetry", {
  n      <- 10
  a_half <- c(1, -1, 1/2, rep(0, floor(n / 2) - 2))
  a_full <- expand_sym(a_half, n)
  expect_equal(a_full[1], a_half[1])
  for (i in seq_len(floor(n / 2))) {
    expect_equal(a_full[i + 1], a_full[n - i + 1])
  }
})

test_that("ece_first_row matches expected a for L = 2", {
  n          <- 10
  a_expected <- c(1, -1, 1/2, rep(0, n - 5), 1/2, -1)
  expect_equal(ece_first_row(n, 2), a_expected)
})

test_that("ece_min_norm matches ece_first_row for L = 2", {
  n <- 10
  expect_equal(ece_min_norm(n, 2), ece_first_row(n, 2))
})

test_that("ece_min_norm satisfies unbiasedness constraints for L = 3", {
  n <- 20
  L <- 3
  a <- ece_min_norm(n, L)
  A <- circ(a) / n
  G <- constraint_matrix(n, L)
  a_half <- a[seq_len(floor(n / 2) + 1)]
  expect_equal(as.numeric(G %*% a_half), rep(0, nrow(G)), tolerance = 1e-10)
  expect_equal(sum(diag(A)), 1, tolerance = 1e-10)
})

test_that("ece_min_norm has smaller or equal norm than ece_first_row for L = 3", {
  n <- 20
  L <- 3
  expect_lte(sum(ece_min_norm(n, L)^2), sum(ece_first_row(n, L)^2))
})

test_that("circ constructs correct circulant matrix", {
  a <- c(2, -1, 0, -1)
  A <- circ(a)
  expect_equal(A[1, ], a)
  expect_equal(A[2, ], c(-1, 2, -1, 0))
  expect_equal(A[3, ], c(0, -1, 2, -1))
  expect_equal(A[4, ], c(-1, 0, -1, 2))
})

test_that("C_k = C_1^k", {
  n <- 6
  k <- 2
  C1 <- circ(c(0, 1, rep(0, n - 2)))
  Ck <- circ(c(rep(0, k), 1, rep(0, n - k - 1)))
  C1_power_k <- Reduce("%*%", rep(list(C1), k))
  expect_equal(Ck, C1_power_k)
})

test_that("C_k^T = C_{n-k}", {
  n <- 6
  k <- 2
  Ck  <- circ(c(rep(0, k), 1, rep(0, n - k - 1)))
  Cnk <- circ(c(rep(0, n - k), 1, rep(0, k - 1)))
  expect_equal(t(Ck), Cnk)
})

test_that("C_k = circ(e_k)", {
  n <- 5
  for (k in 0:(n - 1)) {
    e_k <- as.numeric(seq_len(n) - 1 == k)
    Ck  <- circ(c(rep(0, k), 1, rep(0, n - k - 1)))
    expect_equal(circ(e_k), Ck)
  }
})
