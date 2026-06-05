
# Symmetry (exact) --------------------------------------------------------
# lag_term(x,y) == lag_term(y,x), so swapping arguments must permute the list

test_that("ece.kappa.mom: swapping x and y permutes the kappa list", {
  params <- scenario(2, n = 500, r = 0.5, L = 4)
  set.seed(1)
  X <- generate_data(params)
  k_xy <- ece.kappa.mom(X[, 1], X[, 2])
  k_yx <- ece.kappa.mom(X[, 2], X[, 1])
  expect_equal(k_xy$k40, k_yx$k04)
  expect_equal(k_xy$k04, k_yx$k40)
  expect_equal(k_xy$k31, k_yx$k13)
  expect_equal(k_xy$k13, k_yx$k31)
  expect_equal(k_xy$k22, k_yx$k22)
})

test_that("ece.kappa.matrix: swapping x and y permutes the kappa list", {
  params <- scenario(2, n = 500, r = 0.5, L = 4)
  set.seed(1)
  X <- generate_data(params)
  k_xy <- ece.kappa.matrix(X[, 1], X[, 2])
  k_yx <- ece.kappa.matrix(X[, 2], X[, 1])
  expect_equal(k_xy$k40, k_yx$k04)
  expect_equal(k_xy$k04, k_yx$k40)
  expect_equal(k_xy$k31, k_yx$k13)
  expect_equal(k_xy$k13, k_yx$k31)
  expect_equal(k_xy$k22, k_yx$k22)
})

# Gaussian convergence (all kappas) ----------------------------------------

test_that("ece.kappa.mom: unbiased for all kappas, Gaussian independent, change points", {
  params <- scenario(2, n = 2000, r = 0, L = 4, err_type = "normal")
  set.seed(1)
  ests <- map(1:500, ~ ece.kappa.mom(generate_data(params)))
  for (nm in c("k40", "k04", "k22", "k31", "k13")) {
    ci <- t.test(map_dbl(ests, nm), conf.level = 0.99)$conf.int
    expect_true(ci[1] < params$kappa[[nm]] && params$kappa[[nm]] < ci[2],
                label = paste("mom", nm, "Gaussian r=0"))
  }
})

test_that("ece.kappa.matrix: unbiased for cross-kappas, Gaussian independent, change points", {
  # k40/k04 are known to have negative bias in the matrix estimator; only test k22/k31/k13
  params <- scenario(2, n = 2000, r = 0, L = 4, err_type = "normal")
  set.seed(2)
  ests <- map(1:500, ~ ece.kappa.matrix(generate_data(params)))
  for (nm in c("k22", "k31", "k13")) {
    ci <- t.test(map_dbl(ests, nm), conf.level = 0.99)$conf.int
    expect_true(ci[1] < params$kappa[[nm]] && params$kappa[[nm]] < ci[2],
                label = paste("matrix", nm, "Gaussian r=0"))
  }
})

test_that("ece.kappa.mom: unbiased for cross-kappas, Gaussian correlated, change points", {
  params <- scenario(2, n = 2000, r = 0.5, L = 4, err_type = "normal")
  set.seed(3)
  ests <- map(1:500, ~ ece.kappa.mom(generate_data(params)))
  for (nm in c("k22", "k31", "k13")) {
    ci <- t.test(map_dbl(ests, nm), conf.level = 0.99)$conf.int
    expect_true(ci[1] < params$kappa[[nm]] && params$kappa[[nm]] < ci[2],
                label = paste("mom", nm, "Gaussian r=0.5"))
  }
})

test_that("ece.kappa.matrix: unbiased for cross-kappas, Gaussian correlated, change points", {
  params <- scenario(2, n = 2000, r = 0.5, L = 4, err_type = "normal")
  set.seed(4)
  ests <- map(1:500, ~ ece.kappa.matrix(generate_data(params)))
  for (nm in c("k22", "k31", "k13")) {
    ci <- t.test(map_dbl(ests, nm), conf.level = 0.99)$conf.int
    expect_true(ci[1] < params$kappa[[nm]] && params$kappa[[nm]] < ci[2],
                label = paste("matrix", nm, "Gaussian r=0.5"))
  }
})

# Non-Gaussian convergence --------------------------------------------------

test_that("ece.kappa.mom: unbiased for k40/k04/k22, exponential errors", {
  params <- scenario(2, n = 2000, r = 0, L = 4, err_type = "exponential")
  set.seed(5)
  ests <- map(1:1000, ~ ece.kappa.mom(generate_data(params)))
  for (nm in c("k40", "k04", "k22")) {
    ci <- t.test(map_dbl(ests, nm), conf.level = 0.99)$conf.int
    expect_true(ci[1] < params$kappa[[nm]] && params$kappa[[nm]] < ci[2],
                label = paste("mom", nm, "exponential"))
  }
})

test_that("ece.kappa.matrix: unbiased for k22, exponential errors", {
  # k40/k04 have known negative bias in the matrix estimator; only test k22
  params <- scenario(2, n = 2000, r = 0, L = 4, err_type = "exponential")
  set.seed(6)
  ests <- map(1:1000, ~ ece.kappa.matrix(generate_data(params)))
  ci <- t.test(map_dbl(ests, "k22"), conf.level = 0.99)$conf.int
  expect_true(ci[1] < params$kappa$k22 && params$kappa$k22 < ci[2],
              label = "matrix k22 exponential")
})

# rho = 0 null forcing ------------------------------------------------------

test_that("rho=0 changes cross-kappas when data are correlated", {
  params <- scenario(2, n = 500, r = 0.5, L = 4)
  set.seed(1)
  X <- generate_data(params)
  expect_false(isTRUE(all.equal(ece.kappa.mom(X)$k22,    ece.kappa.mom(X, rho = 0)$k22)))
  expect_false(isTRUE(all.equal(ece.kappa.matrix(X)$k22, ece.kappa.matrix(X, rho = 0)$k22)))
})

# Mom vs Matrix consistency -------------------------------------------------

test_that("mom and matrix converge to the same value for large n", {
  params <- scenario(2, n = 2000, r = 0.5, L = 4, err_type = "normal")
  set.seed(7)
  mom_ests <- map(1:300, ~ ece.kappa.mom(generate_data(params)))
  set.seed(8)
  mat_ests <- map(1:300, ~ ece.kappa.matrix(generate_data(params)))
  for (nm in c("k40", "k04", "k22", "k31", "k13")) {
    expect_equal(mean(map_dbl(mom_ests, nm)), mean(map_dbl(mat_ests, nm)),
                 tolerance = 0.2, label = paste(nm, "mom vs matrix"))
  }
})
