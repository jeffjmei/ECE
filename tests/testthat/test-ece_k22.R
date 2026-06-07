test_that("ece.kappa returns a named list with all five kappas", {
  X <- matrix(rnorm(200), ncol = 2)
  k <- ece.kappa(X)
  expect_type(k, "list")
  expect_named(k, c("k40", "k31", "k22", "k13", "k04"), ignore.order = TRUE)
})

test_that("ece.kappa and ece.kappa(rho=0) differ when correlation is nonzero", {
  params <- scenario(2, n = 500, r = 0.5, L = 4)
  set.seed(1)
  X <- generate_data(params)
  expect_false(isTRUE(all.equal(ece.kappa(X)$k22, ece.kappa(X, rho = 0)$k22)))
})

test_that("ece.kappa(rho=0) is approximately unbiased for k22 under independence", {
  params <- scenario(2, n = 1000, r = 0, L = 4, err_type = "normal")
  true_k22 <- params$kappa$k22

  set.seed(123)
  estimates <- map_dbl(1:500, ~ ece.kappa(generate_data(params), rho = 0)$k22)
  ci <- t.test(estimates, conf.level = 0.99)$conf.int
  expect_true(ci[1] < true_k22 && true_k22 < ci[2])
})

test_that("ece.kappa is approximately unbiased for k22 under alternative", {
  params <- scenario(2, n = 500, r = 0.5, L = 4, err_type = "normal")
  true_k22 <- params$kappa$k22

  set.seed(456)
  estimates <- map_dbl(1:500, ~ ece.kappa(generate_data(params))$k22)
  ci <- t.test(estimates, conf.level = 0.99)$conf.int
  expect_true(ci[1] < true_k22 && true_k22 < ci[2])
})
