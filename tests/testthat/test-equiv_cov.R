test_that("equiv.cov returns correct structure for matrix input", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  out <- equiv.cov(X, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
  expect_true(is.numeric(out$cov))
  expect_true(is.numeric(out$norm))
})

test_that("equiv.cov handles univariate input with Y", {
  X <- c(-1, -1, 1, 1)
  Y <- c(1, 1, -1, -1)
  out <- equiv.cov(X, Y, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
  expect_equal(out$cov, 0)
  expect_equal(out$norm, -1)
})

test_that("equiv.cov accepts matrix inputs", {
  # FIX: test value
  n <- 10
  S_ind <- matrix(c(2, 0, 0, 3), nrow = 2)
  S_dep <- matrix(c(2, 1, 1, 3), nrow = 2)
  e <- mvrnorm(n = n, mu = c(0, 0), Sigma = S_ind)
  hx <- c(rep(-1, n / 2), rep(1, n / 2))
  hy <- c(rep(1, n / 2), rep(-1, n / 2))
  X <- hx + e[, 1]
  Y <- hy + e[, 2]
  out <- equiv.cov(X, Y, return.norm = TRUE)
  expect_named(out, c("cov", "norm"))
})

test_that("equiv.cov errors with mismatched X and Y", {
  expect_error(equiv.cov(1:10, 1:5), "X and Y must be the same length")
})

test_that("equiv.cov outputs correct norms", {
  # exclude misspecified cases
  n_scenarios <- 11
  misspecified_scenarios <- c(8, 9)
  scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
  for (s in 10) {
    # set parameters
    params <- scenario(s)
    n <- params$n
    h <- params$h

    expect_equal(
      equiv.cov(h[, 1], h[, 2], return.norm = TRUE)$norm,
      lag_diff(h[, 1], h[, 2]) / n
    )
  }
})

test_that("covariance matrix is correct", {
  params <- scenario(11, s12 = 0, n = 1000)
  params$S[1, 1] <- 3
  params$S[2, 2] <- 5
  S <- params$S
  var_sims <- map(1:1000, ~ {
    X <- generate_data(params)
    S_hat <- equiv.cov(X)
    return(
      list(
        sx = sqrt(S_hat[1, 1]),
        sy = sqrt(S_hat[2, 2]),
        sxy = S_hat[1, 2]
      )
    )
  })
  sx <- map_dbl(var_sims, "sx") %>% mean()
  sy <- map_dbl(var_sims, "sy") %>% mean()
  sxy <- map_dbl(var_sims, "sxy") %>% mean()
  expect_equal(sx^2, params$S[1, 1], tol = 0.05)
  expect_equal(sy^2, params$S[2, 2], tol = 0.05)
  expect_equal(sxy, params$S[1, 2], tol = 0.05)
})
