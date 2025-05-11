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
  for (s in scenario_list) {
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

# TEST: return.norm yields expected value (no noise)
n_scenarios <- 11
misspecified_scenarios <- c(8, 9)
scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
for (scenario_num in scenario_list) {
  params <- scenario(scenario_num)
  h <- params$h
  n <- params$n

  # estimated value
  w_mat_est <- equiv.cov(h, return.norm = TRUE)$norm

  # expected value
  wx <- lag_diff(h[, 1]) / n
  wy <- lag_diff(h[, 2]) / n
  wxy <- lag_diff(h[, 1], h[, 2]) / n
  w_mat_exp <- matrix(c(wx, wxy, wxy, wy), nrow = 2)

  test_that(sprintf("ECE slope is wxy (Scenario %d) without noise", scenario_num), {
    expect_equal(w_mat_est, w_mat_exp)
  })
}

# TEST: return.norm yields expected value (with noise)
n_scenarios <- 11
misspecified_scenarios <- c(8, 9)
scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
for (scenario_num in scenario_list) {
  params <- scenario(scenario_num, sxy = 0, sx = sqrt(3), sy = sqrt(5))
  h <- params$h
  n <- params$n

  # expected value
  wx <- lag_diff(h[, 1]) / n
  wy <- lag_diff(h[, 2]) / n
  wxy <- lag_diff(h[, 1], h[, 2]) / n
  w_mat_exp <- matrix(c(wx, wxy, wxy, wy), nrow = 2)

  # estimated value
  w_mat_sims <- map(1:10000, ~ {
    X <- generate_data(params)
    w_mat_est <- equiv.cov(X, return.norm = TRUE)$norm
    return(
      list(
        wx = w_mat_est[1, 1],
        wy = w_mat_est[2, 2],
        wxy = w_mat_est[1, 2]
      )
    )
  })
  wx_est <- map_dbl(w_mat_sims, "wx")
  wy_est <- map_dbl(w_mat_sims, "wy")
  wxy_est <- map_dbl(w_mat_sims, "wxy")

  # check if values are within confidence bound
  wx_ci <- t.test(wx_est, conf.level = 0.99)$conf.int
  wy_ci <- t.test(wy_est, conf.level = 0.99)$conf.int
  wxy_ci <- t.test(wxy_est, conf.level = 0.99)$conf.int
  test_that(sprintf("ECE slope is wxy (Scenario %d) with noise", scenario_num), {
    expect_true((wx_ci[1] < wx) & (wx < wx_ci[2]))
    expect_true((wy_ci[1] < wy) & (wy < wy_ci[2]))
    expect_true((wxy_ci[1] < wxy) & (wxy < wxy_ci[2]))
  })
}
