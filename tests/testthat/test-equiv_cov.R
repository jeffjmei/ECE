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
  expect_equal(out$norm, -2)
})

test_that("equiv.cov accepts matrix inputs", {
  n <- 10
  S_ind <- matrix(c(2, 0, 0, 3), nrow = 2)
  S_dep <- matrix(c(2, 1, 1, 3), nrow = 2)
  e <- MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = S_ind)
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
    params <- scenario(s, sxy = 0, sx = 1, sy = 1, n = 1000)
    n <- params$n
    h <- params$h

    expect_equal(
      equiv.cov(h[, 1], h[, 2], return.norm = TRUE)$norm,
      lag_diff(h[, 1], h[, 2]) / n
    )
  }
})

test_that("covariance matrix is correct", {
  params <- scenario(11, sxy = 0, sx = sqrt(3), sy = sqrt(5), n = 1000)
  S <- params$S
  set.seed(123)
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
  sx_sim <- map_dbl(var_sims, "sx")
  sy_sim <- map_dbl(var_sims, "sy")
  sxy_sim <- map_dbl(var_sims, "sxy")

  sx_ci <- t.test(sx_sim, conf.level = 0.99)$conf.int
  sy_ci <- t.test(sy_sim, conf.level = 0.99)$conf.int
  sxy_ci <- t.test(sxy_sim, conf.level = 0.99)$conf.int

  sx <- sqrt(params$S[1, 1])
  sy <- sqrt(params$S[2, 2])
  sxy <- params$S[1, 2]

  expect_true((sx_ci[1] < sx) & (sx < sx_ci[2]))
  expect_true((sy_ci[1] < sy) & (sy < sy_ci[2]))
  expect_true((sxy_ci[1] < sxy) & (sxy < sxy_ci[2]))
})

# TEST: return.norm yields expected value (no noise)
n_scenarios <- 11
misspecified_scenarios <- c(8, 9)
scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
for (scenario_num in scenario_list) {
  params <- scenario(scenario_num, sxy = 0, sx = 1, sy = 1, n = 1000)
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
  params <- scenario(scenario_num, sxy = 0, sx = 1, sy = 1, n = 1000)
  sim_results <- map(1:1000, ~ {
    sim_results_inner <- map(1:10, ~ {
      X <- generate_data(params)
      ece_obj <- equiv.cov(X, return.norm = TRUE)
      return(
        list(
          wx = ece_obj$norm[1, 1],
          wy = ece_obj$norm[2, 2],
          wxy = ece_obj$norm[1, 2],
          rho = cov2cor(ece_obj$cov)[1, 2]
        )
      )
    })
    return(
      list(
        wx = map_dbl(sim_results_inner, "wx") %>% mean(),
        wy = map_dbl(sim_results_inner, "wy") %>% mean(),
        wxy = map_dbl(sim_results_inner, "wxy") %>% mean(),
        rho = map_dbl(sim_results_inner, "rho") %>% var()
      )
    )
  })

  # save simulations
  wx_sims <- map_dbl(sim_results, "wx")
  wy_sims <- map_dbl(sim_results, "wy")
  wxy_sims <- map_dbl(sim_results, "wxy")
  rho_sims <- map_dbl(sim_results, "rho")

  # get confidence intervals
  wx_ci <- t.test(wx_sims, conf.level = 0.99)$conf.int
  wy_ci <- t.test(wy_sims, conf.level = 0.99)$conf.int
  wxy_ci <- t.test(wxy_sims, conf.level = 0.99)$conf.int
  rho_ci <- t.test(rho_sims, conf.level = 0.99)$conf.int

  # get expected values
  ece_obj <- equiv.cov(params$h, return.norm = TRUE)
  wx_true <- ece_obj$norm[1, 1]
  wy_true <- ece_obj$norm[2, 2]
  wxy_true <- ece_obj$norm[1, 2]
  rho_true <- ece.cor.asymp(params)

  # Test
  test_that("norms are properly estimated (with noise)", {
    expect_true((wx_ci[1] < wx_true) & (wx_true < wx_ci[2]))
    expect_true((wy_ci[1] < wy_true) & (wy_true < wy_ci[2]))
    expect_true((wxy_ci[1] < wxy_true) & (wxy_true < wxy_ci[2]))
  })
  test_that("variance of correlation estimate converges to asymptotic estimate", {
    expect_true((rho_ci[1] < rho_true) & (rho_true < rho_ci[2]))
  })
}
