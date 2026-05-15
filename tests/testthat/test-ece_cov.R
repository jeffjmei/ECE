test_that("ece.cov returns a numeric matrix for matrix input", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  out <- ece.cov(X)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5, 5))
  expect_true(is.numeric(out))
})

test_that("ece.complexity returns a numeric matrix for matrix input", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 5)
  out <- ece.complexity(X)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5, 5))
  expect_true(is.numeric(out))
})

test_that("ece.cov and ece.complexity handle vector input", {
  x <- c(-1, -1, 1, 1)
  y <- c(1, 1, -1, -1)
  expect_equal(ece.cov(x, y), 0)
  expect_equal(ece.complexity(x, y), -2)
})

test_that("ece.cov accepts matrix inputs", {
  n <- 10
  S_ind <- matrix(c(2, 0, 0, 3), nrow = 2)
  e <- MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = S_ind)
  hx <- c(rep(-1, n / 2), rep(1, n / 2))
  hy <- c(rep(1, n / 2), rep(-1, n / 2))
  X <- cbind(hx + e[, 1], hy + e[, 2])
  expect_true(is.matrix(ece.cov(X)))
})

test_that("ece.complexity outputs correct values", {
  n_scenarios <- 11
  misspecified_scenarios <- c(8, 9)
  scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
  for (s in scenario_list) {
    params <- scenario(s, sxy = 0, sx = 1, sy = 1, n = 1000)
    n <- params$n
    h <- params$h

    expect_equal(
      ece.complexity(h[, 1], h[, 2]),
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
    S_hat <- ece.cov(X)
    list(
      sx  = sqrt(S_hat[1, 1]),
      sy  = sqrt(S_hat[2, 2]),
      sxy = S_hat[1, 2]
    )
  })
  sx_sim  <- map_dbl(var_sims, "sx")
  sy_sim  <- map_dbl(var_sims, "sy")
  sxy_sim <- map_dbl(var_sims, "sxy")

  sx_ci  <- t.test(sx_sim,  conf.level = 0.99)$conf.int
  sy_ci  <- t.test(sy_sim,  conf.level = 0.99)$conf.int
  sxy_ci <- t.test(sxy_sim, conf.level = 0.99)$conf.int

  sx  <- sqrt(params$S[1, 1])
  sy  <- sqrt(params$S[2, 2])
  sxy <- params$S[1, 2]

  expect_true((sx_ci[1]  < sx)  & (sx  < sx_ci[2]))
  expect_true((sy_ci[1]  < sy)  & (sy  < sy_ci[2]))
  expect_true((sxy_ci[1] < sxy) & (sxy < sxy_ci[2]))
})

# TEST: ece.complexity yields expected value (no noise)
n_scenarios <- 11
misspecified_scenarios <- c(8, 9)
scenario_list <- setdiff(1:n_scenarios, misspecified_scenarios)
for (scenario_num in scenario_list) {
  params <- scenario(scenario_num, sxy = 0, sx = 1, sy = 1, n = 1000)
  h <- params$h
  n <- params$n

  w_mat_est <- ece.complexity(h)

  wx  <- lag_diff(h[, 1]) / n
  wy  <- lag_diff(h[, 2]) / n
  wxy <- lag_diff(h[, 1], h[, 2]) / n
  w_mat_exp <- matrix(c(wx, wxy, wxy, wy), nrow = 2)

  test_that(sprintf("ECE complexity is wxy (Scenario %d) without noise", scenario_num), {
    expect_equal(w_mat_est, w_mat_exp)
  })
}

# TEST: ece.complexity yields expected value (with noise)
for (scenario_num in scenario_list) {
  params <- scenario(scenario_num, sxy = 0, sx = 1, sy = 1, n = 1000)
  sim_results <- map(1:1000, ~ {
    sim_results_inner <- map(1:10, ~ {
      X <- generate_data(params)
      list(
        wx  = ece.complexity(X)[1, 1],
        wy  = ece.complexity(X)[2, 2],
        wxy = ece.complexity(X)[1, 2],
        rho = cov2cor(ece.cov(X))[1, 2]
      )
    })
    list(
      wx  = map_dbl(sim_results_inner, "wx")  %>% mean(),
      wy  = map_dbl(sim_results_inner, "wy")  %>% mean(),
      wxy = map_dbl(sim_results_inner, "wxy") %>% mean(),
      rho = map_dbl(sim_results_inner, "rho") %>% var()
    )
  })

  wx_sims  <- map_dbl(sim_results, "wx")
  wy_sims  <- map_dbl(sim_results, "wy")
  wxy_sims <- map_dbl(sim_results, "wxy")
  rho_sims <- map_dbl(sim_results, "rho")

  wx_ci  <- t.test(wx_sims,  conf.level = 0.99)$conf.int
  wy_ci  <- t.test(wy_sims,  conf.level = 0.99)$conf.int
  wxy_ci <- t.test(wxy_sims, conf.level = 0.99)$conf.int
  rho_ci <- t.test(rho_sims, conf.level = 0.99)$conf.int

  cpx_h  <- ece.complexity(params$h)
  wx_true  <- cpx_h[1, 1]
  wy_true  <- cpx_h[2, 2]
  wxy_true <- cpx_h[1, 2]
  rho_true <- ece.cor.asymp(params)

  test_that("complexity is properly estimated (with noise)", {
    expect_true((wx_ci[1]  < wx_true)  & (wx_true  < wx_ci[2]))
    expect_true((wy_ci[1]  < wy_true)  & (wy_true  < wy_ci[2]))
    expect_true((wxy_ci[1] < wxy_true) & (wxy_true < wxy_ci[2]))
  })
  test_that("variance of correlation estimate converges to asymptotic estimate", {
    expect_true((rho_ci[1] < rho_true) & (rho_true < rho_ci[2]))
  })
}
