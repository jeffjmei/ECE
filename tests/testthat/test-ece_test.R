test_that("p-value is a valid probability", {
  params <- scenario(10, n = 1000, sxy = 0, sx = 2, sy = 3)

  results <- map_lgl(1:100, ~ {
    X <- generate_data(params)
    pval <- ece.test(X[, 1:2])$p.value
    pval >= 0 && pval <= 1
  })

  expect_true(all(results))
})

test_that("estimator is unbiased under null", {
  params <- scenario(10, n = 1000, sxy = 0, sx = 2, sy = 3)

  set.seed(123)
  rxy_est_sim <- map_dbl(1:1000, ~ {
    X <- generate_data(params)
    ece.test(X[, 1:2])$estimate
  })

  ci <- t.test(rxy_est_sim, conf.level = 0.99)$conf.int
  rxy <- params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2])
  expect_true((ci[1] < rxy) & (rxy < ci[2]))
})

test_that("type I error rate is controlled", {
  params <- scenario(10, n = 1000, sxy = 0, sx = 2, sy = 3)

  set.seed(123)
  pvals <- map_dbl(1:1000, ~ {
    X <- generate_data(params)
    ece.test(X[, 1:2])$p.value
  })

  type1_error_rate <- mean(pvals < 0.05)
  expect_lt(abs(type1_error_rate - 0.05), 0.015)
})
