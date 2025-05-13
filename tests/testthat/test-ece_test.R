test_that("p-value and confidence interval align across simulations", {
  params <- scenario(10, n = 1000, sxy = 0, sx = 2, sy = 3)

  results <- map_lgl(1:100, ~ {
    X <- generate_data(params)
    result <- ece.test(X[, 1], X[, 2])

    ci <- result$conf.int
    pval <- result$p.value

    # Returns TRUE if the p-value and CI are consistent, FALSE otherwise
    (pval > 0.05 && ci[1] < 0 && ci[2] > 0) ||
      (pval <= 0.05 && (ci[1] > 0 || ci[2] < 0))
  })

  # Fail if too many inconsistencies (allowing for small randomness margin)
  expect_equal(mean(results), 1)
})

test_that("estimator is unbiased under null", {
  params <- scenario(10, n = 1000, sxy = 0, sx = 2, sy = 3)

  # simulate ece correlation estimate
  rxy_est_sim <- map_dbl(1:1000, ~ {
    X <- generate_data(params)
    ece.test(X[, 1], X[, 2])$estimate
  })

  # test if true correlation is within ece.ci
  ci <- t.test(rxy_est_sim, conf.level = 0.99)$conf.int
  rxy <- params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2])
  expect_true((ci[1] < rxy) & (rxy < ci[2]))
})
