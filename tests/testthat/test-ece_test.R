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
