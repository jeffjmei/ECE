# Check if Estimate is Right
params <- scenario(10, sxy = 0, sx = 1, sy = 1, n = 1000)
rho_var <- ece.cor.asymp(params)
# TODO: generalize to any scenario

# Simulate
rho_var_sims <- map_dbl(1:1000, ~ {
  rho_sims <- map_dbl(1:10, ~ {
    X <- generate_data(params)
    rho <- cov2cor(equiv.cov(X))[1, 2]
    return(rho)
  })
  return(var(rho_sims))
})

# Test
ci <- t.test(rho_var_sims)$conf.int
test_that("Theoretical Value Matches Empirical", {
  expect_true((ci[1] < rho_var) & (rho_var < ci[2]))
})
