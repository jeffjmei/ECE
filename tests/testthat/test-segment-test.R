test_that("p-value is a valid probability (naive and corrected)", {
  params <- scenario(2, n = 500, r = 0, L = 4)

  results <- map_lgl(1:50, ~ {
    X <- generate_data(params)
    p_naive <- segment_test(X, params, penalty = "Oracle", correct_df = FALSE)$p.value
    p_correct <- segment_test(X, params, penalty = "Oracle", correct_df = TRUE)$p.value
    all(p_naive >= 0, p_naive <= 1, p_correct >= 0, p_correct <= 1)
  })

  expect_true(all(results))
})

test_that("corrected-df type I error rate is roughly controlled under the null", {
  params <- scenario(2, n = 1000, r = 0, L = 4)

  set.seed(123)
  pvals <- map_dbl(1:500, ~ {
    X <- generate_data(params)
    segment_test(X, params, penalty = "Oracle", correct_df = TRUE)$p.value
  })

  type1_error_rate <- mean(pvals < 0.05)
  expect_lt(abs(type1_error_rate - 0.05), 0.03)
})

test_that("oracle K matches the true change-point count", {
  params <- scenario(5, n = 500, r = 0, L = 4, prob = 0.10)
  X <- generate_data(params)

  result <- segment_test(X, params, penalty = "Oracle")
  expect_equal(result$K, length(params$scenario_param$cp))
})

test_that("AIC and BIC penalties run without error and return valid p-values", {
  params <- scenario(2, n = 500, r = 0, L = 4)
  X <- generate_data(params)

  p_aic <- segment_test(X, params, penalty = "AIC")$p.value
  p_bic <- segment_test(X, params, penalty = "BIC")$p.value

  expect_true(p_aic >= 0 && p_aic <= 1)
  expect_true(p_bic >= 0 && p_bic <= 1)
})

test_that("segment_test requires a 2-column matrix", {
  params <- scenario(2, n = 200, p = 3, r = 0, L = 4)
  X <- generate_data(params)
  expect_error(segment_test(X, params), "2-column matrix")
})

test_that("segment_pval returns a symmetric p x p matrix with zero diagonal", {
  params <- scenario(2, n = 200, p = 4, r = 0, L = 4)
  X <- generate_data(params)

  pval_mat <- segment_pval(X, params, penalty = "Oracle")

  expect_equal(dim(pval_mat), c(4, 4))
  expect_equal(pval_mat, t(pval_mat))
  expect_true(all(diag(pval_mat) == 0))
})
