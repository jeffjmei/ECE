test_that("find_cp identifies change points", {
  h <- c(0, 0, 1, 1)
  expect_equal(find_cp(h), 2)

  h <- c(0, 0, 0, 0)
  expect_equal(find_cp(h), integer(0))
})

test_that("segmented_mean correctly segments mean", {
  hx <- c(0, 0, 1, 1)
  hy <- c(0, 0, 1, 1)
  ex <- rnorm(length(hx))
  ey <- rnorm(length(hy))

  h <- cbind(hx, hy)
  e <- cbind(ex, ey)
  X <- h + e

  result <- map2(
    .x = as.data.frame(X),
    .y = as.data.frame(h),
    ~ segmented_mean(.x, find_cp(.y))
  ) %>% do.call(what = cbind)

  target <- cbind(
    hx = c(
      rep(mean(X[1:2, 1]), length(1:2)),
      rep(mean(X[3:4, 1]), length(3:4))
    ),
    hy = c(
      rep(mean(X[1:2, 2]), length(1:2)),
      rep(mean(X[3:4, 2]), length(3:4))
    )
  )
  expect_equal(result, target)
})
