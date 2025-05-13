# TODO: add docstring
ece.test <- function(X, Y, L = 2, alpha = 0.05) {
  # TEST: len(X) != len(Y)

  # extract variables
  n <- length(X)
  z <- qnorm(1 - alpha / 2)
  ece_obj <- equiv.cov(cbind(X, Y), L = 2, return.norm = TRUE)
  sx <- sqrt(ece_obj$cov[1, 1])
  sy <- sqrt(ece_obj$cov[2, 2])
  sxy <- ece_obj$cov[1, 2]
  wx <- ece_obj$norm[1, 1]
  wy <- ece_obj$norm[2, 2]
  wxy <- ece_obj$norm[1, 2]

  # asymptotic variance (rho=0)
  k22 <- 1 # TODO: true for gaussian
  rxy <- 0 # TODO: generalize beyond null hypothesis
  v <- (1 / n) * (
    k22 +
      (5 / 2) +
      (3 / 2) * rxy^2 +
      sx^(-2) * wx +
      sy^(-2) * wy +
      sx^(-1) * sy^(-1) * 2 * rxy * wxy
  )

  # return values
  se <- sqrt(v)
  rxy <- cov2cor(ece_obj$cov)[1, 2] # point estimate
  pval <- 2 * (1 - pnorm(abs(rxy / se))) # p-value
  ci <- c(rxy - z * se, rxy + z * se) # confidence interval
  return(
    list(
      estimate = rxy,
      p.value = pval,
      conf.int = ci
    )
  )
}
