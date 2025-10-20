# TODO: add docstring
ece.test <- function(X, Y = NULL, L = 2, alpha = 0.05) {
  X <- as.matrix(X)
  if (!is.null(Y)) {
    Y <- as.matrix(Y)
    if (nrow(X) != nrow(Y)) stop("X and Y must have same number of rows")
    X <- cbind(X, Y)
  } else {
    X <- X
  }

  # extract variables
  n <- nrow(X)
  ece_obj <- equiv.cov(X, return.norm = TRUE)
  z <- qnorm(1 - alpha / 2)
  sx <- sqrt(ece_obj$cov[1, 1])
  sy <- sqrt(ece_obj$cov[2, 2])
  sxy <- ece_obj$cov[1, 2]

  v <- var_corr_est(n, ece_obj)
  se <- sqrt(v)

  # return values
  rxy <- sxy / (sx * sy) # point estimate
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
