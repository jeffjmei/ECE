#' @export
segment_test <- function(X, params, penalty = c("Oracle", "BIC", "AIC"), correct_df = FALSE) {
  penalty <- match.arg(penalty)
  if (ncol(X) != 2) stop("segment_test requires a 2-column matrix")
  n <- nrow(X)

  if (penalty == "Oracle") {
    K <- length(params$scenario_param$cp)
    fitted <- segment_mean_oracle(X, params)
  } else {
    cp <- lapply(1:2, function(j) changepoint::cpts(get_cp(X[, j], penalty = penalty)))
    K <- length(Reduce(union, cp))
    fitted <- do.call(cbind, Map(segmented_mean, as.data.frame(X), cp))
  }

  resid <- X - fitted
  r <- cor(resid[, 1], resid[, 2])

  df <- if (correct_df) n - K - 2 else n - 2
  t_stat <- r * sqrt(df) / sqrt(1 - r^2)
  p_value <- 2 * pt(-abs(t_stat), df = df)

  list(statistic = t_stat, p.value = p_value, df = df, estimate = r, K = K)
}

#' @export
segment_pval <- function(X, params, penalty = c("Oracle", "BIC", "AIC"), correct_df = FALSE) {
  penalty <- match.arg(penalty)
  p <- ncol(X)
  pval_mat <- matrix(NA, p, p)
  colnames(pval_mat) <- rownames(pval_mat) <- colnames(X)
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      params_ij <- modifyList(params, list(h = params$h[, c(i, j), drop = FALSE]))
      pval <- segment_test(cbind(X[, i], X[, j]), params_ij, penalty = penalty, correct_df = correct_df)$p.value
      pval_mat[i, j] <- pval
      pval_mat[j, i] <- pval
    }
  }
  diag(pval_mat) <- 0
  pval_mat
}
