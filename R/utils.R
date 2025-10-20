# TODO: add roxygen docstring
segmented_mean <- function(data, change_points) {
  # Ensure inputs are valid
  if (!is.numeric(data)) stop("Data must be a numeric vector")
  if (!is.numeric(change_points)) stop("Change points must be numeric indices")

  # Add start and end points
  cpts <- c(0, change_points, length(data))
  seg_mean <- numeric(length(data))

  # Loop through each segment and assign mean
  for (i in seq_len(length(cpts) - 1)) {
    idx <- (cpts[i] + 1):cpts[i + 1]
    seg_mean[idx] <- mean(data[idx], na.rm = TRUE)
  }

  return(seg_mean)
}

#' Detect Change Points in a Numeric Vector
#'
#' Applies change point detection to a numeric vector using the `cpt.mean()` function from the `changepoint` package.
#'
#' @param x A numeric vector to detect change points in.
#' @param method A character string specifying the segmentation method. Default is `"PELT"`. Other options include `"AMOC"` and `"BinSeg"`.
#' @param penalty A character string specifying the penalty method. Options include `"BIC"`, `"AIC"`, `"Manual"`, etc. Default is `"BIC"`.
#' @param minseglen An integer specifying the minimum segment length. Default is `2`.
#' @param pen.value A numeric value for the penalty, used only if `penalty = "Manual"`. Default is `NULL`.
#'
#' @return An object of class `cpt` representing the estimated change points.
#'
#' @details This function is a convenience wrapper around `changepoint::cpt.mean()` that allows optional manual penalty specification. It is primarily used for detecting mean shifts in time series or sequences.
#'
#' @seealso [changepoint::cpt.mean()]
#'
#' @examples
#' if (requireNamespace("changepoint", quietly = TRUE)) {
#'   x <- c(rep(0, 50), rep(5, 50), rep(0, 50))
#'   cp <- get_cp(x)
#'   changepoint::cpts(cp)
#' }
#'
#' @export
get_cp <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  if (penalty == "Manual") {
    cp <- changepoint::cpt.mean(
      x,
      method = method,
      penalty = penalty,
      minseglen = minseglen,
      pen.value = pen.value
    )
  } else {
    cp <- changepoint::cpt.mean(
      x,
      method = method,
      penalty = penalty,
      minseglen = minseglen,
    )
  }
  return(cp)
}

find_cp <- function(h) {
  if (is.vector(h)) {
    which(h[-1] != h[-length(h)])
  } else if (is.matrix(h)) {
    change_exists <- (h[-1, ] != h[-nrow(h), ])
    which(apply(change_exists, 1, any))
  }
}

segment_mean_vec <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  if (penalty == "Manual") {
    cp <- changepoint::cpts(get_cp(x, method, penalty, minseglen, pen.value = pen.value))
  } else {
    cp <- changepoint::cpts(get_cp(x, method, penalty, minseglen))
  }
  segmented_mean(x, cp)
}

segment_mean_mat <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  map(as.data.frame(x), segment_mean_vec) %>%
    do.call(what = cbind)
}

segment_mean <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  if (is.vector(x)) {
    segment_mean_vec(x, method = method, penalty = penalty, minseglen = minseglen, pen.value = pen.value)
  } else if (is.matrix(x) || is.data.frame(x)) {
    segment_mean_mat(x, method = method, penalty = penalty, minseglen = minseglen, pen.value = pen.value)
  }
}

smooth_mean_vec <- function(x, method = "loess", ...) {
  if (method == "loess") {
    default_args <- list(span = 0.3, family = "symmetric")
    user_args <- list(...)
    loess_args <- modifyList(default_args, user_args)
    loess_args$formula <- x ~ seq_along(x)
    do.call(loess, loess_args)$fitted
  } else if (method == "exponential") {
    forecast::ses(ts(x), ...)$fitted
  } else {
    stop("Unknown smoothing method.")
  }
}

smooth_mean_mat <- function(X, method = "loess", ...) {
  X_df <- as.data.frame(X)
  smoothed_list <- lapply(X_df, smooth_mean_vec, method = method, ...)
  do.call(cbind, smoothed_list)
}

smooth_mean <- function(X, method = "loess", ...) {
  if (is.vector(X)) {
    smooth_mean_vec(X, method = method, ...)
  } else if (is.matrix(X) || is.data.frame(X)) {
    smooth_mean_mat(X, method = method, ...)
  } else {
    stop("Unsupported input type. Must be vector, matrix, or data.frame.")
  }
}

winsorize <- function(x, alpha = 0.01) {
  q <- quantile(x, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

detrend <- function(X) {
  (X - rotate(X))[-nrow(X), ] / 2
}

oracle_vec <- function(x, h) {
  # identify change points in mean vector
  cp <- c(0, which((h - rotate(h)) != 0), length(x)) %>%
    unique() %>%
    sort()
  map(seq_along(cp[-1]), ~ {
    rep(
      mean(x[(cp[.x] + 1):cp[.x + 1]]), # segment average
      cp[.x + 1] - cp[.x] # number of repetitions
    )
  }) %>% unlist()
}

segment_mean_oracle <- function(X, params) {
  # apply to every column in matrix/df
  map2_dfc(
    as.data.frame(X),
    as.data.frame(params$h),
    oracle_vec
  ) %>% as.matrix()
}

cor_pval <- function(X) {
  # gets a matrix of p-values for various methods
  p <- ncol(X)
  pval_mat <- matrix(NA, p, p)
  colnames(pval_mat) <- rownames(pval_mat) <- colnames(X)
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      pval <- cor.test(X[, i], X[, j])$p.value
      pval_mat[i, j] <- pval
      pval_mat[j, i] <- pval
    }
  }
  diag(pval_mat) <- 0
  pval_mat
}

ece_pval <- function(X) {
  p <- ncol(X)
  pval_mat <- matrix(NA, p, p)
  colnames(pval_mat) <- rownames(pval_mat) <- colnames(X)
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      pval <- ece.test(X[, i], X[, j])$p.value
      pval_mat[i, j] <- pval
      pval_mat[j, i] <- pval
    }
  }
  diag(pval_mat) <- 0
  pval_mat
}
