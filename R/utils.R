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
    cp <- cpt.mean(
      x,
      method = method,
      penalty = penalty,
      minseglen = minseglen,
      pen.value = pen.value
    )
  } else {
    cp <- cpt.mean(
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
    cp <- cpts(get_cp(x, method, penalty, minseglen, pen.value = pen.value))
  } else {
    cp <- cpts(get_cp(x, method, penalty, minseglen))
  }
  segmented_mean(x, cp)
}

segment_mean_mat <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  map(as.data.frame(x), segment_mean_vec) %>%
    do.call(what = cbind)
}

segment_mean <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  if (is.vector(x)) {
    segment_mean_vec(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL)
  } else if (is.matrix(x)) {
    segment_mean_mat(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL)
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
