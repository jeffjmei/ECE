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

# TODO: add roxygen docstring
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

segment_mean <- function(x, method = "PELT", penalty = "BIC", minseglen = 2, pen.value = NULL) {
  if (penalty == "Manual") {
    cp <- cpts(get_cp(x, method, penalty, minseglen, pen.value = pen.value))
  } else {
    cp <- cpts(get_cp(x, method, penalty, minseglen))
  }
  segmented_mean(x, cp)
}

winsorize <- function(x, alpha = 0.01) {
  q <- quantile(x, probs = c(alpha, 1 - alpha), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}
