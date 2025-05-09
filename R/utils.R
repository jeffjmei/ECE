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
