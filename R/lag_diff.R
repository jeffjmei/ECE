#' Lagged Difference or Cross-Lagged Product
#'
#' Computes the sum of squared lagged differences of a vector, or the sum of
#' lagged cross-products between two vectors.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as `x`. Defaults to `x`.
#' @param k An integer indicating the lag (default is 1). Lagging is done via circular rotation.
#'
#' @return A single numeric value.
#'
#' @examples
#' x <- 1:3
#' lag_diff(x)        # 6
#' lag_diff(x, k = 2) # 6
#'
#' y <- c(4, 5, 6)
#' lag_diff(x, y)     # 27
#'
#' @export
lag_diff <- function(x, y = x, k = 1) {
  sum((x - rotate(x, k)) * (y - rotate(y, k)))
}
