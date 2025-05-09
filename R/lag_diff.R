#' Lagged Difference or Cross-Lagged Product
#'
#' Computes the squared difference between a vector and its lagged version, or the cross-product with a second lagged vector.
#'
#' @param X A numeric vector.
#' @param Y (Optional) A second numeric vector of the same length as `X`. If provided, computes a lagged cross-product instead of a squared difference.
#' @param k An integer indicating the lag (default is 1). Lagging is done via circular rotation.
#'
#' @return A single numeric value representing the sum of squared differences (if `Y` is `NULL`) or the sum of lagged cross-products (if `Y` is provided).
#'
#' @examples
#' X <- 1:3
#' lag_diff(X) # 6
#' lag_diff(X, k = 2) # 6
#'
#' Y <- c(4, 5, 6)
#' lag_diff(X, Y = Y) # 27
#'
#' @export
lag_diff <- function(X, Y = NULL, k = 1) {
  # Validate inputs
  if (!is.numeric(X)) stop("X must be numeric")
  if (!is.null(Y)) {
    if (!is.numeric(Y)) stop("Y must be numeric")
    if (length(X) != length(Y)) stop("X and Y must be the same length")
  }
  if (k < 0 || k >= length(X)) stop("k must be between 0 and length(X) - 1")

  # calculate lagged difference
  if (is.null(Y)) {
    sum((X - rotate(X, k))^2)
  } else {
    sum(
      (X - rotate(X, k)) *
        (Y - rotate(Y, k))
    )
  }
}
