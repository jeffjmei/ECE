#' Rotate Elements to the Right
#'
#' Rotates a vector or matrix downwards (i.e., toward the end) by `k` positions. For a vector, the last `k` elements move to the front. For a matrix, rows are rotated downward.
#'
#' @param X A numeric vector or matrix.
#' @param k An integer specifying how many positions to rotate. Defaults to 1.
#'
#' @return A rotated vector or matrix of the same dimensions as `X`.
#'
#' @examples
#' rotate(1:3, k = 1) # returns 3 1 2
#'
#' X <- matrix(c(1:3, 1:3), ncol = 2)
#' rotate(X, k = 1) # rotates rows downward
#'
#' @export
rotate <- function(X, k = 1) {
  if (is.matrix(X)) {
    n <- nrow(X)
    if (k %% n == 0) {
      return(X)
    }
    return(X[c((k + 1):n, 1:k), ])
  } else {
    n <- length(X)
    if (k %% n == 0) {
      return(X)
    }
    return(X[c((k + 1):n, 1:k)])
  }
}
