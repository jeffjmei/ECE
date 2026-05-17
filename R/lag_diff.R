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
#' lag_diff(x) # 6
#' lag_diff(x, k = 2) # 6
#'
#' y <- c(4, 5, 6)
#' lag_diff(x, y) # 27
#'
#' @export
lag_diff <- function(x, y = x, k = 1) {
  sum((x - rotate(x, k)) * (y - rotate(y, k)))
}

#' Element-wise Lag Term
#'
#' Computes the element-wise contributions whose sum equals
#' \eqn{2 \cdot \mathrm{lag\_diff}(x, y, k=1) - \mathrm{lag\_diff}(x, y, k=2)}.
#' Each element has mean zero under independence, making this the building
#' block for the bootstrap multiplier test.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as \code{x}. Defaults to \code{x}.
#'
#' @return A numeric vector of the same length as \code{x}.
#'
#' @seealso [lag_diff()], [lag_terms()]
#'
#' @examples
#' x <- rnorm(50); y <- rnorm(50)
#' sum(lag_term(x, y)) # equals 2*lag_diff(x,y,k=1) - lag_diff(x,y,k=2)
#'
#' @export
lag_term <- function(x, y = x) {
  ((x - rotate(x, 1)) * (y - rotate(y, 1)) +
    (rotate(x, 1) - rotate(x, 2)) * (rotate(y, 1) - rotate(y, 2)) -
    (x - rotate(x, 2)) * (y - rotate(y, 2)))
}

#' Pairwise Lag Terms for a Multivariate Series
#'
#' Computes [lag_term()] for all \eqn{\binom{p}{2}} column pairs of a matrix,
#' returning the results as a matrix of columns.
#'
#' @param x An \eqn{n \times p} numeric matrix.
#'
#' @return An \eqn{n \times \binom{p}{2}} numeric matrix. Columns follow the
#'   ordering of [combn(p, 2)]: pairs (1,2), (1,3), \ldots, (p-1, p).
#'
#' @seealso [lag_term()], [split_indep()]
#'
#' @examples
#' X <- matrix(rnorm(300), ncol = 3)
#' lag_terms(X) # 100 x 3 matrix of pairwise lag terms
#'
#' @export
lag_terms <- function(x) {
  p <- ncol(x)
  idx <- combn(p, 2)
  do.call(cbind, map2(idx[1, ], idx[2, ], \(i, j) lag_term(x[, i], x[, j])))
}

#' Split into Independent Subsequences
#'
#' Splits the rows of a matrix into \code{stride} non-overlapping subsequences
#' by selecting every \code{stride}-th row. Because [lag_term()] values within
#' distance \code{stride} are dependent, this ensures each subsequence contains
#' approximately independent observations.
#'
#' @param x A numeric vector or matrix.
#' @param stride A positive integer giving the spacing between selected rows
#'   (default 3, matching the dependence range of [lag_term()]).
#'
#' @return A list of \code{stride} matrices. Subsequences may differ in length
#'   by one row when \code{nrow(x)} is not divisible by \code{stride}.
#'
#' @seealso [lag_terms()]
split_indep <- function(x, stride = 3) {
  x <- as.matrix(x)
  map(1:stride, \(r) x[seq(r, nrow(x), stride), , drop = FALSE])
}
