#' Construct a Circulant Matrix
#'
#' Constructs an \eqn{n \times n} circulant matrix from its first row.
#'
#' @param a A numeric vector of length \eqn{n} giving the first row.
#'
#' @return An \eqn{n \times n} circulant matrix.
#'
#' @examples
#' circ(c(2, -1, 0, -1))
#'
#' @export
circ <- function(a) {
  n <- length(a)
  matrix(a[mod_idx(col(matrix(0, n, n)) - row(matrix(0, n, n)) + 1, n)], n, n)
}

