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

#' Circulant Shift Matrix
#'
#' Constructs the \eqn{n \times n} circulant shift matrix \eqn{C_k}, where
#' entry \eqn{(i,j)} is 1 if \eqn{j - i \equiv k \pmod{n}} and 0 otherwise.
#'
#' @param n A positive integer giving the matrix dimension.
#' @param k A non-negative integer giving the shift.
#'
#' @return An \eqn{n \times n} circulant shift matrix.
#'
#' @examples
#' circ_shift(4, 1)
#'
#' @export
circ_shift <- function(n, k) {
  e_k <- as.numeric(seq_len(n) - 1 == k %% n)
  circ(e_k)
}

#' Lag Matrix
#'
#' Constructs the \eqn{n \times n} matrix \eqn{A_k = 2I - C_k - C_k^T},
#' the matrix form of the \eqn{k}-lagged circular difference operator,
#' so that \eqn{T_k(X, Y) = X^T A_k Y}.
#'
#' @param n A positive integer giving the matrix dimension.
#' @param k A positive integer giving the lag.
#'
#' @return An \eqn{n \times n} symmetric matrix.
#'
#' @examples
#' lag_mat(4, 1)
#'
#' @export
lag_mat <- function(n, k) {
  Ck <- circ_shift(n, k)
  2 * diag(n) - Ck - t(Ck)
}

