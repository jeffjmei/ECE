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

#' Block Sum Constraint Matrix
#'
#' Builds the constraint matrix \eqn{G} whose rows encode \eqn{g(\ell) = 0}
#' for \eqn{\ell = L, \ldots, \lfloor n/2 \rfloor}. Columns correspond to
#' the unknowns \eqn{(a_0, a_1, \ldots, a_{\lfloor n/2 \rfloor})}.
#'
#' @param n A positive integer giving the series length.
#' @param L A positive integer giving the minimum segment length.
#'
#' @return A numeric matrix of size \eqn{(\lfloor n/2 \rfloor - L + 1) \times (\lfloor n/2 \rfloor + 1)}.
#'
#' @examples
#' constraint_matrix(10, 2)
#'
#' @export
constraint_matrix <- function(n, L) {
  half <- floor(n / 2)
  G <- matrix(0, nrow = half - L + 1, ncol = half + 1)
  for (row in seq_len(nrow(G))) {
    l          <- row + L - 1
    G[row, 1]  <- l                              # coefficient of a_0 is l
    G[row, -1] <- pmax(0, 2 * (l - 1:half))     # coefficient of a_i is 2(l - i)
  }
  G
}

#' Null Space of a Matrix
#'
#' Computes the null space of a matrix via SVD.
#'
#' @param G A numeric matrix.
#' @param tol Tolerance for identifying zero singular values.
#'
#' @return A matrix whose columns form a basis for the null space of \eqn{G}.
#'
#' @examples
#' null_space(constraint_matrix(10, 2))
#'
#' @export
null_space <- function(G, tol = 1e-10) {
  # Apply Singular Value Decomposition
  sv <- svd(G, nu = 0, nv = ncol(G))

  # Find Nonzero Singular Values
  rank_G <- sum(sv$d > tol)

  # Nullspace = Columns Associated with Trivial Singular Values
  ns <- sv$v[, (rank_G + 1):ncol(G), drop = FALSE]
}

#' Expand a Symmetric Half-Vector to a Full Circulant First Row
#'
#' Given the \eqn{\lfloor n/2 \rfloor + 1} unique entries
#' \eqn{(a_0, a_1, \ldots, a_{\lfloor n/2 \rfloor})} of a symmetric circulant,
#' reconstructs the full length-\eqn{n} first row using \eqn{a_{n-i} = a_i}.
#'
#' @param a_half A numeric vector of length \eqn{\lfloor n/2 \rfloor + 1}.
#' @param n A positive integer giving the full vector length.
#'
#' @return A numeric vector of length \eqn{n}.
#'
#' @examples
#' expand_sym(c(1, -1, 0.5), 4)
#'
#' @export
expand_sym <- function(a_half, n) {
  c(a_half, rev(a_half[2:(n - length(a_half) + 1)]))
}

#' ECE Circulant First Row
#'
#' Computes the first row of the ECE matrix \eqn{A = \frac{1}{n}\operatorname{circ}(a)}
#' by solving the unbiasedness constraints via the null space of the constraint matrix.
#' Returns the normalized half-vector expanded to the full first row.
#'
#' For \eqn{L = 2} the solution is unique. For \eqn{L > 2} the null space may be
#' higher-dimensional; this function returns the first basis vector with \eqn{a_0 \neq 0}.
#'
#' @param n A positive integer giving the series length.
#' @param L A positive integer giving the minimum segment length.
#'
#' @return A numeric vector of length \eqn{n}, the first row of \eqn{n \cdot A}.
#'
#' @examples
#' ece_first_row(10, 2)
#'
#' @export
ece_first_row <- function(n, L) {
  G <- constraint_matrix(n, L)
  ns <- null_space(G)
  keep <- apply(ns, 2, function(v) abs(v[1]) > 1e-10)
  a_half <- ns[, keep, drop = FALSE][, 1]
  a_half <- a_half / a_half[1]
  expand_sym(a_half, n)
}

