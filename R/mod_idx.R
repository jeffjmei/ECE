#' Circular Modular Index
#'
#' Computes a modular (wrap-around) index in the range `1` to `n`, useful for circular embeddings.
#'
#' @param i An integer or vector of integers indicating the original index or indices.
#' @param n A positive integer indicating the modulus (i.e., total number of positions).
#'
#' @return An integer or vector of integers, each in the range `1` to `n`, representing the modularized index.
#'
#' @examples
#' mod_idx(10, 10) # 10
#' mod_idx(11, 10) # 1
#' mod_idx(c(0, -1, 21), 10) # 10, 9, 1
#'
#' @export
mod_idx <- function(i, n) {
  (i - 1) %% n + 1
}
