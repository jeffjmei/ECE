generate_data <- function(params) {
  e <- mvrnorm(params$n, c(0, 0), params$S)
  X <- params$h + e
  colnames(X) <- paste0("x", 1:nrow(params$S))
  return(X)
}
