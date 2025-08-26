generate_data <- function(params) {
  if (params$scenario < 13) {
    e <- MASS::mvrnorm(params$n, c(0, 0), params$S)
    X <- params$h + e
    colnames(X) <- paste0("x", 1:nrow(params$S))
    return(X)
  } else if (params$scenario == 13) {
    e <- MASS::mvrnorm(params$n, c(0, 0), params$S)
    ex <- cumsum(e[, 1])
    ey <- cumsum(e[, 2])
    X <- cbind(ex, ey)
    colnames(X) <- paste0("x", 1:nrow(params$S))
    return(X)
  }
}
