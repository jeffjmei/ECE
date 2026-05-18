cov_cs <- function(r, p) {
  S <- diag(p)
  S[upper.tri(S)] <- S[lower.tri(S)] <- r
  return(S)
}

cov_ar1 <- function(r, p) {
  r^abs(outer(1:p, 1:p, "-"))
}

make_cov <- function(r, p, cov_type = "compound") {
  switch(cov_type,
    compound = cov_cs(r, p),
    ar1      = cov_ar1(r, p),
    stop("Unknown cov_type: ", cov_type)
  )
}
