scenario1 <- function(s12 = 0, n = 100, signal = 1) {
  # No Change Point
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- rep(0, n)
  h2 <- rep(0, n)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 1, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario2 <- function(s12 = 0, n = 100, signal = 1) {
  # Single Change Point (Unison)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(-1 / 2, n / 2), rep(1 / 2, n / 2))
  h2 <- c(rep(-1 / 2, n / 2), rep(1 / 2, n / 2))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 2, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario3 <- function(s12 = 0, n = 100, signal = 1) {
  # Single Change Point (Mirrored)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(-1 / 2, n / 2), rep(1 / 2, n / 2))
  h2 <- c(rep(1 / 2, n / 2), rep(-1 / 2, n / 2))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 3, n = n, S = S, h = h, signal = signal)
  return(obj)
}


scenario4 <- function(s12 = 0, n = 100, signal = 1) {
  # Flip-Flop (Unison)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(0.5, n / 4), rep(-0.5, n / 4), rep(0.5, n / 4), rep(-0.5, n / 4))
  h2 <- c(rep(0.5, n / 4), rep(-0.5, n / 4), rep(0.5, n / 4), rep(-0.5, n / 4))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 4, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario5 <- function(s12 = 0, n = 100, signal = 1) {
  # Flip-Flop (Mirrored)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(0.5, n / 4), rep(-0.5, n / 4), rep(0.5, n / 4), rep(-0.5, n / 4))
  h2 <- c(rep(-0.5, n / 4), rep(0.5, n / 4), rep(-0.5, n / 4), rep(0.5, n / 4))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 5, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario6 <- function(s12 = 0, n = 100, signal = 1) {
  # Progression
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(-1, n / 4), rep(-0.5, n / 4), rep(0, n / 4), rep(0.5, n / 4))
  h2 <- c(rep(-1, n / 4), rep(-0.5, n / 4), rep(0, n / 4), rep(0.5, n / 4))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 6, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario7 <- function(s12 = 0, n = 100, signal = 1) {
  # Progression (Mirrored)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- c(rep(-1, n / 4), rep(-0.5, n / 4), rep(0, n / 4), rep(0.5, n / 4))
  h2 <- c(rep(1, n / 4), rep(0.5, n / 4), rep(0, n / 4), rep(-0.5, n / 4))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 7, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario8 <- function(s12 = 0, n = 100, signal = 1) {
  # Misspecified Sin (Unison)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(1:n / 10)
  h2 <- sin(1:n / 10)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 8, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario9 <- function(s12 = 0, n = 100, signal = 1) {
  # Misspecified Sin (Asynchronous)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(1:n / 10)
  h2 <- sin(1:n / 5)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 9, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario10 <- function(s12 = 0, n = 100, signal = 1) {
  # Ultra Flip-Flop (Unison)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- rep(c(1, 1, 1, 1, -1, -1, -1, -1), n / 8)
  h2 <- rep(c(1, 1, 1, 1, -1, -1, -1, -1), n / 8)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 10, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario11 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 10
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- rnorm(K + 1, 0, 1)
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- rnorm(K + 1, 0, 1)
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 11, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario <- function(
    scenario_num = 1,
    sxy = 0,
    sx = 1,
    sy = 1,
    n = 100,
    signal = 1,
    seed = 321) {
  # used to select simulation scenario

  if (scenario_num == 1) {
    params <- scenario1(sxy, n, signal)
  } else if (scenario_num == 2) {
    params <- scenario2(sxy, n, signal)
  } else if (scenario_num == 3) {
    params <- scenario3(sxy, n, signal)
  } else if (scenario_num == 4) {
    params <- scenario4(sxy, n, signal)
  } else if (scenario_num == 5) {
    params <- scenario5(sxy, n, signal)
  } else if (scenario_num == 6) {
    params <- scenario6(sxy, n, signal)
  } else if (scenario_num == 7) {
    params <- scenario7(sxy, n, signal)
  } else if (scenario_num == 8) {
    params <- scenario8(sxy, n, signal)
  } else if (scenario_num == 9) {
    params <- scenario9(sxy, n, signal)
  } else if (scenario_num == 10) {
    params <- scenario10(sxy, n, signal)
  } else if (scenario_num == 11) {
    params <- scenario11(sxy, n, signal, seed)
  } else {
    stop("No such scenario. Try again.")
  }
  # HACK: should probably change individual scenario parameters
  # - e.g. scenario1(sxy, sx, sy, n, signal)
  params$S[1, 1] <- sx^2
  params$S[1, 2] <- sxy
  params$S[2, 1] <- sxy
  params$S[2, 2] <- sy^2
  return(params)
}
