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
  set.seed(seed)
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
  obj <- list(scenario = 11, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario12 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Misspecified Linear
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- signal * (1:n - n / 2) / 100 # divided arbitrarily
  h2 <- signal * (1:n - n / 2) / 100 # as a sensible default
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 12, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario13 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Misspecified Random Walk
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h <- NA

  # Return Parameter Object
  obj <- list(scenario = 13, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario14 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Yearly Variation
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 365)
  h2 <- sin(2 * pi * (1:n) / 365)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 14, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario15 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Yearly Variation with Spikes
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 365)
  h2 <- sin(2 * pi * (1:n) / 365)

  spike_idx <- ((1:n - 90) %% 365) %in% 0:1
  h1[spike_idx] <- 4 + h1[spike_idx] # double the height for spike days
  h2[spike_idx] <- 4 + h2[spike_idx] # double the height for spike days
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 15, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario16 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Yearly Variation with Spikes
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 365)
  h2 <- sin(2 * pi * (1:n) / 365)

  spike_idx <- ((1:n - 90) %% 365) %in% 0
  h1[spike_idx] <- 4 + h1[spike_idx] # double the height for spike days
  h2[spike_idx] <- 4 + h2[spike_idx] # double the height for spike days
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 16, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario17 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Misspecified Linear
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  h1 <- signal * (1:n - n / 2) / 100 # divided arbitrarily
  h2 <- -signal * (1:n - n / 2) / 100 # as a sensible default
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 17, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario18 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style (Exponential)
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 10
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- rexp(K + 1)
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- rexp(K + 1)
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 18, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario19 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style (Random Walk)
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 10
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- cumsum(rnorm(K + 1, 0, 1))
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- cumsum(rnorm(K + 1, 0, 1))
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 19, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario20 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Sinusoidal Daily Pattern (24 observations/period)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 24)
  h2 <- sin(2 * pi * (1:n) / 24)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 20, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario21 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Sinusoidal Daily Pattern (24 observations/period) (async)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 24)
  h2 <- sin(2 * pi * (1:n) / 24 + pi / 2)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 21, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario22 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style (Exponential) - Fewer CP
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 50
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- rexp(K + 1)
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- rexp(K + 1)
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 22, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario23 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Yearly Variation
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 365)
  h2 <- sin(2 * pi * (1:n) / 365 + pi / 2)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 23, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario24 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Sinusoidal Daily Pattern (24 observations/period)
  # (async - half-cycle-shift)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 24)
  h2 <- sin(2 * pi * (1:n) / 24 + pi)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 24, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario25 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Yearly Variation (half-cycle shift)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)

  # Mean Vector
  h1 <- sin(2 * pi * (1:n) / 365)
  h2 <- sin(2 * pi * (1:n) / 365 + pi)
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 25, n = n, S = S, h = h, signal = signal)
  return(obj)
}

scenario26 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style (Random Walk - Exponential)
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 10
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- cumsum(1 - rexp(K + 1))
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- cumsum(1 - rexp(K + 1))
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 26, n = n, S = S, h = h, signal = signal, seed = seed)
  return(obj)
}

scenario27 <- function(s12 = 0, n = 100, signal = 1, seed = 321) {
  # Wild Style (Exponential - smaller lambda)
  set.seed(seed)
  S <- matrix(c(
    1, s12,
    s12, 1
  ), byrow = T, ncol = 2)
  K <- n / 10
  cp1 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp1 <- c(0, cp1, n)
  mu1 <- rexp(K + 1, 2)
  h1 <- unlist(sapply(1:(K + 1), function(i) rep(mu1[i], cp1[i + 1] - cp1[i])))

  cp2 <- 4 * sort(sample(1:(n / 4 - 1), K, replace = F))
  cp2 <- c(0, cp2, n)
  mu2 <- rexp(K + 1, 2)
  h2 <- unlist(sapply(1:(K + 1), function(i) rep(mu2[i], cp2[i + 1] - cp2[i])))
  h <- signal * cbind(h1, h2)

  # Return Parameter Object
  obj <- list(scenario = 27, n = n, S = S, h = h, signal = signal, seed = seed)
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
  } else if (scenario_num == 12) {
    params <- scenario12(sxy, n, signal, seed)
  } else if (scenario_num == 13) {
    params <- scenario13(sxy, n, signal, seed)
  } else if (scenario_num == 14) {
    params <- scenario14(sxy, n, signal, seed)
  } else if (scenario_num == 15) {
    params <- scenario15(sxy, n, signal, seed)
  } else if (scenario_num == 16) {
    params <- scenario16(sxy, n, signal, seed)
  } else if (scenario_num == 17) {
    params <- scenario17(sxy, n, signal, seed)
  } else if (scenario_num == 18) {
    params <- scenario18(sxy, n, signal, seed)
  } else if (scenario_num == 19) {
    params <- scenario19(sxy, n, signal, seed)
  } else if (scenario_num == 20) {
    params <- scenario20(sxy, n, signal, seed)
  } else if (scenario_num == 21) {
    params <- scenario21(sxy, n, signal, seed)
  } else if (scenario_num == 22) {
    params <- scenario22(sxy, n, signal, seed)
  } else if (scenario_num == 23) {
    params <- scenario23(sxy, n, signal, seed)
  } else if (scenario_num == 24) {
    params <- scenario24(sxy, n, signal, seed)
  } else if (scenario_num == 25) {
    params <- scenario25(sxy, n, signal, seed)
  } else if (scenario_num == 26) {
    params <- scenario26(sxy, n, signal, seed)
  } else if (scenario_num == 27) {
    params <- scenario27(sxy, n, signal, seed)
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
