# Plot Simulation Instance
plot_scenario <- function(params, main = "Scenario Example") {
  e <- MASS::mvrnorm(params$n, c(0, 0), params$S)
  X <- data.frame(params$h + e)
  colnames(X) <- c("X1", "X2")
  X$h1 <- params$h[, 1]
  X$h2 <- params$h[, 2]
  X$index <- seq_len(params$n)

  params$scenario
  ggplot(X, aes(x = index)) +
    geom_line(aes(y = X1), color = "red", alpha = 0.5) +
    geom_line(aes(y = X2), color = "blue", alpha = 0.5) +
    geom_line(aes(y = h1), color = "red", linewidth = 1.5) +
    geom_line(aes(y = h2), color = "blue", linewidth = 1.5) +
    labs(
      title = glue::glue("Scenario {params$scenario} (n = {params$n})"),
      x = "Index",
      y = "Value"
    ) +
    theme_minimal()
}

scenario_name_to_num <- function(name) {
  name_lst <- c(
    "No Change Point",
    "Change Point (Unison)",
    "Change Point (Mirrored)",
    "Flip Flop (Unison)",
    "Flip Flop (Mirrored)",
    "Progression (Unison)",
    "Progression (Flip Flop)",
    "Misspecified Sin (Unison)",
    "Misspecified Sin (Mirrored)"
  )


  if (name == name_lst[1]) {
    return(1)
  } else if (name == name_lst[2]) {
    return(2)
  } else if (name == name_lst[3]) {
    return(3)
  } else if (name == name_lst[4]) {
    return(4)
  } else if (name == name_lst[5]) {
    return(5)
  } else if (name == name_lst[6]) {
    return(6)
  } else if (name == name_lst[7]) {
    return(7)
  } else if (name == name_lst[8]) {
    return(8)
  } else if (name == name_lst[9]) {
    return(9)
  }
}

scenario_num_to_name <- function(scenario_num) {
  name_lst <- c(
    "No Change Point",
    "Change Point (Unison)",
    "Change Point (Mirrored)",
    "Flip Flop (Unison)",
    "Flip Flop (Mirrored)",
    "Progression (Unison)",
    "Progression (Flip Flop)",
    "Misspecified Sin (Unison)",
    "Misspecified Sin (Mirrored)"
  )


  if (scenario_num == 1) {
    return(name_lst[1])
  } else if (scenario_num == 2) {
    return(name_lst[2])
  } else if (scenario_num == 3) {
    return(name_lst[3])
  } else if (scenario_num == 4) {
    return(name_lst[4])
  } else if (scenario_num == 5) {
    return(name_lst[5])
  } else if (scenario_num == 6) {
    return(name_lst[6])
  } else if (scenario_num == 7) {
    return(name_lst[7])
  } else if (scenario_num == 8) {
    return(name_lst[8])
  } else if (scenario_num == 9) {
    return(name_lst[9])
  } else {
    stop("No such scenario. Try again.")
  }
}

simulate_power_demean <- function(params, n_sim = 1000, ...) {
  pval <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- segment_mean(X, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$p.val # estimate cor
  })
  mean(pval < 0.05)
}

simulate_power_desmooth <- function(params, n_sim = 1000, method = "loess", ...) {
  pval <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- smooth_mean(X, method = method, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$p.val # estimate cor
  })
  mean(pval < 0.05)
}

simulate_power_ece <- function(params, n_sim = 1000) {
  pval <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    ece.test(X[, 1], X[, 2])$p.val # estimate cor
  })
  mean(pval < 0.05)
}

simulate_power <- function(params, method, n_sim = 1000, ...) {
  if (method == "ECE") {
    simulate_power_ece(params, n_sim)
  } else if (method == "demean") {
    simulate_power_demean(params, n_sim, ...)
  } else if (method == "desmooth") {
    simulate_power_desmooth(params, n_sim, ...)
  }
}

plot_power <- function(sim_est, sample_size, scenario, n_simulations) {
  sim_est %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations
    ) %>%
    ggplot(aes(x = sxy, y = power, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.05) +
    labs(
      title = "Power by Correlation Level (n = 200, Scenario 1)",
      x = "True Correlation (sxy)",
      y = "Power",
      color = "Method"
    ) +
    theme_minimal()
}

plot_est <- function(sim_est, sample_size, scenario, n_simulations) {
  sim_est %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations
    ) %>%
    ggplot(aes(x = sxy, y = est, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    xlim(0, 0.55) +
    ylim(0, 0.55) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = glue::glue("Estimated Correlation vs True Correlation (n = {sample_size}, Scenario {scenario})"),
      x = "True Correlation (sxy)",
      y = "Estimated Correlation",
      color = "Method"
    ) +
    theme_minimal()
}

plot_mse <- function(sim_est, sample_size, scenario, n_simulations) {
  sim_mse %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations
    ) %>%
    ggplot(aes(x = sxy, y = val, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = glue::glue("MSE vs Correlation (n = {sample_size}, Scenario {scenario})"),
      x = "Correlation (sxy)",
      y = "MSE",
      color = "Method"
    ) +
    theme_minimal()
}

simulate_demean <- function(params, n_sim = 1000, ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- segment_mean(X, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
  })
  mean(est)
}

simulate_desmooth <- function(params, n_sim = 1000, method = "loess", ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- smooth_mean(X, method = method, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
  })
  mean(est)
}

simulate_ece <- function(params, n_sim = 1000) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    ece.test(X[, 1], X[, 2])$estimate # estimate cor
  })
  mean(est)
}

simulate_est <- function(params, method, n_sim = 1000, ...) {
  if (method == "ECE") {
    simulate_ece(params, n_sim)
  } else if (method == "demean") {
    simulate_demean(params, n_sim, ...)
  } else if (method == "desmooth") {
    simulate_desmooth(params, n_sim, ...)
  }
}

simulate_demean_mse <- function(params, n_sim = 1000, ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- segment_mean(X, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
  })
  mean(
    (est - params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2]))^2
  )
}

simulate_desmooth_mse <- function(params, n_sim = 1000, method = "loess", ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- smooth_mean(X, method = method, ...) # estimate mean
    X_demean <- X - X_mean # remove mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
  })
  mean(
    (est - params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2]))^2
  )
}

simulate_ece_mse <- function(params, n_sim = 1000) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    ece.test(X[, 1], X[, 2])$estimate # estimate cor
  })
  mean(
    (est - params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2]))^2
  )
}

simulate_mse <- function(params, method, n_sim = 1000, ...) {
  if (method == "ECE") {
    simulate_ece_mse(params, n_sim)
  } else if (method == "demean") {
    simulate_demean_mse(params, n_sim, ...)
  } else if (method == "desmooth") {
    simulate_desmooth_mse(params, n_sim, ...)
  }
}

export_simulations <- function(val, method, params, n_sims, export_file) {
  row <- data.frame(
    val = val,
    method = method,
    n = params$n,
    sx = sqrt(params$S[1, 1]),
    sy = sqrt(params$S[2, 2]),
    sxy = params$S[1, 2],
    signal = params$signal,
    scenario_num = params$scenario,
    n_sims = n_sims,
    datetime = Sys.time()
  )

  # If file doesn't exist, write with header
  if (!file.exists(export_file)) {
    write.table(row, export_file, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # Append without writing header
    write.table(row, export_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  }
}
