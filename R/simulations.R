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

simulate_power_oracle <- function(params, n_sim = 1000) {
  pval <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    cor.test(X[, 1] - params$h[, 1], X[, 2] - params$h[, 2])$p.val # estimate cor
  })
  mean(pval < 0.05)
}

simulate_power_oracle_cp <- function(params, n_sim = 1000, ...) {
  pval <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- cbind(
      X1 = segmented_mean(X[, 1], find_cp(params$h)),
      X2 = segmented_mean(X[, 2], find_cp(params$h))
    )
    X_demean <- X - X_mean
    cor.test(X_demean[, 1], X_demean[, 2])$p.val # estimate cor
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
  } else if (method == "oracle") {
    simulate_power_oracle(params, n_sim)
  } else if (method == "oracle cp") {
    simulate_power_oracle_cp(params, n_sim)
  }
}

plot_power <- function(sim_power, sample_size, scenario, n_simulations, signal_val = 1) {
  sim_power %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations,
      signal == signal_val
    ) %>%
    ggplot(aes(x = sxy, y = val, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ylim(0, 1) +
    geom_hline(yintercept = 0.05) +
    labs(
      title = glue::glue("Power (n = {sample_size}, Scenario {scenario})"),
      x = "True Correlation (sxy)",
      y = "Power",
      color = "Method"
    ) +
    theme_minimal()
}

plot_est <- function(sim_est, sample_size, scenario, n_simulations, signal_val = 1) {
  sim_est %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations,
      signal == signal_val
    ) %>%
    ggplot(aes(x = sxy, y = sxy - val, color = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    # xlim(0, 0.55) +
    # ylim(0, 0.55) +
    # geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = glue::glue("Correlation Bias (n = {sample_size}, Scenario {scenario})"),
      x = "True Correlation (sxy)",
      y = "Bias",
      color = "Method"
    ) +
    theme_minimal()
}

plot_mse <- function(sim_est, sample_size, scenario, n_simulations, signal_val = 1) {
  sim_mse %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations,
      signal == signal_val
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

simulate_oracle <- function(params, n_sim = 1000) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    cor.test(X[, 1] - params$h[, 1], X[, 2] - params$h[, 2])$estimate # estimate cor
  })
  mean(est)
}

simulate_oracle_cp <- function(params, n_sim = 1000, ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- cbind(
      X1 = segmented_mean(X[, 1], find_cp(params$h)),
      X2 = segmented_mean(X[, 2], find_cp(params$h))
    )
    X_demean <- X - X_mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
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
  } else if (method == "oracle") {
    simulate_oracle(params, n_sim)
  } else if (method == "oracle cp") {
    simulate_oracle_cp(params, n_sim)
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

simulate_oracle_mse <- function(params, n_sim = 1000) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    cor.test(X[, 1] - params$h[, 1], X[, 2] - params$h[, 2])$estimate # estimate cor
  })
  mean(
    (est - params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2]))^2
  )
}

simulate_oracle_cp_mse <- function(params, n_sim = 1000, ...) {
  est <- replicate(n_sim, {
    X <- generate_data(params) # generate data
    X_mean <- cbind(
      X1 = segmented_mean(X[, 1], find_cp(params$h)),
      X2 = segmented_mean(X[, 2], find_cp(params$h))
    )
    X_demean <- X - X_mean
    cor.test(X_demean[, 1], X_demean[, 2])$estimate # estimate cor
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
  } else if (method == "oracle") {
    simulate_oracle_mse(params, n_sim)
  } else if (method == "oracle cp") {
    simulate_oracle_cp_mse(params, n_sim)
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
