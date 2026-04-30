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

plot_type1 <- function(sim_type1, sample_size, scenario, n_simulations, signal_val = 1) {
  sim_type1 %>%
    filter(
      n == sample_size,
      scenario_num == scenario,
      n_sims == n_simulations,
      signal == signal_val
    ) %>%
    mutate(
      se       = sd / sqrt(n_sims),
      ci_lower = type1 - 1.96 * se,
      ci_upper = type1 + 1.96 * se
    ) %>%
    ggplot(aes(x = method, y = type1)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    labs(
      x = "Method",
      y = "Estimated Type I Error (95% CI)",
      title = glue::glue("Type I Error (n = {sample_size}, Scenario {scenario})")
    ) +
    theme_minimal()
}

t1_table <- function(df, scenario) {
  # Enforce method order
  desired <- c(
    "Oracle",
    "ECE",
    "Segment-ECE",
    "Segment (BIC)",
    "Segment (MBIC)",
    "Segment (AIC)"
  )
  tbl <- df %>%
    filter(scenario_num == scenario) %>%
    mutate(
      # Clean up method names
      method = recode(
        method,
        "segment-ECE" = "Segment-ECE",
        "segmentation" = "Segment (BIC)",
        "ECE" = "ECE",
        "segment-AIC" = "Segment (AIC)",
        "segment-MBIC" = "Segment (MBIC)",
        "oracle" = "Oracle"
      )
    ) %>%
    select(
      n,
      method,
      export_vals
    ) %>%
    pivot_wider(
      names_from = method,
      values_from = export_vals
    ) %>%
    round(3) %>%
    # enforce order
    dplyr::relocate(
      dplyr::any_of(desired),
      .after = n
    )
  return(tbl)
}
