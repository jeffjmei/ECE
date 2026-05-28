make_sim_hist <- function(sim, true, xlab) {
  tibble::tibble(val = sim) |>
    ggplot2::ggplot(ggplot2::aes(x = val)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 40, fill = "steelblue", color = "white", alpha = 0.8
    ) +
    ggplot2::geom_vline(xintercept = true, color = "firebrick") +
    ggplot2::geom_vline(xintercept = mean(sim), linetype = "dashed") +
    ggplot2::labs(x = xlab, y = "density",
                  subtitle = "Red = true, dashed = sample mean") +
    ggplot2::theme_minimal()
}
