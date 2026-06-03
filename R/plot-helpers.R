make_overlap_hist <- function(sims, xlab = "value", colors = NULL) {
  nms <- names(sims)
  if (is.null(colors)) {
    palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3", "#A65628")
    colors <- setNames(palette[seq_along(sims)], nms)
  } else if (is.null(names(colors))) {
    colors <- setNames(colors, nms)
  }

  dat <- purrr::map_dfr(nms, \(nm) tibble::tibble(val = sims[[nm]], group = nm))
  means <- tibble::tibble(group = nms, mean = purrr::map_dbl(sims, mean))

  ggplot2::ggplot(dat, ggplot2::aes(x = val, fill = group)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 40, color = "white", alpha = 0.6, position = "identity"
    ) +
    ggplot2::geom_vline(
      data = means,
      ggplot2::aes(xintercept = mean, color = group),
      linetype = "dashed"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = colors, guide = "none") +
    ggplot2::labs(
      x = xlab, y = "Density", fill = "Estimator",
      subtitle = "Dashed = sample mean"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

make_sim_hist <- function(sim, true, xlab) {
  tibble::tibble(val = sim) |>
    ggplot2::ggplot(ggplot2::aes(x = val)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 40, fill = "steelblue", color = "white", alpha = 0.8
    ) +
    ggplot2::geom_vline(xintercept = true, color = "firebrick") +
    ggplot2::geom_vline(xintercept = mean(sim), linetype = "dashed") +
    ggplot2::labs(
      x = xlab, y = "density",
      subtitle = "Red = true, dashed = sample mean"
    ) +
    ggplot2::theme_minimal()
}

pcurve <- function(params, method, n_sim = 1000, B = NULL) {
  map_dbl(seq_len(n_sim), \(i) {
    X <- generate_data(params)
    test.diag(X, method = method, B = B, params = params)$p.value
  })
}

plot_pcurve <- function(pvals, facet = ~method, title = NULL) {
  dat <- map_dfr(names(pvals), \(m) tibble(p_value = pvals[[m]], method = m))
  ggplot(dat, aes(x = p_value)) +
    geom_histogram(aes(y = after_stat(density)),
      breaks = seq(0, 1, by = 0.05),
      fill = "steelblue", color = "white", alpha = 0.8
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    facet_wrap(facet) +
    labs(x = "p-value", y = "Density", title = title) +
    theme_minimal()
}
