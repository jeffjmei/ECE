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

plot_pvals <- function(dat, facet = NULL, title = NULL, ncol = NULL, dir = "h", bins = 10) {
  if (!bins %in% c(10, 20)) stop("`bins` must be 10 or 20")
  bucket_cols <- grep("^p[0-9]{2}$", names(dat), value = TRUE)
  if (length(bucket_cols) == 0) {
    stop("No bucket columns (p00–p95) found in `dat`. ",
         "Found columns: ", paste(names(dat), collapse = ", "))
  }
  bin_width <- 1 / bins
  long <- dat |>
    tidyr::pivot_longer(dplyr::all_of(bucket_cols), names_to = "bucket", values_to = "prop") |>
    dplyr::mutate(
      idx = as.integer(sub("^p", "", bucket)) %/% 5L,   # 0..19
      x   = (idx %/% (20L %/% bins)) * bin_width
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(setdiff(names(dat), bucket_cols), "x")))) |>
    dplyr::summarise(density = sum(prop) / bin_width, .groups = "drop")

  facet_vars <- if (!is.null(facet)) all.vars(facet) else character(0)
  group_vars <- if (length(facet_vars) > 0) facet_vars else character(0)
  if (length(group_vars) > 0) {
    dups <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(.n_rows = dplyr::n(), .groups = "drop") |>
      dplyr::filter(.n_rows > 1)
    if (nrow(dups) > 0) {
      stop("Multiple rows share the same facet level(s). Filter to one row per level before plotting.\n",
           "Duplicated combinations:\n",
           paste(capture.output(print(dups)), collapse = "\n"))
    }
  } else if (nrow(dat) > 1) {
    stop("Multiple rows provided but no facet variable specified. ",
         "Either filter to one row or supply a facet argument.")
  }

  facet_layer <- if (is.null(facet)) {
    NULL
  } else if (inherits(facet, "Facet")) {
    facet
  } else {
    ggplot2::facet_wrap(facet, ncol = ncol, dir = dir)
  }

  ggplot2::ggplot(long, ggplot2::aes(x = x, y = density)) +
    ggplot2::geom_col(width = bin_width, fill = "steelblue", color = "white", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    facet_layer +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = bin_width * 2)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) +
    ggplot2::labs(x = "p-value", y = "Density", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

pcurve <- function(params, method, n_sim = 1000, B = NULL, seed = 1) {
  set.seed(seed)
  map_dbl(seq_len(n_sim), \(i) {
    X <- generate_data(params)
    test.diag(X, method = method, B = B, params = params)$p.value
  })
}

plot_pcurve <- function(pvals, facet = ~method, title = NULL, ncol = NULL, dir = "h") {
  dat <- if (is.data.frame(pvals)) {
    pvals
  } else {
    nms <- names(pvals)
    map_dfr(nms, \(m) tibble(p_value = pvals[[m]], method = m)) |>
      mutate(method = factor(method, levels = nms))
  }
  facet_layer <- if (inherits(facet, "Facet")) facet else facet_wrap(facet, ncol = ncol, dir = dir)
  ggplot(dat, aes(x = p_value)) +
    geom_histogram(aes(y = after_stat(density)),
      breaks = seq(0, 1, by = 0.1),
      fill = "steelblue", color = "white", alpha = 0.8
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    facet_layer +
    scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) +
    labs(x = "p-value", y = "Density", title = title) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}
