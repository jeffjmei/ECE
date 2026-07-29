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

plot_pvals <- function(dat, color_by = NULL, facet = NULL, title = NULL, ncol = NULL, dir = "h", bins = 10) {
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
      idx = as.integer(sub("^p", "", bucket)) %/% 5L,
      x   = (idx %/% (20L %/% bins)) * bin_width
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(setdiff(names(dat), bucket_cols), "x")))) |>
    dplyr::summarise(density = sum(prop) / bin_width, .groups = "drop")

  if (!inherits(facet, "Facet")) {
    id_vars <- c(color_by, if (!is.null(facet)) all.vars(facet) else character(0))
    if (length(id_vars) > 0) {
      dups <- dat |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_vars))) |>
        dplyr::summarise(.n_rows = dplyr::n(), .groups = "drop") |>
        dplyr::filter(.n_rows > 1)
      if (nrow(dups) > 0) {
        stop("Multiple rows share the same color/facet combination(s).\n",
             paste(capture.output(print(dups)), collapse = "\n"))
      }
    } else if (nrow(dat) > 1) {
      stop("Multiple rows provided but neither color_by nor facet specified.")
    }
  }

  facet_layer <- if (is.null(facet)) {
    NULL
  } else if (inherits(facet, "Facet")) {
    facet
  } else {
    ggplot2::facet_wrap(facet, ncol = ncol, dir = dir)
  }

  if (!is.null(color_by)) {
    bar_aes  <- ggplot2::aes(x = x, y = density, fill = .data[[color_by]])
    bar_geom <- ggplot2::geom_col(width = bin_width, color = "white", alpha = 0.6, position = "identity")
    color_scale <- ggplot2::scale_fill_brewer(palette = "Set1")
  } else {
    bar_aes  <- ggplot2::aes(x = x, y = density)
    bar_geom <- ggplot2::geom_col(width = bin_width, fill = "steelblue", color = "white", alpha = 0.8)
    color_scale <- NULL
  }

  ggplot2::ggplot(long, bar_aes) +
    bar_geom +
    color_scale +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    facet_layer +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = bin_width * 2)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) +
    ggplot2::labs(x = "p-value", y = "Density", title = title, fill = color_by) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position    = "bottom"
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

# Diagnostic: is the ECE covariance constant over time?
#
# For each column pair of `X`, plots the raw ECE terms (light gray, so outliers
# stand out) with a rolling-window mean on top and a dashed reference line at the
# global mean (the ECE covariance the test sums). Under a constant correlation
# the running mean fluctuates around the flat line; a change point shows up as a
# step or ramp away from it. `window` controls smoothing (defaults to n/5).
plot_ece_terms <- function(X, window = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(window)) window <- max(10, floor(n / 5))

  idx <- combn(p, 2)

  dat <- purrr::map_dfr(seq_len(ncol(idx)), function(k) {
    i <- idx[1, k]
    j <- idx[2, k]
    s <- lag_term(X[, i], X[, j])
    tibble::tibble(
      t      = seq_len(n),
      pair   = paste(i, j, sep = "-"),
      raw    = s,
      run    = rolling_mean(s, window),
      global = mean(s)
    )
  })

  ggplot2::ggplot(dat, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = raw), color = "gray80") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = global),
      linetype = "dashed", color = "firebrick"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = run), color = "steelblue", na.rm = TRUE) +
    ggplot2::facet_wrap(~pair, scales = "free_y") +
    ggplot2::labs(
      x = "time", y = "ECE term (covariance scale)",
      subtitle = sprintf("gray = raw term, blue = %d-window mean, dashed = global mean", window)
    ) +
    ggplot2::theme_minimal()
}

# Diagnostic: is the ECE variance (diagonal) constant over time?
#
# Same grammar as plot_ece_terms(), but for each column against itself
# (lag_term(X[,i], X[,i])) instead of each column pair — i.e. the diagonal
# ECE term, whose mean is the "variance" that shows up in ece.cov()'s
# diagonal. Useful for seeing whether a column's own diagonal term is
# unstable (e.g. hovering near/below zero) rather than only looking at the
# aggregate ece.cov() value. Note this plots the raw lag_term average, not
# ece.cov()'s L-extrapolated estimate (same simplification plot_ece_terms
# already makes for the off-diagonal/covariance case).
plot_ece_variance <- function(X, window = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(p))
  if (is.null(window)) window <- max(10, floor(n / 5))

  dat <- purrr::map_dfr(seq_len(p), function(i) {
    s <- lag_term(X[, i], X[, i])
    tibble::tibble(
      t      = seq_len(n),
      series = colnames(X)[i],
      raw    = s,
      run    = rolling_mean(s, window),
      global = mean(s)
    )
  })

  ggplot2::ggplot(dat, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = raw), color = "gray80") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = global),
      linetype = "dashed", color = "firebrick"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = run), color = "steelblue", na.rm = TRUE) +
    ggplot2::facet_wrap(~series, scales = "free_y") +
    ggplot2::labs(
      x = "time", y = "ECE diagonal term (variance scale)",
      subtitle = sprintf("gray = raw term, blue = %d-window mean, dashed = global mean, solid black = 0", window)
    ) +
    ggplot2::theme_minimal()
}

# Diagnostic: naive analog to plot_ece_terms().
#
# Same visual grammar as plot_ece_terms() (raw pairwise product in gray, a
# rolling-window mean in blue, dashed global mean), but the mean is removed
# naively — a rolling mean per column — rather than through ECE's
# difference-based, mean-shift-invariant construction. Drift here reflects
# both genuine covariance non-stationarity and any mean-shift leakage the
# rolling mean failed to remove, so comparing this against plot_ece_terms()
# shows how much of the naive drift is a mean-shift artifact.
plot_naive_terms <- function(X, window = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(window)) window <- max(10, floor(n / 5))

  R <- apply(X, 2, function(col) col - rolling_mean(col, window))
  idx <- combn(p, 2)

  dat <- purrr::map_dfr(seq_len(ncol(idx)), function(k) {
    i <- idx[1, k]
    j <- idx[2, k]
    s <- R[, i] * R[, j]
    tibble::tibble(
      t      = seq_len(n),
      pair   = paste(i, j, sep = "-"),
      raw    = s,
      run    = rolling_mean(s, window),
      global = mean(s, na.rm = TRUE)
    )
  })

  ggplot2::ggplot(dat, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = raw), color = "gray80", na.rm = TRUE) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = global),
      linetype = "dashed", color = "firebrick"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = run), color = "steelblue", na.rm = TRUE) +
    ggplot2::facet_wrap(~pair, scales = "free_y") +
    ggplot2::labs(
      x = "time", y = "naive term (covariance scale)",
      subtitle = sprintf("gray = raw product of rolling-mean residuals, blue = %d-window mean, dashed = global mean", window)
    ) +
    ggplot2::theme_minimal()
}

# Runs SIP::SIP.acf while intercepting its "sample variance will be used"
# warnings, which fire per-lag whenever SIP.acf's own difference-based
# variance estimate comes out negative and it falls back to var(x) instead.
# The warning message is the only place that lag index is exposed (SIP.acf's
# return value doesn't carry it), so we parse it out of the message text and
# suppress the warning from printing. Returns SIP.acf's result with an added
# `fallback` logical vector, aligned with `lag`, flagging which lags used the
# substituted variance.
sip_acf_with_fallback <- function(x, lag.max = 4, ci.level = 0.95) {
  fallback <- logical(lag.max + 1)

  res <- withCallingHandlers(
    SIP::SIP.acf(x, lag.max = lag.max, ci.level = ci.level, plot = FALSE),
    warning = function(w) {
      if (grepl("sample variance will be used", conditionMessage(w))) {
        lag_i <- as.integer(regmatches(
          conditionMessage(w),
          regexpr("(?<=at )[0-9]+", conditionMessage(w), perl = TRUE)
        ))
        fallback[lag_i + 1] <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  res$fallback <- fallback
  res
}

# Diagnostic: mean-shift-invariant ACF via SIP::SIP.acf
#
# For each column of `X`, draws a base-R acf()-style plot (thin vertical
# lines from 0, dashed blue confidence bounds) using the SIP.acf estimate,
# with bars colored red where |acf| exceeds the null CI. Unlike a naive
# acf(), SIP.acf discounts autocorrelation induced by mean shifts, so
# comparing this against base-R acf() shows how much of the naive series'
# autocorrelation is attributable to a non-stationary mean versus genuine
# residual dependence.
#
# Lags where SIP.acf's own variance estimate went negative (see
# sip_acf_with_fallback()) fall back to the naive var(x) as a denominator.
# If the series has real mean shifts, var(x) is inflated by that mean
# structure relative to the mean-shift-adjusted variance SIP.acf is meant to
# use, so those lags' bars are drawn at reduced opacity to flag them as less
# trustworthy than the rest.
plot_sip_acf <- function(X, lag.max = 4, ci.level = 0.95) {
  X <- as.matrix(X)
  p <- ncol(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(p))

  par(mfrow = c(p, 1), mar = c(2, 3, 0, 1), oma = c(2, 0, 1, 0), mgp = c(1.8, 1, 0))

  for (i in seq_len(p)) {
    res <- sip_acf_with_fallback(X[, i], lag.max = lag.max, ci.level = ci.level)
    lag <- res$lag
    acf <- res$acf
    bar_col <- ifelse(abs(acf) > res$h0.ci, "red", "black")
    bar_col <- mapply(adjustcolor, bar_col, alpha.f = ifelse(res$fallback, 0.3, 1))
    plot(lag, acf, type = "h", xlab = "", ylab = colnames(X)[i], col = bar_col)
    abline(h = 0)
    abline(h = c(-1, 1) * res$h0.ci, col = "blue", lty = 2)
  }
  mtext("Lag", side = 1, outer = TRUE, line = 0.5)
}
