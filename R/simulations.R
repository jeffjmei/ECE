demean <- function(X, method, params, ...) {
  if (method == "ECE") {
    X_mean <- 0 # do nothing
  } else if (method == "demean") {
    X_mean <- segment_mean(X, ...)
  } else if (method == "desmooth") {
    X_mean <- smooth_mean(X, ...)
  } else if (method == "oracle") {
    X_mean <- cbind(
      X1 = params$h[, 1],
      X2 = params$h[, 2]
    )
  } else if (method == "oracle cp") {
    X_mean <- cbind(
      X1 = segmented_mean(X[, 1], find_cp(params$h)),
      X2 = segmented_mean(X[, 2], find_cp(params$h))
    )
  }
  return(X - X_mean)
}

simulate_metric <- function(method, metric, params, n_sim = 1000, ...) {
  available_methods <- c(
    "ECE",
    "demean",
    "desmooth",
    "oracle",
    "oracle cp",
    "segmentation",
    "detrend",
    "pearson"
  )
  if (!(method %in% available_methods)) {
    stop("No such property. Please choose from power, est, mse")
  }

  # simulate correlation
  sim <- map(1:n_sim, ~ {
    # generate data
    X <- generate_data(params)

    # apply correlation
    if (method == "ECE") {
      cor_obj <- ece.test(X[, 1], X[, 2])
    } else if (method == "pearson") {
      cor_obj <- cor.test(X[, 1], X[, 2])
    } else if (method == "oracle") {
      X_segment <- segment_mean_oracle(X, params)
      cor_obj <- cor.test(
        X[, 1] - X_segment[, 1],
        X[, 2] - X_segment[, 2]
      )
    } else if (method == "segmentation") {
      X_segment <- segment_mean(X)
      cor.test(
        X[, 1] - X_segment[, 1],
        X[, 2] - X_segment[, 2]
      )
    } else if (method == "detrend") {
      X_detrended <- X - rotate(X)
      cor.test(X_detrended[, 1], X_detrended[, 2])
    } else {
      # HACK: demean missing ... because it causes conflicting
      #   names. `method` is being used for "loess" and "PELT"
      #   in addition to `cor.method` such as "ECE" and "desmooth"
      X_demean <- demean(X, method, params)
      cor_obj <- cor.test(X_demean[, 1], X_demean[, 2])
    }
  })
  est <- map_dbl(sim, "estimate")
  pval <- map_dbl(sim, "p.value")

  # extract value
  if (metric == "power") {
    mean(pval < 0.05)
  } else if (metric == "est") {
    mean(est)
  } else if (metric == "mse") {
    rxy <- params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2])
    mean((est - rxy)^2)
  } else if (metric == "bias") {
    rxy <- params$S[1, 2] / sqrt(params$S[1, 1] * params$S[2, 2])
    mean(est - rxy)
  } else if (metric == "type1") {
    list(
      type1 = mean(pval < 0.05),
      sd = sd(pval < 0.05)
    )
  }
}

export_simulations <- function(..., method, metric, params, n_sims, export_file) {
  row <- data.frame(
    ...,
    method = method,
    metric = metric,
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

simulate_grid <- function(param_grid, metric, export_file) {
  progress_ct <- 0 # global counter
  sim <- pmap_dfr(
    param_grid,
    function(scenario, method, sxy, sx, sy, n, signal, n_sim) {
      # update counter
      progress_ct <<- progress_ct + 1
      message("Running: ", progress_ct, "/", nrow(param_grid))

      # pull scenario information
      params <- scenario(
        scenario_num = scenario,
        sxy = sxy,
        sx = sx,
        sy = sy,
        n = n,
        signal = signal
      )

      # run simulation
      export_vals <- simulate_metric(
        method = method,
        metric = metric,
        params = params,
        n_sim = n_sim
      )

      # export to csv
      export_simulations(
        export_vals,
        method = method,
        metric = metric,
        params = params,
        n_sims = n_sim,
        export_file = export_file
      )
    }
  )
}
