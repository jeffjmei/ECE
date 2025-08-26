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

plot.ece.helper <- function(X, Y, L = 2) {
  n <- length(X)

  # generate points
  xx <- 1:2
  yy <- c(
    lag_diff(X, Y, k = 1),
    lag_diff(X, Y, k = 2)
  ) / (2 * n)
  fit <- lm(yy ~ xx)

  # initialize plot
  plot(NULL,
    xlim = c(-1, 2),
    ylim = c(
      -0.5, 1
    ),
    ylab = "Tk",
    main = paste(
      paste("variance =", round(fit$coef[1], 3)),
      paste("theta =", round(fit$coef[2], 3))
    )
  )
  abline(v = 0, h = 0, lwd = 2)
  points(xx, yy)

  # plot regression line
  abline(fit)
  points(0, fit$coef[1], col = "red")

  # print slope/intercept
}

plot.ece <- function(X) {
  p <- ncol(X)
  par(mfrow = c(p, p))
  for (i in 1:p) {
    for (j in i:p) {
      plot.ece.helper(X[, i], X[, j])
    }
  }
  par(mfrow = c(1, 1))
}
