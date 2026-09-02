make_bezier <- function(x, y, n = 100) {
  n_points <- length(x)

  # ----------------------------------------------------------
  # 2 points: straight line
  # ----------------------------------------------------------

  if (n_points == 2) {
    t <- seq(0, 1, length.out = n)

    x_new <- (1 - t) * x[1] + t * x[2]

    y_new <- (1 - t) * y[1] + t * y[2]

    # ----------------------------------------------------------
    # 3 points: quadratic Bezier
    # ----------------------------------------------------------
  } else if (n_points == 3) {
    t <- seq(0, 1, length.out = n)

    x_new <- (1 - t)^2 * x[1] + 2 * (1 - t) * t * x[2] + t^2 * x[3]

    y_new <- (1 - t)^2 * y[1] + 2 * (1 - t) * t * y[2] + t^2 * y[3]

    # ----------------------------------------------------------
    # 4 points: cubic Bezier
    # ----------------------------------------------------------
  } else if (n_points == 4) {
    t <- seq(0, 1, length.out = n)

    x_new <- (1 - t)^3 *
      x[1] +
      3 * (1 - t)^2 * t * x[2] +
      3 * (1 - t) * t^2 * x[3] +
      t^3 * x[4]

    y_new <- (1 - t)^3 *
      y[1] +
      3 * (1 - t)^2 * t * y[2] +
      3 * (1 - t) * t^2 * y[3] +
      t^3 * y[4]
  } else {
    stop(
      "Each flow must have 2, 3, or 4 control points."
    )
  }

  tibble(
    x = x_new,
    y = y_new
  )
}
