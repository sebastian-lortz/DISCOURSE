#' Plot cooling schedule of a \code{discourse.object}
#'
#' @param discourse_obj Output from \code{optim_*} functions, with class \code{discourse.object}.
#' @return A \pkg{ggplot2} line plot of squared temperature vs. iteration number.
#' @importFrom rlang .data
#' @export
plot_cooling <- function(discourse_obj) {
  if (!inherits(discourse_obj, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  inputs <- discourse_obj$inputs
  if (is.null(inputs$max_iter) || is.null(inputs$init_temp) || is.null(inputs$cooling_rate)) {
    stop("Missing 'max_iter', 'init_temp', or 'cooling_rate' in inputs.")
  }
  if (!requireNamespace("ggplot2", quietly=TRUE)) {
    stop("`ggplot2` is needed to plot summaries; please install it.")
  }
  max_iter     <- inputs$max_iter
  init_temp    <- inputs$init_temp
  cooling_rate <- inputs$cooling_rate

  iterations    <- seq_len(max_iter)
  temperatures  <- init_temp * cooling_rate^iterations
  squared_temps <- temperatures^2

  df <- data.frame(
    Iteration   = iterations,
    Temperature = squared_temps
  )

  apa_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
      axis.title       = ggplot2::element_text(),
      axis.text        = ggplot2::element_text(),
    )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Iteration, y = .data$Temperature)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Cooling Schedule",
      x     = "Iteration",
      y     = "Temperature"
    ) +
    apa_theme +
    ggplot2::coord_cartesian(clip = "off")

  print(p)
}
