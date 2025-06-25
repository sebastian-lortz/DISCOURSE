#' Plot RMSE Comparison for discourse.object runs
#'
#' Visualizes the root-mean-square error (RMSE) distributions for between-run variability
#' versus deviations from target values across multiple `discourse.object` runs (e.g. parallel_aov()).
#'
#' @param object_list A `discourse.object` or list thereof, typically output from `optim_*` functions.
#'
#' @return A `ggplot2` object.
#'
#' @example
#' results <- parallel_aov(args = ..., ...)
#' plot_rmse(results)
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
plot_rmse <- function(object_list) {

  # input check
  if (!is.list(object_list)) object_list <- list(object_list)
  if (!all(sapply(object_list, function(x) is.list(x) && inherits(x, "discourse.object")))) {
    stop("object_list must be a list of objects of class 'discourse.object'.")
  }
  if (!requireNamespace("ggplot2", quietly=TRUE)) {
    stop("`ggplot2` is needed to plot summaries; please install it.")
  }

  # theme
  apa_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Helvetica") +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
      axis.title       = ggplot2::element_text(color = "black"),
      axis.text        = ggplot2::element_text(color = "black"),
    )

  # data
  obj <- get_rmse_parallel(object_list)
  dr <- obj$data_rmse
  metrics <- if (is.list(dr$between)) names(dr$between) else "rmse_F"
  rmse_data <- do.call(rbind, lapply(metrics, function(metric) {
    if (is.list(dr$between)) {
      between_vals <- dr$between[[metric]]
      target_vals  <- dr$target[[metric]]
    } else {
      between_vals <- dr$between
      target_vals  <- dr$target
    }
    if (all(is.na(between_vals)) && all(is.na(target_vals))) return(NULL)
    df_between <- data.frame(Metric = metric, Type = "Between", RMSE = between_vals)
    df_target  <- data.frame(Metric = metric, Type = "Target",  RMSE = target_vals)
    rbind(df_between, df_target)
  }))
  rmse_data <- rmse_data[!is.na(rmse_data$RMSE), ]
  if (nrow(rmse_data) == 0) stop("No RMSE data available to plot.")

  # plot
  p <- ggplot2::ggplot(rmse_data, ggplot2::aes(x = .data$Type, y = .data$RMSE, fill = .data$Type)) +
    ggplot2::geom_boxplot(width = 0.6, alpha = 0.4, outlier.shape = NA) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.25, color = "black") +
    ggplot2::geom_jitter(ggplot2::aes(color = .data$Type), width = 0.15, height = 0, alpha = 0.8, size = 2) +
    ggplot2::facet_wrap(~ Metric, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("Between" = "darkgrey", "Target" = "darkgrey")) +
    ggplot2::scale_color_manual(values = c("Between" = "steelblue", "Target" = "steelblue")) +
    ggplot2::labs(
      title = "RMSE Comparison: Target vs. Between Runs",
      x     = "RMSE Type",
      y     = "RMSE"
    ) +
    apa_theme +
    ggplot2::theme(
      legend.position    = "none",
      strip.background   = ggplot2::element_rect(fill = "gray90", color = "gray50"),
      strip.text         = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )
  return(p)
}
