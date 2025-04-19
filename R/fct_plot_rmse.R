#' Plot RMSE Comparison
#'
#' @description Plot RMSE distributions (Between vs. Target) across metrics.
#' @param object_list List of discourse objects.
#' @return ggplot2 object
#' @importFrom rlang .data
#' @export
plot_rmse <- function(object_list) {
  # Retrieve RMSE data
  obj <- get_rmse_parallel(object_list)

  # Extract raw RMSE values
  dr <- obj$data_rmse

  # Determine metrics
  metrics <- if (is.list(dr$between)) names(dr$between) else "rmse_F"

  # Build long-format data frame
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

  # Filter out NA values and validate
  rmse_data <- rmse_data[!is.na(rmse_data$RMSE), ]
  if (nrow(rmse_data) == 0) stop("No RMSE data available to plot.")

  # Create plot
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
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position    = "none",
      strip.background   = ggplot2::element_rect(fill = "gray90", color = "gray50"),
      strip.text         = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )

  return(p)
}
