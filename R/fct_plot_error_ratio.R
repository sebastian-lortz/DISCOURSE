#' plot_error_ratio
#'
#' @description Plot error ratio over iterations for a discourse object.
#'
#' @param discourse_obj A discourse object with track_error_ratio data.
#' @param run Integer index for list-based track_error_ratio (default = 1).
#' @param show_mean Logical; add mean line (default = TRUE).
#' @param show_median Logical; add median line (default = TRUE).
#' @param show_final Logical; add final value line (default = TRUE).
#'
#' @return A ggplot2 plot of error ratio evolution.
#' @importFrom rlang .data
#' @export
plot_error_ratio <- function(discourse_obj, run = 1,
                             show_mean = TRUE,
                             show_median = TRUE,
                             show_final = TRUE) {

  # theme setup
  apa_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title       = ggplot2::element_text(color = "black"),
      axis.text        = ggplot2::element_text(color = "black"),
      plot.margin      = ggplot2::margin(5.5, 5.5, 5.5, 70)
    )

  # validate input
  if (!inherits(discourse_obj, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  if (is.null(discourse_obj$track_error_ratio)) {
    stop("Only discourse.object from optim_lm or optim_lme have a track_error_ratio element.")
  }

  # extract error data
  err_data <- discourse_obj$track_error_ratio
  if (is.list(err_data)) {
    if (length(err_data) < run) stop("The specified run does not exist in track_error_ratio.")
    err_vec <- err_data[[run]]
  } else {
    err_vec <- err_data
  }

  # prepare data frame
  df <- data.frame(
    Iteration = seq_along(err_vec),
    Error     = err_vec
  )

  # compute stats
  mean_err    <- round(mean(err_vec),  1)
  median_err  <- round(stats::median(err_vec),1)
  final_ratio <- round(utils::tail(err_vec, 1),1)

  # base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Iteration, y = .data$Error)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::labs(
      title = "Error Ratio Cor/Reg of Objective Function",
      x     = "Iteration",
      y     = "Error Ratio Cor/Reg"
    ) +
    apa_theme +
    ggplot2::coord_cartesian(clip = "off")

  # stats lines setup
  df_stats <- data.frame(stat = character(), y = numeric(), linetype = character(), stringsAsFactors = FALSE)
  if (show_mean)   df_stats <- rbind(df_stats, data.frame(stat = paste0("Mean: ",    mean_err),    y = mean_err,    linetype = "dashed"))
  if (show_median) df_stats <- rbind(df_stats, data.frame(stat = paste0("Median: ", median_err), y = median_err, linetype = "dotted"))
  if (show_final)  df_stats <- rbind(df_stats, data.frame(stat = paste0("Final: ",  final_ratio), y = final_ratio, linetype = "solid"))

  # add stat lines
  if (nrow(df_stats) > 0) {
    p <- p + ggplot2::geom_hline(
      data    = df_stats,
      mapping = ggplot2::aes(yintercept = .data$y, color = .data$stat, linetype = .data$stat),
      size    = 1
    ) +
      ggplot2::scale_color_manual(name = "", values = rep("darkgray", nrow(df_stats)), breaks = df_stats$stat) +
      ggplot2::scale_linetype_manual(name = "", values = stats::setNames(df_stats$linetype, df_stats$stat), breaks = df_stats$stat) +
      ggplot2::theme(legend.position = "bottom")
  }

  print(p)
}
