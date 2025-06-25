#' Plot Summary of Simulated vs. Target Statistics for a discourse.object
#'
#' Visualizes discrepancies between simulated and target summary statistics
#' (e.g., means, SDs, F-values, correlations, regression coefficients) for a single `discourse.object`.
#'
#' @param discourse_obj A `discourse.object` produced by `optim_*` functions,
#'   containing both simulated data/results and `inputs$target_*` values.
#' @param standardised Logical; if `TRUE`, differences are divided by target values
#'   (except when targets are near zero); default `TRUE`.
#' @param eps Numeric; threshold below which a target is treated as zero for standardisation;
#'   default `1e-12`.
#'
#' @return A `ggplot2` object.
#'
#' @example
#' res <- optim_aov(...)
#' plot_summary(res, standardised = FALSE)
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @import dplyr
#' @export
plot_summary <- function(discourse_obj, standardised = TRUE, eps = 1e-12) {

  # input check
  if (!inherits(discourse_obj, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  if (!is.list(discourse_obj) || is.null(discourse_obj$inputs) || !is.list(discourse_obj$inputs)) {
    stop("`discourse_obj` must be a list containing an 'inputs' list element.")
  }
  if (!is.logical(standardised) || length(standardised) != 1) {
    stop("`standardised` must be a single logical value.")
  }
  if (!is.numeric(eps) || length(eps) != 1) {
    stop("`eps` must be a single numeric value.")
  }
  if (!requireNamespace("ggplot2", quietly=TRUE)) {
    stop("`ggplot2` is needed to plot summaries; please install it.")
  }

  # theme
  apa_theme <- ggplot2::theme_minimal(base_size = 12, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      plot.title       = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
      axis.text        = ggplot2::element_text(color = "black")
    )
  y_label <- if (standardised) {
    "((Simulated - Target) / Target)"
  } else {
    "Simulated - Target"
  }
  title_prefix <- if (standardised) {
    "Standardised"
  } else {
    "Unstandardised"
  }

  # helper
  compute_centered <- function(sim, target) {
    if (standardised) {
      ifelse(abs(target) < eps, sim - target, (sim - target) / target)
    } else {
      sim - target
    }
  }

  # data lm and lme module
  if (!is.null(discourse_obj$inputs$target_reg)) {
    target_reg <- discourse_obj$inputs$target_reg
    target_cor <- discourse_obj$inputs$target_cor
    target_se  <- discourse_obj$inputs$target_se
    reg_dec <- max(count_decimals(target_reg))
    cor_dec <- max(count_decimals(target_cor))
    stats    <- get_stats(discourse_obj)
    sim_reg  <- stats$reg
    sim_cor  <- stats$cor
    sim_se   <- stats$se
    sim_reg_r <- round(sim_reg, reg_dec)
    sim_cor_r <- round(sim_cor, cor_dec)
    df_reg <- data.frame(
      Measure   = "Regression Coefficient",
      Variable  = names(target_reg),
      Simulated = sim_reg_r,
      Target    = target_reg,
      Centered  = compute_centered(sim_reg_r, target_reg),
      stringsAsFactors = FALSE
    )
    df_reg$SimulatedType <- ifelse(
      standardised & abs(df_reg$Target) < eps,
      "Unstandardized Diff", "Simulated"
    )
    var_names_cor <- names(target_cor)
    if (is.null(var_names_cor)) var_names_cor <- paste0("Cor", seq_along(target_cor))
    df_cor <- data.frame(
      Measure   = "Correlation",
      Variable  = var_names_cor,
      Simulated = sim_cor_r,
      Target    = target_cor,
      Centered  = compute_centered(sim_cor_r, target_cor),
      stringsAsFactors = FALSE
    )
    df_cor$SimulatedType <- ifelse(
      standardised & abs(df_cor$Target) < eps,
      "Unstandardized Diff", "Simulated"
    )
    if (!is.null(target_se)) {
      se_dec    <- max(count_decimals(target_se))
      sim_se_r  <- round(sim_se, se_dec)
      if (!is.null(names(target_se))) {
        var_names_se <- names(target_se)
      } else {
        var_names_se <- names(target_reg)[seq_along(target_se)]
      }
            df_se <- data.frame(
        Measure   = "Standard Error",
        Variable  = var_names_se,
        Simulated = sim_se_r,
        Target    = target_se,
        Centered  = compute_centered(sim_se_r, target_se),
        stringsAsFactors = FALSE
      )
      df_se$SimulatedType <- ifelse(
        standardised & abs(df_se$Target) < eps,
        "Unstandardized Diff", "Simulated"
      )
    } else {
      df_se <- NULL
    }
    df_all <- dplyr::bind_rows(df_reg, df_cor, df_se) %>%
      dplyr::group_by(.data$Measure) %>%
      dplyr::mutate(
        Variable = factor(.data$Variable, levels = unique(.data$Variable))
      ) %>%
      dplyr::ungroup()
    if (any(df_all$SimulatedType == "Unstandardized Diff", na.rm = TRUE)) {
      warning(
        "One or more target values were practically 0; ",
        "the unstandardised difference was computed for these values."
      )
    }

    # plot
    p <- ggplot2::ggplot(df_all, ggplot2::aes(x = .data$Variable, y = .data$Centered)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$SimulatedType), size = 4, na.rm = TRUE) +
      ggplot2::geom_segment(ggplot2::aes(x = .data$Variable, xend = .data$Variable, y = .data$Centered, yend = 0),
                   color = "gray50", linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      ggplot2::facet_wrap(~ .data$Measure, scales = "free_x") +
      ggplot2::scale_color_manual(name = "",
                         values = c("Simulated" = "steelblue",
                                    "Unstandardized Diff" = "red")) +
      ggplot2::labs(title = paste(title_prefix, "Difference of Summary Statistics"),
           y = y_label,
           x = "Parameter") +
      apa_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            strip.background = ggplot2::element_rect(fill = "gray90", color = "gray50"),
            strip.text = ggplot2::element_text(face = "bold"))
    return(p)

    # aov module
  } else if (!is.null(discourse_obj$inputs$target_f_list)) {
    target_F <- discourse_obj$inputs$target_f_list$F
    F_dec <- count_decimals(target_F)
    stats <- get_stats(discourse_obj)
    sim_F <- stats$F_value
    sim_F_r <- round(sim_F, F_dec)
    centered <- compute_centered(sim_F_r, target_F)
    effect_names <- names(target_F)
    if (is.null(effect_names)) effect_names <- paste0("Effect", seq_along(target_F))
    df <- data.frame(
      Measure   = "F Statistic",
      Variable  = effect_names,
      Simulated = sim_F_r,
      Target    = target_F,
      Centered  = centered,
      stringsAsFactors = FALSE
    )
    df$SimulatedType <- ifelse(standardised & abs(df$Target) < eps,
                               "Unstandardized Diff", "Simulated")
    if(any(df$SimulatedType == "Unstandardized Diff")){
      warning("One or more target values were practically 0; the unstandardised difference was computed for these values.")
    }

    # plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$Centered)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$SimulatedType), size = 4, na.rm = TRUE) +
      ggplot2::geom_segment(ggplot2::aes(x = .data$Variable, xend = .data$Variable, y = .data$Centered, yend = 0),
                   color = "gray50", linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      ggplot2::facet_wrap(~ .data$Measure, scales = "free_x") +
      ggplot2::scale_color_manual(name = "",
                         values = c("Simulated" = "steelblue",
                                    "Unstandardized Diff" = "red")) +
      ggplot2::labs(title = paste(title_prefix, "Difference of Summary Statistics"),
           y = y_label,
           x = "Effect") +
      apa_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            strip.background = ggplot2::element_rect(fill = "gray90", color = "gray50"),
            strip.text = ggplot2::element_text(face = "bold"))
    return(p)

    # vec module
  } else {
    target_mean <- discourse_obj$inputs$target_mean
    target_sd   <- discourse_obj$inputs$target_sd
    mean_dec <- count_decimals(target_mean)
    sd_dec   <- count_decimals(target_sd)
    stats <- get_stats(discourse_obj)
    sim_mean <- stats$mean
    sim_sd   <- stats$sd
    sim_mean_r <- round(sim_mean, mean_dec)
    sim_sd_r   <- round(sim_sd, sd_dec)
    centered_mean <- compute_centered(sim_mean_r, target_mean)
    centered_sd   <- compute_centered(sim_sd_r, target_sd)
    sim_data <- as.data.frame(discourse_obj$data)
    vars <- colnames(sim_data)
    if (is.null(vars)) vars <- names(target_mean)
    df <- data.frame(
      Variable  = rep(vars, 2),
      Measure   = rep(c("Mean", "SD"), each = length(vars)),
      Simulated = c(sim_mean_r, sim_sd_r),
      Target    = c(target_mean, target_sd),
      Centered  = c(centered_mean, centered_sd),
      stringsAsFactors = FALSE
    )
    df$SimulatedType <- ifelse(standardised & abs(df$Target) < eps,
                               "Unstandardized Diff", "Simulated")
    df <- df %>% dplyr::group_by(.data$Measure) %>%
      dplyr::mutate(Variable = factor(.data$Variable, levels = unique(.data$Variable))) %>%
      dplyr::ungroup()
    if(any(df$SimulatedType == "Unstandardized Diff")){
      warning("One or more target values were practically 0; the unstandardised difference was computed for these values.")
    }

    # plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$Centered)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$SimulatedType), size = 4, na.rm = TRUE) +
      ggplot2::geom_segment(ggplot2::aes(x = .data$Variable, xend = .data$Variable, y = .data$Centered, yend = 0),
                   color = "gray50", linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      ggplot2::facet_wrap(~ .data$Measure, scales = "free_x") +
      ggplot2::scale_color_manual(name = "",
                         values = c("Simulated" = "steelblue",
                                    "Unstandardized Diff" = "red")) +
      ggplot2::labs(title = paste(title_prefix,"Difference of Summary Statistics"),
           y = y_label,
           x = "Variable") +
      apa_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            strip.background = ggplot2::element_rect(fill = "gray90", color = "gray50"),
            strip.text = ggplot2::element_text(face = "bold"))
    return(p)
  }
}
