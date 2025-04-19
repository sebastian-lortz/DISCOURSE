#' Compute RMSE metrics across discourse.object runs
#' @param object_list A discourse.object or list thereof
#' @return List with between-run and target RMSE summaries and raw values
#' @export
get_rmse_parallel <- function(object_list) {
  # ensure input is a list
  if (!is.list(object_list)) object_list <- list(object_list)
  # validate each element is a discourse.object
  if (!all(sapply(object_list, function(x) is.list(x) && inherits(x, "discourse.object")))) {
    stop("object_list must be a list of objects of class 'discourse.object'.")
  }
  first_obj <- object_list[[1]]

  # branch 1: regression-based objects
  if (!is.null(first_obj$inputs$target_reg)) {
    # collect computed stats from runs
    opt_metrics <- lapply(object_list, get_stats)
    n_runs      <- length(opt_metrics)
    # determine rounding precision
    target_cor <- first_obj$inputs$target_cor
    target_reg <- first_obj$inputs$target_reg
    target_se  <- first_obj$inputs$target_se
    cor_dec    <- max(count_decimals(target_cor))
    reg_dec    <- max(count_decimals(target_reg))
    se_dec     <- if (!is.null(target_se)) max(count_decimals(target_se))
    # round each run's metrics
    opt_metrics <- lapply(opt_metrics, function(run) {
      list(
        cor = if (!is.null(target_cor)) round(run$cor, cor_dec),
        reg = if (!is.null(target_reg)) round(run$reg, reg_dec),
        se  = if (!is.null(target_se))  round(run$se,  se_dec)
      )
    })
    # helper: summarize an RMSE vector
    calc_summary <- function(vec) {
      if (all(is.na(vec)) || length(vec) == 0) {
        c(Mean_RMSE = NA, SD_RMSE = NA, Min_RMSE = NA, Max_RMSE = NA)
      } else {
        c(
          Mean_RMSE = mean(vec, na.rm = TRUE),
          SD_RMSE   = stats::sd(vec,   na.rm = TRUE),
          Min_RMSE  = min(vec,  na.rm = TRUE),
          Max_RMSE  = max(vec,  na.rm = TRUE)
        )
      }
    }
    # compute between-run RMSEs
    between_differences <- lapply(opt_metrics, function(run) {
      # recompute overall means inside loop (equivalent to original overall_mean)
      overall_cor <- Reduce("+", lapply(opt_metrics, `[[`, "cor")) / n_runs
      overall_reg <- Reduce("+", lapply(opt_metrics, `[[`, "reg")) / n_runs
      overall_se  <- Reduce("+", lapply(opt_metrics, `[[`, "se"))  / n_runs
      diff_cor <- if (!is.null(target_cor)) run$cor - overall_cor
      diff_reg <- if (!is.null(target_reg)) run$reg - overall_reg
      diff_se  <- if (!is.null(target_se))  run$se  - overall_se
      list(
        rmse_cor = sqrt(mean(diff_cor^2, na.rm = TRUE)),
        rmse_reg = sqrt(mean(diff_reg^2, na.rm = TRUE)),
        rmse_se  = sqrt(mean(diff_se^2,  na.rm = TRUE))
      )
    })
    rmse_cor_vec <- sapply(between_differences, `[[`, "rmse_cor")
    rmse_reg_vec <- sapply(between_differences, `[[`, "rmse_reg")
    rmse_se_vec  <- sapply(between_differences, `[[`, "rmse_se")
    # assemble between-run summary
    between_summary <- data.frame(
      Metric    = c("rmse_cor","rmse_reg","rmse_se"),
      Mean_RMSE = c(calc_summary(rmse_cor_vec)["Mean_RMSE"],
                    calc_summary(rmse_reg_vec)["Mean_RMSE"],
                    calc_summary(rmse_se_vec)["Mean_RMSE"]),
      SD_RMSE   = c(calc_summary(rmse_cor_vec)["SD_RMSE"],
                    calc_summary(rmse_reg_vec)["SD_RMSE"],
                    calc_summary(rmse_se_vec)["SD_RMSE"]),
      Min_RMSE  = c(calc_summary(rmse_cor_vec)["Min_RMSE"],
                    calc_summary(rmse_reg_vec)["Min_RMSE"],
                    calc_summary(rmse_se_vec)["Min_RMSE"]),
      Max_RMSE  = c(calc_summary(rmse_cor_vec)["Max_RMSE"],
                    calc_summary(rmse_reg_vec)["Max_RMSE"],
                    calc_summary(rmse_se_vec)["Max_RMSE"])
    )
    # compute target-run RMSEs
    target_differences <- lapply(opt_metrics, function(run) {
      diff_cor <- run$cor - target_cor
      diff_reg <- run$reg - target_reg
      diff_se  <- run$se  - target_se
      list(
        rmse_cor = sqrt(mean(diff_cor^2, na.rm = TRUE)),
        rmse_reg = sqrt(mean(diff_reg^2, na.rm = TRUE)),
        rmse_se  = sqrt(mean(diff_se^2,  na.rm = TRUE))
      )
    })
    target_rmse_cor_vec <- sapply(target_differences, `[[`, "rmse_cor")
    target_rmse_reg_vec <- sapply(target_differences, `[[`, "rmse_reg")
    target_rmse_se_vec  <- sapply(target_differences, `[[`, "rmse_se")
    # assemble target-run summary
    target_summary <- data.frame(
      Metric    = c("rmse_cor","rmse_reg","rmse_se"),
      Mean_RMSE = c(calc_summary(target_rmse_cor_vec)["Mean_RMSE"],
                    calc_summary(target_rmse_reg_vec)["Mean_RMSE"],
                    calc_summary(target_rmse_se_vec)["Mean_RMSE"]),
      SD_RMSE   = c(calc_summary(target_rmse_cor_vec)["SD_RMSE"],
                    calc_summary(target_rmse_reg_vec)["SD_RMSE"],
                    calc_summary(target_rmse_se_vec)["SD_RMSE"]),
      Min_RMSE  = c(calc_summary(target_rmse_cor_vec)["Min_RMSE"],
                    calc_summary(target_rmse_reg_vec)["Min_RMSE"],
                    calc_summary(target_rmse_se_vec)["Min_RMSE"]),
      Max_RMSE  = c(calc_summary(target_rmse_cor_vec)["Max_RMSE"],
                    calc_summary(target_rmse_reg_vec)["Max_RMSE"],
                    calc_summary(target_rmse_se_vec)["Max_RMSE"])
    )
    # raw RMSE values
    data_rmse <- list(between = list(rmse_cor = rmse_cor_vec,
                                     rmse_reg = rmse_reg_vec,
                                     rmse_se  = rmse_se_vec),
                      target  = list(rmse_cor = target_rmse_cor_vec,
                                     rmse_reg = target_rmse_reg_vec,
                                     rmse_se  = target_rmse_se_vec))
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))

  } else if (!is.null(first_obj$inputs$target_f_vec)) {
    # branch 2: ANOVA-based objects
    target_F_values <- first_obj$inputs$target_f_vec$F
    F_dec           <- max(count_decimals(target_F_values))
    opt_metrics     <- lapply(object_list, function(res) {
      stats <- get_stats(res)
      list(F_value = round(stats$F_value, F_dec))
    })
    # between-run RMSE for F
    overall_F          <- mean(sapply(opt_metrics, `[[`, "F_value"))
    between_differences <- sapply(opt_metrics, function(run) sqrt((run$F_value - overall_F)^2))
    between_summary    <- data.frame(
      Metric    = "rmse_F",
      Mean_RMSE = mean(between_differences, na.rm = TRUE),
      SD_RMSE   = stats::sd(between_differences,   na.rm = TRUE),
      Min_RMSE  = min(between_differences,  na.rm = TRUE),
      Max_RMSE  = max(between_differences,  na.rm = TRUE)
    )
    # target-run RMSE for F
    target_differences <- sapply(opt_metrics, function(run) sqrt((run$F_value - target_F_values)^2))
    target_summary     <- data.frame(
      Metric    = "rmse_F",
      Mean_RMSE = mean(target_differences, na.rm = TRUE),
      SD_RMSE   = stats::sd(target_differences,   na.rm = TRUE),
      Min_RMSE  = min(target_differences,  na.rm = TRUE),
      Max_RMSE  = max(target_differences,  na.rm = TRUE)
    )
    # raw F RMSEs
    data_rmse <- list(between = between_differences, target = target_differences)
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))

  } else {
    # branch 3: vector-based objects
    target_mean <- first_obj$inputs$target_mean
    target_sd   <- first_obj$inputs$target_sd
    mean_dec    <- max(count_decimals(target_mean))
    sd_dec      <- max(count_decimals(target_sd))
    opt_metrics <- lapply(object_list, function(res) {
      stats <- get_stats(res)
      list(mean = round(stats$mean, mean_dec), sd = round(stats$sd, sd_dec))
    })
    n_runs        <- length(opt_metrics)
    overall_mean <- c(
      mean = mean(sapply(opt_metrics, `[[`, "mean")),
      sd   = mean(sapply(opt_metrics, `[[`, "sd"))
    )
    # between-run RMSE
    between_differences <- lapply(opt_metrics, function(run) {
      diff_mean <- run$mean - overall_mean["mean"]
      diff_sd   <- run$sd   - overall_mean["sd"]
      list(rmse_mean = sqrt(mean(diff_mean^2, na.rm = TRUE)),
           rmse_sd   = sqrt(mean(diff_sd^2,   na.rm = TRUE)))
    })
    between_matrix  <- do.call(rbind, lapply(between_differences, unlist))
    between_summary <- data.frame(
      Metric    = rownames(between_matrix),
      Mean_RMSE = rowMeans(between_matrix, na.rm = TRUE),
      SD_RMSE   = apply(between_matrix, 2, stats::sd,   na.rm = TRUE),
      Min_RMSE  = apply(between_matrix, 2, min,  na.rm = TRUE),
      Max_RMSE  = apply(between_matrix, 2, max,  na.rm = TRUE)
    )
    # target-run RMSE
    target_differences <- lapply(opt_metrics, function(run) {
      diff_mean <- run$mean - target_mean
      diff_sd   <- run$sd   - target_sd
      list(rmse_mean = sqrt(mean(diff_mean^2, na.rm = TRUE)),
           rmse_sd   = sqrt(mean(diff_sd^2,   na.rm = TRUE)))
    })
    target_matrix    <- do.call(rbind, lapply(target_differences, unlist))
    target_summary   <- data.frame(
      Metric    = rownames(target_matrix),
      Mean_RMSE = rowMeans(target_matrix, na.rm = TRUE),
      SD_RMSE   = apply(target_matrix, 2, stats::sd,   na.rm = TRUE),
      Min_RMSE  = apply(target_matrix, 2, min,  na.rm = TRUE),
      Max_RMSE  = apply(target_matrix, 2, max,  na.rm = TRUE)
    )
    # raw mean/sd RMSEs
    data_rmse <- list(between = between_differences, target = target_differences)
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))
  }
}
