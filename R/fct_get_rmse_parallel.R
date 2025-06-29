#' Compute RMSE metrics across discourse.object runs
#'
#' Calculates root-mean-square error (RMSE) metrics for both between-run variability and deviations
#' from specified target values across one or more `discourse.object` results.
#'
#' @param object_list A `discourse.object` or list thereof. Objects produced by analysis functions such as `parallel_aov()`, `parallel_lm()`, `parallel_lme()`.
#'
#' @return A list with components:
#' \describe{
#'   \item{between_rmse}{A `data.frame` summarizing RMSE metrics (Mean_RMSE, SD_RMSE, Min_RMSE, Max_RMSE) for between-run differences (not taking into account targets).}
#'   \item{target_rmse}{A `data.frame` summarizing RMSE metrics for deviations from the original target inputs (not taking into account the mean across runs).}
#'   \item{data_rmse}{A `list` with raw RMSE values:
#'     \describe{
#'       \item{between}{Named numeric vector of between-run RMSEs (`rmse_cor`, `rmse_reg`, etc.).}
#'       \item{target}{Named numeric vector of target-run RMSEs.}
#'     }
#'   }
#' }
#'
#' @examples
#'  \dontrun{
#' # Multiple-run RMSE comparison
#' result <- parallel_lm(args = ..., ...)
#' get_rmse_parallel(result)
#' }
#' @export
get_rmse_parallel <- function(object_list) {
  # input checks
  if (!is.list(object_list)) object_list <- list(object_list)
  if (!all(sapply(object_list, function(x) is.list(x) && inherits(x, "discourse.object")))) {
    stop("object_list must be a list of objects of class 'discourse.object'.")
  }
  first_obj <- object_list[[1]]

  # LM and LME module
  if (!is.null(first_obj$inputs$target_reg)) {
    opt_metrics <- lapply(object_list, get_stats)
    n_runs      <- length(opt_metrics)
    # rounding
    target_cor <- first_obj$inputs$target_cor
    target_reg <- first_obj$inputs$target_reg
    target_se  <- first_obj$inputs$target_se
    cor_dec    <- max(count_decimals(target_cor))
    reg_dec    <- max(count_decimals(target_reg))
    se_dec     <- if (!is.null(target_se)) max(count_decimals(target_se))
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
    # between-run RMSE
    between_differences <- lapply(opt_metrics, function(run) {
      na.cor <- is.na(target_cor)
      na.reg <- is.na(target_reg)
      if (!is.null(target_se)) na.se <- is.na(target_se)
      overall_cor <- round(Reduce("+", lapply(opt_metrics, `[[`, "cor")) / n_runs, cor_dec)
      overall_reg <- round(Reduce("+", lapply(opt_metrics, `[[`, "reg")) / n_runs, reg_dec)
      if (!is.null(target_se)) overall_se  <- round(Reduce("+", lapply(opt_metrics, `[[`, "se"))  / n_runs, se_dec)
      diff_cor <-  run$cor[!na.cor] - overall_cor[!na.cor]
      diff_reg <-  run$reg[!na.reg] - overall_reg[!na.reg]
      diff_se  <- if (!is.null(target_se))  run$se[!na.se]  - overall_se[!na.se]
      list(
        rmse_cor = sqrt(mean(diff_cor^2, na.rm = TRUE)),
        rmse_reg = sqrt(mean(diff_reg^2, na.rm = TRUE)),
        rmse_se  = sqrt(mean(diff_se^2,  na.rm = TRUE))
      )
    })
    rmse_cor_vec <- sapply(between_differences, `[[`, "rmse_cor")
    rmse_reg_vec <- sapply(between_differences, `[[`, "rmse_reg")
    rmse_se_vec  <- sapply(between_differences, `[[`, "rmse_se")
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
    # target-run RMSE
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
    # combine output
    data_rmse <- list(between = list(rmse_cor = rmse_cor_vec,
                                     rmse_reg = rmse_reg_vec,
                                     rmse_se  = rmse_se_vec),
                      target  = list(rmse_cor = target_rmse_cor_vec,
                                     rmse_reg = target_rmse_reg_vec,
                                     rmse_se  = target_rmse_se_vec))
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))

  } else if (!is.null(first_obj$inputs$target_f_list)) {
    # aov module
    target_F_values <- first_obj$inputs$target_f_list$F
    F_dec           <- max(count_decimals(target_F_values))
    opt_metrics     <- lapply(object_list, function(res) {
      stats <- get_stats(res)
      round(stats$F_value, F_dec)
    })
    # between-run RMSE
    F_mat <- do.call(rbind, opt_metrics)
    overall_F <- colMeans(F_mat, na.rm = TRUE)
    between_differences <- apply(F_mat, 1, function(F_run) {
      sqrt( mean((F_run - overall_F)^2, na.rm = TRUE) )
    })
    between_summary    <- data.frame(
      Metric    = "rmse_F",
      Mean_RMSE = mean(between_differences, na.rm = TRUE),
      SD_RMSE   = stats::sd(between_differences,   na.rm = TRUE),
      Min_RMSE  = min(between_differences,  na.rm = TRUE),
      Max_RMSE  = max(between_differences,  na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    # target-run RMSE
    target_differences <- apply(F_mat, 1, function(F_run) {
      sqrt( mean((F_run - target_F_values)^2, na.rm = TRUE) )
    })
    target_summary     <- data.frame(
      Metric    = "rmse_F",
      Mean_RMSE = mean(target_differences, na.rm = TRUE),
      SD_RMSE   = stats::sd(target_differences,   na.rm = TRUE),
      Min_RMSE  = min(target_differences,  na.rm = TRUE),
      Max_RMSE  = max(target_differences,  na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    # combine output
    data_rmse <- list(between = between_differences, target = target_differences)
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))
  } else {
    # vec module
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
    # combine output
    data_rmse <- list(between = between_differences, target = target_differences)
    return(list(between_rmse = between_summary,
                target_rmse  = target_summary,
                data_rmse    = data_rmse))
  }
}
