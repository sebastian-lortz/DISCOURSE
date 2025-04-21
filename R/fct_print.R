#' Print Summary of a Discourse Object
#'
#' @description
#' A minimal S3 print method for summarizing objects of class `summary.discourse.object`.
#'
#' @param x An object of class `summary.discourse.object`, produced by the Discourse analysis routines.
#' @param ... Additional arguments (ignored).
#'
#' @return
#' Invisibly returns the input object `x` after printing its summary.
#'
#' @method print summary.discourse.object
#' @exportS3Method print summary.discourse.object
print.summary.discourse.object <- function(x, ...) {
  cat("DISCOURSE Object Summary\n")
  cat("-------------------------------------------------\n\n")

  if (!is.null(x$inputs$reg_equation)) {
    # regression‐model branch
    cat("Achieved Loss of Optimization: ", x$best_error, "\n\n")
    cat("RMSE of Summary Statistics\n")
    cat("  Correlations:            ", x$rmse$rmse_cor, "\n")
    cat("  Regression Coefficients: ", x$rmse$rmse_reg, "\n")
    cat("  Standard Errors:         ", x$rmse$rmse_se, "\n\n")

    cat("Regression Model:\n")
    cat(paste(deparse(x$inputs$reg_equation), collapse = "\n"), "\n\n")

    cat("Simulated Data Summary:\n")
    cat("  Coefficients:\n"); print(x$coefficients)
    cat("\n  Std. Errors:\n");   print(x$std_errors)
    cat("\n  Correlations:\n");  print(x$correlations)
    cat("\n  Means:\n");         print(x$means)
    cat("\n  SDs:\n");           print(x$sds)

  } else if (!is.null(x$inputs$target_f_vec)) {
    # factorial‐model branch
    cat("Achieved Loss of Optimization: ", x$best_error, "\n\n")
    cat("RMSE of F statistics: ", x$rmse$rmse_F, "\n\n")

    cat("Factorial Model:\n")
    cat(paste(deparse(x$inputs$formula), collapse = "\n"), "\n\n")

    cat("Group Means:\n"); print(x$means)

  } else {
    # optim_vec branch
    best_err <- unlist(x$best_error)
    names(best_err) <- names(x$means)
    cat("Achieved Loss of Optimization:\n"); print(best_err)
    cat("\nRMSE of Summary Statistics\n")
    cat("  Means: ", x$rmse$rmse_mean, "\n")
    cat("  SDs:   ", x$rmse$rmse_sd, "\n\n")

    cat("Means:\n"); print(x$means)
    cat("\nSDs:\n");   print(x$sds)
  }

  invisible(x)
}
