#' Summarize a discourse.object
#' @param object A `discourse.object` to summarize
#' @param ...   Additional arguments (unused)
#' @return An object of class `summary.discourse.object`
#' @export
summary.discourse.object <- function(object, ...) {

  if (!inherits(object, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  stats <- get_stats(object)
  rmse  <- get_rmse(object)

  if (!is.null(object$inputs$reg_equation)) {
    summary_obj <- list(
      rmse              = rmse,
      inputs            = object$inputs,
      data              = object$data,
      best_error        = object$best_error,
      track_error       = object$track_error,
      track_error_ratio = object$track_error_ratio,
      model             = stats$model,
      coefficients      = stats$reg,
      std_errors        = stats$se,
      correlations      = stats$cor,
      means             = stats$mean,
      sds               = stats$sd
    )

  } else if (!is.null(object$inputs$target_f_list)) {
    summary_obj <- list(
      rmse       = rmse,
      inputs     = object$inputs,
      data       = object$data,
      best_error = object$best_error,
      track_error= object$track_error,
      model      = stats$model,
      F_value    = stats$F_value,
      means      = stats$mean
    )

  } else {
    summary_obj <- list(
      rmse       = rmse,
      inputs     = object$inputs,
      data       = object$data,
      best_error = object$best_error,
      track_error= object$track_error,
      means      = stats$mean,
      sds        = stats$sd
    )
  }

  class(summary_obj) <- "summary.discourse.object"
  summary_obj
}
