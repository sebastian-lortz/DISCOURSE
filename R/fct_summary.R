#' Summarize a discourse.object
#'
#' Provides a comprehensive summary of a `discourse.object`, including RMSE,
#' model components, and statistics depending on the analysis type (LM/LME, ANOVA, or vec).
#'
#' @param object A `discourse.object` produced by `optim_*` functions.
#' @param ...   Additional arguments (unused).
#'
#' @return An object of class `summary.discourse.object` containing:
#' \describe{
#'   \item{rmse}{Named list of RMSE values.}
#'   \item{inputs}{Original input list.}
#'   \item{data}{Optimized data frame.}
#'   \item{best_error}{Numeric. Minimum objective error achieved.}
#'   \item{track_error}{Numeric vector of best error at each iteration.}
#'   \item{track_error_ratio}{Numeric vector of error ratios (cor vs. reg) per iteration (if available).}
#'   \item{model}{Fitted regression model or ANOVA table (if applicable).}
#'   \item{coefficients}{regression estimates (LM/LME only).}
#'   \item{std_errors}{Coefficient standard errors (LM/LME only).}
#'   \item{correlations}{Correlation values (LM/LME only).}
#'   \item{F_value}{ANOVA F statistics (ANOVA only).}
#'   \item{means}{Group or variable means.}
#'   \item{sds}{Group or variable standard deviations (vector only).}
#' }
#'
#' @examples
#'  \dontrun{
#' res_lm <- optim_lm(args = ..., ...)
#' summary(res_lm)
#' }
#' @export
summary.discourse.object <- function(object, ...) {

  # input checks
  if (!inherits(object, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  stats <- get_stats(object)
  rmse  <- get_rmse(object)

  if (!is.null(object$inputs$reg_equation)) {

    # lm and lme module
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

    # aov module
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

    # vec module
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
