#' Optimize multiple data sets to match ANOVA F-values
#'
#' Uses the DISCOURSE algorithmic framework to simulate multiple data sets that
#' produce target ANOVA F-statistics under a specified factorial design given input parameters.
#'
#' @param N Integer. Total number of subjects (sum of `subgroup_sizes`).
#' @param levels Integer vector. Number of factor levels per factor in the design.
#' @param subgroup_sizes Numeric vector. Optional sizes of each between-subjects group for unbalanced designs; length must equal product of `levels` for between factors.
#' @param target_group_means Numeric vector. Desired means for each group in the design.
#' @param target_f_list List with components:
#'   \describe{
#'     \item{F}{Numeric vector of target F-statistics.}
#'     \item{effect}{Character vector of effect names matching `F`.}
#'     \item{contrast}{Optional character formula for contrasts.}
#'     \item{contrast_method}{Optional character specifying contrast method.}
#'   }
#' @param parallel_start Number of independent runs (parallel or sequential) to simulate data sets.
#' @param return_best_solution Logical; return only the best run if TRUE.
#' @param formula Formula or character. Model formula used to compute F-values (e.g., `y ~ A + B + A*B`).
#' @param factor_type Character vector. Type of each factor (`"between"` or `"within"`) matching length of `levels`.
#' @param range Numeric vector of length 2. Lower and upper bounds for candidate means.
#' @param integer Logical. If TRUE, candidate values are treated as integers, if FALSE treated as continuous values.
#' @param typeSS Integer. Type of sums-of-squares for ANOVA (2 or 3). Default is 3.
#' @param df_effects Numeric vector. Degrees of freedom of the model effects. Default is `NULL`.
#' @param max_iter Integer. Maximum iterations per restart. Default is 1e3.
#' @param max_starts Integer. Number of annealing restarts. Default is 1.
#' @param init_temp Numeric. Initial temperature for annealing. Default is 1.
#' @param cooling_rate Numeric. Cooling rate per iteration (between 0 and 1); if NULL, calculated automatically as `(init_temp-10)/init_temp`.
#' @param max_step Numeric. Maximum move size as proportion of `range`. Default is 0.2.
#' @param tolerance Numeric. Error tolerance for convergence; stops early if best error < `tolerance`. Default `1e-6`.
#' @param checkGrim Logical. If TRUE and `integer = TRUE`, perform GRIM checks on `target_group_means`. Default is FALSE.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#' @return A list of multiple `discourse.object`s, each containing:
#' \describe{
#'   \item{best_error}{Numeric. Minimum error (RMSE) achieved.}
#'   \item{data}{Data frame of optimized outcome values (and grouping variables).}
#'   \item{inputs}{List of all input arguments.}
#'   \item{track_error}{Numeric vector of best error at each iteration.}
#'   \item{grim}{List of the GRIM results.}
#' }
#'
#' @examples
#'  \dontrun{
#' # Balanced 2x2 design
#' parallel_aov(
#'   parallel_start = 7,
#'   return_best_solution = FALSE,
#'   N = 40,
#'   levels = c(2, 2),
#'   target_group_means = c(1, 2, 3, 4),
#'   target_f_list = list(effect = c("A", "B"),
#'                        F = c(5.6, 8.3), ),
#'   formula = y ~ A + B + A*B,
#'   factor_type = c("between", "between"),
#'   range = c(0, 5),
#'   integer = FALSE,
#'   max_iter = 1000,
#'   max_starts = 3
#' )
#' }
#' @export
parallel_aov <- function(
    parallel_start = 3,
    return_best_solution = FALSE,
    N,
    levels,
    target_group_means,
    target_f_list,
    integer,
    range,
    formula,
    factor_type,
    subgroup_sizes = NULL,
    df_effects = NULL,
    tolerance = 1e-8,
    typeSS = 3,
    max_iter = 1e3,
    init_temp = 1,
    cooling_rate = NULL,
    max_step = .2,
    max_starts = 1,
    checkGrim = FALSE,
    min_decimals = 1,
    progress_mode = "console"
) {
  # input check
  if (!is.numeric(parallel_start) || length(parallel_start) != 1 ||
      parallel_start < 1 || parallel_start != as.integer(parallel_start)) {
    stop("`parallel_start` must be a single positive integer indicating the number of parallel runs.")
  }
  if (!is.logical(return_best_solution) || length(return_best_solution) != 1) {
    stop("`return_best_solution` must be a single logical value (TRUE or FALSE).")
  }
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer.")
  }
  if (
    !is.numeric(levels) ||
    length(levels) < 1 ||
    any(levels != as.integer(levels))
  ) {
    stop("`levels` must be a numeric vector of integers (e.g. 1, 2, 3) with length > 0, specifying the number of levels per factor.")
  }
  if (!is.numeric(target_group_means) || length(target_group_means) < 1) {
    stop("`target_group_means` must be a non-empty numeric vector of target means.")
  }
  if (any(target_group_means < range[1] | target_group_means > range[2])) {
    stop("All `target_group_means` must be lie within the specified range.")
  }
  if (!is.list(target_f_list) ||
      !is.numeric(target_f_list$F) || length(target_f_list$F) < 1) {
    stop("`target_f_list` must be a list with a numeric vector `F` of target F statistics.")
  }
  if (!is.character(target_f_list$effect) ||
      length(target_f_list$effect) != length(target_f_list$F)) {
    stop("`target_f_list$effect` must be a character vector the same length as `target_f_list$F`.")
  }
  if (!is.character(formula) || length(formula) != 1) {
    stop("`formula` must be a single character string giving the regression formula.")
  }
  if (!is.character(factor_type) ||
      length(factor_type) != length(levels) ||
      !all(factor_type %in% c("between", "within"))) {
    stop("`factor_type` must be a character vector ('between'/'within') matching length of `levels`.")
  }
  n_between <- prod(levels[factor_type == "between"])
  if (!is.null(subgroup_sizes) &&
      (!is.numeric(subgroup_sizes) ||
       length(subgroup_sizes) != n_between)) {
    stop("`subgroup_sizes`, if provided, must be a numeric vector matching the number of between-subject groups.")
  }
  if (!is.null(subgroup_sizes)) {
    if (N != sum(subgroup_sizes)) {
      stop("`N` must equal the sum of `subgroup_sizes` and thus, the number of subjects (not observations).")
    }
  }
  if (!is.null(df_effects) &&
      (!is.numeric(df_effects) ||
       length(df_effects) != length(target_f_list$F))) {
    stop("`df_effects`, if provided, must be a numeric vector the same length as `target_f_list$F`.")
  }
  if (!is.numeric(range) || length(range) != 2) {
    stop("`range` must be a numeric vector of length 2 specifying allowed candidate range.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("`tolerance` must be a single non-negative numeric value.")
  }
  if (!is.numeric(typeSS) || length(typeSS) != 1 || !typeSS %in% c(2, 3)) {
    stop("`typeSS` must be either 2 or 3.")
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    stop("`max_iter` must be a single positive integer (or numeric convertible to integer).")
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    stop("`init_temp` must be a single positive numeric value.")
  }
  if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
         cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate))) {
    stop("`cooling_rate` must be a single numeric between 0 and 1, or NULL.")
  }
  if (!is.numeric(max_step) || length(max_step) != 1 ||
      max_step <= 0 || max_step >= 1) {
    stop("`max_step` must be a single numeric between 0 and 1.")
  }
  if (!is.logical(integer) || length(integer) != 1) {
    stop("`integer` must be a single logical value.")
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    stop("`max_starts` must be a single positive integer.")
  }
  if (!is.logical(checkGrim) || length(checkGrim) != 1) {
    stop("`checkGrim` must be a single logical value.")
  }
  if (!is.numeric(min_decimals) || length(min_decimals) != 1 ||
      min_decimals < 0 || min_decimals != as.integer(min_decimals)) {
    stop("`min_decimals` must be a single non-negative integer.")
  }

  # set up backend
  old_plan <- future::plan()
  on.exit( future::plan(old_plan), add = TRUE )
  cat("There are ", future::availableCores() , "available workers. \n")
  real_cores <- future::availableCores()
  n_workers  <- min(parallel_start, max(real_cores-1,1))
  if (n_workers > 1L) {
    future::plan(future::multisession, workers = n_workers)
  } else {
    future::plan(future::sequential)
  }
  cat("Running with", n_workers, "worker(s). \n")
  pkgs <- c("discourse", "Rcpp")
  cat("\nParallel optimization is running...\n")
  start_time <- Sys.time()

  # set progressr
  if(progress_mode == "shiny") {
    handler <- list(progressr::handler_shiny())
  } else {
    handler <-list(progressr::handler_txtprogressbar())
  }

  # Optimization process
  values <- progressr::with_progress({
  p <- progressr::progressor(steps = parallel_start)
    future.apply::future_lapply(
      X           = seq_len(parallel_start),
      FUN         = function(i) {
                              res <- optim_aov(
                                  N       = N,
                                  levels            = levels,
                                  subgroup_sizes    = subgroup_sizes,
                                  target_group_means= target_group_means,
                                  target_f_list      = target_f_list,
                                  df_effects        = df_effects,
                                  range             = range,
                                  formula           = formula,
                                  tolerance         = tolerance,
                                  factor_type       = factor_type,
                                  typeSS            = typeSS,
                                  max_iter          = max_iter,
                                  init_temp         = init_temp,
                                  cooling_rate      = cooling_rate,
                                  progress_bar      = FALSE,
                                  integer           = integer,
                                  max_starts        = max_starts,
                                  checkGrim         = checkGrim,
                                  max_step          = max_step
                                )
                                p()
                                res
                               },
    future.seed = TRUE
    )
    },
  handlers = handler)

  cat(" finished.\n")
  stop_time <- Sys.time()
  cat("\nParallel optimization time was", stop_time - start_time, "\n")

  # assemble output
  if (return_best_solution) {
    errors <- sapply(values, function(x) {
      if (inherits(x, "error")) NA_real_ else x$best_error
    })
    if (all(is.na(errors))) {
      stop("All parallel iterations failed.")
    } else {
      idx <- which.min(errors)
      result <- values[[idx]]
      class(result) <- "discourse.object"
      return(result)
    }
  } else {
    return(values)
  }
}
