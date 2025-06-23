#' parallel_aov: parallel ANOVA optimization
#' @description Run ANOVA optimization across multiple parallel workers
#' @param N Total sample size of participants
#' @param levels Vector specifying number of levels for each factor
#' @param subgroup_sizes Optional subgroup sizes for unbalanced designs
#' @param target_group_means Vector of target means for each group
#' @param target_f_list List with target F-values and related parameters
#' @param df_effects Degrees of freedom for each effect
#' @param range Numeric vector of length 2 specifying candidate value range
#' @param formula Model formula for computing F statistics
#' @param tolerance Convergence tolerance for the optimization
#' @param factor_type Vector indicating factor types ("between"/"within")
#' @param typeSS Type of sums of squares (e.g., 3 for Type III)
#' @param max_iter Maximum iterations for simulated annealing
#' @param max_step Proportion; The maximum step size for modifications as proportion of the range
#' @param init_temp Initial temperature for annealing
#' @param cooling_rate Cooling rate per iteration
#' @param integer Logical; treat candidate values as integers if TRUE
#' @param max_starts Number of restart cycles
#' @param checkGrim Logical; perform GRIM consistency checks if TRUE
#' @param parallel_start Number of independent parallel runs
#' @param return_best_solution Logical; return only the best run if TRUE
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the targets
#'
#' @importFrom foreach %dopar%
#'
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
    min_decimals = 1
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
  if (!is.numeric(levels) || length(levels) < 1) {
    stop("`levels` must be a numeric vector of length > 0 specifying factor levels per factor.")
  }
  if (!is.numeric(target_group_means) || length(target_group_means) < 1) {
    stop("`target_group_means` must be a non-empty numeric vector of target means.")
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

  old_plan <- future::plan()
  on.exit( future::plan(old_plan), add = TRUE )

  cat("There are ", future::availableCores() , "available workers. \n")
  real_cores <- future::availableCores()
  n_workers  <- min(parallel_start, real_cores)
  if (n_workers > 1L) {
    future::plan(future::multisession, workers = n_workers)
  } else {
    future::plan(future::sequential)
  }
  cat("Running with", n_workers, "worker(s). \n")

  pkgs <- c("discourse", "Rcpp")

  cat("\nParallel optimization is running...\n")
  start_time <- Sys.time()

  # parallel optim_aov
  values <- future.apply::future_lapply(
    X           = seq_len(parallel_start),
    FUN         = function(i) {
                                optim_aov(
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
                             },
  future.seed = TRUE    # safe RNG in each worker
  #packages    = pkgs,
  #error       = function(e) e
  )


  cat(" finished.\n")
  stop_time <- Sys.time()
  cat("\nParallel optimization time was", stop_time - start_time, "\n")

  # return best solution if requested
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
