#' Parallel Linear Model Optimization
#'
#' @description Run parallelized optimization for linear model parameters.
#' @param sim_data Simulation data.
#' @param target_cor Target correlation.
#' @param target_reg Target regression.
#' @param reg_equation Regression equation.
#' @param target_se Optional standard error target.
#' @param weight Numeric vector of length 2: run weights.
#' @param max_iter Maximum number of iterations.
#' @param init_temp Initial temperature.
#' @param cooling_rate Cooling schedule rate.
#' @param tol Convergence tolerance.
#' @param prob_global_move Probability of global move.
#' @param max_starts Number of starts per run.
#' @param parallel_start Number of parallel runs.
#' @param hill_climbs Number of hill climbs.
#' @param return_best_solution Logical; return best solution only.
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the targets
#' @return List of optimization results or single best result.
#'
#' @importFrom foreach %dopar%
#'
#' @export
parallel_lm <- function(
    parallel_start,
    return_best_solution,
    sim_data,
    target_cor,
    target_reg,
    reg_equation,
    target_se = NULL,
    weight = c(1, 1),
    max_iter = 1e5,
    init_temp = 1,
    cooling_rate = NULL,
    tol = 1e-6,
    prob_global_move = 0.1,
    max_starts = 1,
    hill_climbs = NULL,
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
  p       <- ncol(sim_data)
  exp_cor <- p*(p-1)/2
  term_lbls<- attr(stats::terms(stats::as.formula(reg_equation)), "term.labels")
  exp_reg  <- length(term_lbls) + 1
  if (length(target_cor) != exp_cor) {
    stop(sprintf("`target_cor` must be a numeric vector of length %d, not %d.",
                 exp_cor, length(target_cor)))
  }
  if (!is.numeric(target_cor) || !any(!is.na(target_cor))) {
    stop("`target_cor` must contain at least one non-NA numeric value.")
  }
  if (!is.numeric(target_reg) || length(target_reg) != exp_reg) {
    stop(sprintf("`target_reg` must be a numeric vector of length %d, not %d.",
                 exp_reg, length(target_reg)))
  }
  if (!any(!is.na(target_reg))) {
    stop("`target_reg` must contain at least one non-NA numeric value.")
  }
  if (!is.null(target_se)) {
    if (!is.numeric(target_se) || length(target_se) != length(target_reg)) {
      stop("`target_se`, if provided, must be a numeric vector the same length as `target_reg`.")
    }
  }
  if (!is.numeric(weight) || length(weight) != 2) {
    stop("`weight` must be a numeric vector of length 2 (correlation vs. regressino error weights).")
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    stop("`max_iter` must be a single positive integer (or numeric convertible to integer).")
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    stop("`init_temp` must be a single positive numeric value.")
  }
  if (!(
    (is.numeric(cooling_rate) && length(cooling_rate) == 1 && cooling_rate > 0 && cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    stop("`cooling_rate` must be a single numeric between 0 and 1, or NULL.")
  }
  if (!is.numeric(tol) || length(tol) != 1 || tol < 0) {
    stop("`tol` must be a single non-negative numeric value.")
  }
  if (!is.numeric(prob_global_move) || length(prob_global_move) != 1 ||
      prob_global_move < 0 || prob_global_move > 1) {
    stop("`prob_global_move` must be a single numeric between 0 and 1.")
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    stop("`max_starts` must be a single positive integer.")
  }
  if (!(
    is.null(hill_climbs) ||
    (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
     hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))
  )) {
    stop("`hill_climbs` must be NULL or a single non-negative integer.")
  }
  if (!is.numeric(min_decimals) || length(min_decimals) != 1 ||
      min_decimals < 0 || min_decimals != as.integer(min_decimals)) {
    stop("`min_decimals` must be a single non-negative integer.")
  }

  # Setup parallel backend
  cores <- parallel::detectCores() - 1
  cl    <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  cat("\nParallel backend registered with:", cores, "cores.\n")

  # ensure cluster stop and cleanup on exit
  on.exit({
    parallel::stopCluster(cl)
    gc()
  })

  # Define packages for parallel workers
  pkgs <- c("discourse", "Rcpp")

  cat("\nParallel optimization is running...\n")
  start_time <- Sys.time()

  # Run parallel optimization
  values <- foreach::foreach(i = 1:parallel_start,
                             .packages = pkgs,
                             .errorhandling = "pass") %dopar% {
                               optim_lm(
                                 sim_data = sim_data,
                                 target_cor = target_cor,
                                 target_reg = target_reg,
                                 reg_equation = reg_equation,
                                 target_se = target_se,
                                 weight = weight,
                                 max_iter = max_iter,
                                 init_temp = init_temp,
                                 cooling_rate = cooling_rate,
                                 tol = tol,
                                 prob_global_move = prob_global_move,
                                 max_starts = max_starts,
                                 hill_climbs = hill_climbs,
                                 progress_bar = FALSE
                               )
                             }
  cat(" finished.\n")
  stop_time <- Sys.time()
  cat("\nParallel optimization time was", stop_time - start_time, "\n")

  # Return results
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
