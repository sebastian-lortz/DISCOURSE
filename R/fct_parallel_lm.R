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
#' @param rcpp Logical; use Rcpp.
#' @param starts Number of starts per run.
#' @param parallel_start Number of parallel runs.
#' @param hill_climbs Number of hill climbs.
#' @param return_best_solution Logical; return best solution only.
#' @return List of optimization results or single best result.
#' @importFrom foreach %dopar%
#' @export
parallel_lm <- function(sim_data, target_cor,
                        target_reg,
                        reg_equation,
                        target_se = NULL,
                        weight = c(1, 1),
                        max_iter = 1e5,
                        init_temp = 1,
                        cooling_rate = NA,
                        tol = 1e-6,
                        prob_global_move = 0.1,
                        rcpp = TRUE,
                        starts = 1,
                        parallel_start = 3,
                        hill_climbs = NA,
                        return_best_solution = FALSE) {
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
                                 rcpp = rcpp,
                                 starts = starts,
                                 hill_climbs = hill_climbs,
                                 progress_bar = FALSE
                               )
                             }
  cat(" finished.\n")
  stop_time <- Sys.time()
  cat("\nParallel optimization time was", stop_time - start_time, "seconds.\n")

  # Return results
  if (return_best_solution) {
    errors <- sapply(values, function(x) {
      if (inherits(x, "error")) NA_real_ else x$best_error
    })
    if (all(is.na(errors))) {
      stop("All parallel iterations failed.")
    } else {
      idx <- which.min(errors)
      return(values[[idx]])
    }
  } else {
    return(values)
  }
}
