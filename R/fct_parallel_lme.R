#' Run mixed-effects linear regression optimization in parallel
#'
#' Executes multiple starts of `optim_lme` across available cores and returns either all results or the best solution.
#'
#' @param sim_data Data frame or list; input data for mixed-effects modeling.
#' @param target_cor Numeric; target correlation(s) to achieve.
#' @param target_reg Numeric; target regression coefficient(s).
#' @param reg_equation Formula or character; regression model specification.
#' @param target_se Numeric, optional; target standard error(s) for coefficients.
#' @param weight Numeric or NA; observation weights.
#' @param max_iter Integer; maximum number of iterations per start (default 1e5).
#' @param init_temp Numeric; initial temperature for simulated annealing.
#' @param cooling_rate Numeric; decay rate of temperature.
#' @param tol Numeric; convergence tolerance (default 1e-6).
#' @param prob_global_move Numeric; probability of global move in annealing (default 0.1).
#' @param rcpp Logical; use C++ objective implementation (default TRUE).
#' @param starts Integer; number of optimization restarts (default 1).
#' @param parallel_start Integer; number of parallel runs to launch.
#' @param hill_climbs Integer or NA; number of hill-climb refinements post-annealing.
#' @param prob_within_move Numeric; probability of within-neighborhood move (default 0.8).
#' @param pnumber Character or factor; grouping variable name for participants.
#' @param timepoint Character or factor; grouping variable name for time points.
#' @param return_best_solution Logical; if TRUE, return only the best fit (default FALSE).
#'
#' @return A list of `discourse.object` results from each parallel start,
#'         or a single `discourse.object` if `return_best_solution = TRUE`.
#' @export
parallel_lme <- function(
    sim_data,
    target_cor,
    target_reg,
    reg_equation,
    target_se = NULL,
    weight = NA,
    max_iter = 1e5,
    init_temp = 1,
    cooling_rate = NA,
    tol = 1e-6,
    prob_global_move = 0.1,
    rcpp = TRUE,
    starts = 1,
    parallel_start = 3,
    hill_climbs = NA,
    prob_within_move = 0.8,
    pnumber,
    timepoint,
    return_best_solution = FALSE
) {
  # detect number of cores
  cores <- parallel::detectCores() - 1

  # create and register cluster
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)

  # ensure cluster stop and cleanup on exit
  on.exit({
    parallel::stopCluster(cl)
    gc()
  })

  # packages to load on workers
  pkgs <- c("discourse", "Rcpp")

  # run optim_lme in parallel
  values <- foreach::foreach(
    i = seq_len(parallel_start),
    .packages = pkgs,
    .errorhandling = "pass"
  ) %dopar% {
    discourse::optim_lme(
      sim_data         = sim_data,
      target_cor       = target_cor,
      target_reg       = target_reg,
      reg_equation     = reg_equation,
      target_se        = target_se,
      weight           = weight,
      max_iter         = max_iter,
      init_temp        = init_temp,
      cooling_rate     = cooling_rate,
      tol              = tol,
      prob_global_move = prob_global_move,
      rcpp             = rcpp,
      starts           = starts,
      hill_climbs      = hill_climbs,
      prob_within_move = prob_within_move,
      pnumber          = pnumber,
      timepoint        = timepoint,
      progress_bar     = FALSE
    )
  }

  # return best solution if requested
  if (return_best_solution) {
    errors <- vapply(
      values,
      function(x) {
        if (inherits(x, "error")) {
          NA_real_
        } else {
          x$best_error
        }
      },
      numeric(1)
    )
    idx <- which.min(errors)
    return(values[[idx]])
  }

  # return all parallel results
  return(values)
}
