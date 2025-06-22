#' Estimate simulation weights via simulated annealing
#'
#' @description
#' Run multiple simulated‑annealing optimizations (LM or LME) to find weights that
#' reproduce target correlations and regression coefficients.
#'
#' @param module     Character; either `"lm"` or `"lme"` to select the model type.
#' @param sim_runs   Integer; number of simulation runs.
#' @param sim_data   Data frame or matrix of predictors and outcome.
#' @param target_cor Numeric vector of target correlations.
#' @param target_reg Numeric vector of target regression coefficients.
#' @param target_se  Numeric vector of target standard errors (optional).
#' @param reg_equation Character string of the regression formula.
#' @param max_iter   Integer; maximum number of iterations (default `1e5`).
#' @param init_temp  Numeric; initial temperature for simulated annealing.
#' @param cooling_rate Numeric; per‑iteration cooling rate.
#' @param tol        Numeric; convergence tolerance.
#' @param prob_global_move Numeric; probability of taking a global move.
#' @param progress_bar   Logical; if `TRUE`, print progress.
#' @param weight     Numeric vector; initial weights (default `c(1,1)`).
#' @param pool_range Integer; how many recent error ratios to average.
#' @param max_starts     Integer; number of restarts.
#' @param parallel_start Integer; number of parallel workers (0 = sequential).
#' @param move_prob List containing start and end probabilities of move selection
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the targets
#'
#' @return
#' A list with components:
#' \describe{
#'   \item{weights}{Numeric vector of estimated weights.}
#'   \item{best_solution}{Best solution object from the last optimizer run.}
#' }
#'
#' @export
weights_est <- function(module,
                        sim_runs,
                        sim_data,
                        target_cor,
                        target_reg,
                        target_se = NULL,
                        reg_equation,
                        max_iter = 1e5,
                        init_temp = 1,
                        cooling_rate = NULL,
                        tol = 1e-6,
                        prob_global_move = 0.05,
                        progress_bar = TRUE,
                        weight = c(1, 1),
                        pool_range = 10,
                        max_starts = 1,
                        parallel_start = 1,
                        move_prob = list(
                          start = c(residual = 0.00,
                                    k_cycle  = 0.00,
                                    local    = 0.25,
                                    tau      = 0.75),
                          end   = c(residual = 0.20,
                                    k_cycle  = 0.10,
                                    local    = 0.70,
                                    tau      = 0.00)
                        ),
                        min_decimals = 0
) {

  # input checks
  if (!is.character(module) || length(module) != 1 || !module %in% c("lm", "lme")) {
    stop("`module` must be a single string: either \"lm\" or \"lme\".")
  }
  if (!is.numeric(sim_runs) || length(sim_runs) != 1 ||
      sim_runs < 1 || sim_runs != as.integer(sim_runs)) {
    stop("`sim_runs` must be a single positive integer indicating the number of simulation runs.")
  }

  error_ratios <- numeric(sim_runs)
  last_opt_run <- NULL

  for (runs in seq_len(sim_runs)) {
    if (parallel_start == 1) {
      if (module == "lme") {
        opt_run <- optim_lme(
          sim_data         = sim_data,
          target_cor       = target_cor,
          target_reg       = target_reg,
          reg_equation     = reg_equation,
          target_se        = target_se,
          weight           = c(1, 1),
          max_iter         = max_iter,
          init_temp        = init_temp,
          cooling_rate     = cooling_rate,
          tol              = tol,
          progress_bar     = progress_bar,
          max_starts       = max_starts,
          hill_climbs      = NULL,
          move_prob        = move_prob,
          min_decimals     = min_decimals
          )
      } else {
        opt_run <- optim_lm(
          sim_data         = sim_data,
          target_cor       = target_cor,
          target_reg       = target_reg,
          reg_equation     = reg_equation,
          target_se        = target_se,
          weight           = c(1, 1),
          max_iter         = max_iter,
          init_temp        = init_temp,
          cooling_rate     = cooling_rate,
          tol              = tol,
          prob_global_move = prob_global_move,
          progress_bar     = progress_bar,
          max_starts       = max_starts,
          hill_climbs      = NULL,
          min_decimals     = min_decimals
        )
      }
    } else {
      if (module == "lme") {
        opt_run <- parallel_lme(
          sim_data             = sim_data,
          target_cor           = target_cor,
          target_reg           = target_reg,
          reg_equation         = reg_equation,
          target_se            = target_se,
          weight               = weight,
          max_iter             = max_iter,
          init_temp            = init_temp,
          cooling_rate         = cooling_rate,
          tol                  = tol,
          max_starts           = max_starts,
          parallel_start       = parallel_start,
          hill_climbs          = NULL,
          return_best_solution = TRUE,
          move_prob            = move_prob,
          min_decimals     = min_decimals
        )
      } else {
        opt_run <- parallel_lm(
          sim_data             = sim_data,
          target_cor           = target_cor,
          target_reg           = target_reg,
          reg_equation         = reg_equation,
          target_se            = target_se,
          weight               = weight,
          max_iter             = max_iter,
          init_temp            = init_temp,
          cooling_rate         = cooling_rate,
          tol                  = tol,
          prob_global_move     = prob_global_move,
          max_starts           = max_starts,
          parallel_start       = parallel_start,
          hill_climbs          = NULL,
          return_best_solution = TRUE,
          min_decimals     = min_decimals
          )
      }
    }

    last_opt_run <- opt_run

    errs <- unique(opt_run$track_error_ratio)
    n_errs <- length(errs)
    if (n_errs > pool_range) {
      error_ratios[runs] <- mean(errs[(n_errs - pool_range + 1):n_errs], na.rm = TRUE)
    } else {
      error_ratios[runs] <- mean(errs, na.rm = TRUE)
    }
  }

  mean_error <- mean(error_ratios)
  weights <- round(c(mean_error, 1) / min(mean_error, 1), 3)

  cat("The estimated weights are:", weights, "\n")
  list(
    weights       = weights,
    data = last_opt_run$data,
    track_error = last_opt_run$track_error,
    error_ratio = error_ratios
  )
}
