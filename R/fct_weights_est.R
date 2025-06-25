#' Estimate objective function weights via pilot simulations
#'
#' Runs single or multiple optimizations (i.e. optim_lm, optim_lme) to calibrate the balance between correlation and regression
#' components in the objective function, yielding suggested weights.
#'
#' @param module     Character; either `"lm"` or `"lme"` to select the module type.
#' @param sim_runs   Integer; number of simulation runs.
#' @param pool_range Integer; the range of best error values to pool for estimating the weights. Default is `10`.
#' @param sim_data Data frame. Predictor variables and outcome to be optimized; at least two columns.
#' @param target_cor Numeric vector. Target upper-triangular (excluding diagonal) correlation values for predictor and outcome variables.
#' @param target_reg Numeric vector. Target regression coefficients including intercept, matching terms in `reg_equation`.
#' @param reg_equation Character or formula. Regression model (e.g., "Y ~ X1 + X2 + X1:X2").
#' @param target_se Numeric vector, optional. Target standard errors for regression coefficients (same length as `target_reg`).
#' @param weight Numeric vector of length 2. Weights for correlation vs. regression error in the objective function. Default `c(1, 1)`.
#' @param max_iter Integer. Maximum iterations for simulated annealing per start. Default `1e5`.
#' @param init_temp Numeric. Initial temperature for annealing. Default `1`.
#' @param cooling_rate Numeric or NULL. Cooling rate per iteration (0–1); if NULL, computed as `(max_iter - 10) / max_iter`.
#' @param tolerance Numeric. Error tolerance for convergence; stops early if best error < `tolerance`. Default `1e-6`.
#' @param prob_global_move Numeric (0–1). Probability of a global shuffle move vs. local swap. Default `0.1`.
#' @param progress_bar Logical. Show text progress bar during optimization. Default `TRUE`.
#' @param max_starts Integer. Number of annealing restarts. Default `1`.
#' @param parallel_start Number of independent runs (parallel or sequential) to simulate the weights.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#' @param move_prob List. Start/end move probabilities for operations: residual swap, k-cycle, local swap, tau reordering.
#'
#' @return A list with components:
#' \describe{
#'   \item{weights}{Numeric vector of estimated weights for (correlation/regression).}
#'   \item{data}{The optimized data set from the final run.}
#'   \item{track_error}{Numeric vector of best error at each iteration of annealing.}
#'   \item{error_ratio}{Numeric vector of pilot-run error ratios used in weight estimation.}
#'   }
#'
#' @examples
#'  \dontrun{
#' # estimate weights of objective function
#' parallel_lm(
#' sim_runs = 1,
#'   parallel_start = 7,
#'   return_best_solution = FALSE,
#'   sim_data = sim_data,
#'   target_cor = c(.23),
#'   target_reg = c(2.1, 1.2, -0.8),
#'   reg_equation = "Y ~ X1 + X2",
#'   max_iter = 10000,
#'   hill_climbs = 50
#' )
#' }
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
                        tolerance = 1e-6,
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
                        min_decimals = 0,
                        progress_mode = "console"
) {

  # input checks
  if (!is.character(module) || length(module) != 1 || !module %in% c("lm", "lme")) {
    stop("`module` must be a single string: either \"lm\" or \"lme\".")
  }
  if (!is.numeric(sim_runs) || length(sim_runs) != 1 ||
      sim_runs < 1 || sim_runs != as.integer(sim_runs)) {
    stop("`sim_runs` must be a single positive integer indicating the number of simulation runs.")
  }

  # storage
  error_ratios <- numeric(sim_runs)
  last_opt_run <- NULL

  # optimization process
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
          tolerance              = tolerance,
          progress_bar     = progress_bar,
          max_starts       = max_starts,
          hill_climbs      = NULL,
          move_prob        = move_prob,
          min_decimals     = min_decimals,
          progress_mode    = progress_mode
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
          tolerance              = tolerance,
          prob_global_move = prob_global_move,
          progress_bar     = progress_bar,
          max_starts       = max_starts,
          hill_climbs      = NULL,
          min_decimals     = min_decimals,
          progress_mode    = progress_mode
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
          tolerance                  = tolerance,
          max_starts           = max_starts,
          parallel_start       = parallel_start,
          hill_climbs          = NULL,
          return_best_solution = TRUE,
          move_prob            = move_prob,
          min_decimals         = min_decimals,
          progress_mode        = progress_mode
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
          tolerance                  = tolerance,
          prob_global_move     = prob_global_move,
          max_starts           = max_starts,
          parallel_start       = parallel_start,
          hill_climbs          = NULL,
          return_best_solution = TRUE,
          min_decimals         = min_decimals,
          progress_mode        = progress_mode
          )
      }
    }

    # combine results
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

  # assemble output
  list(
    weights       = weights,
    data = last_opt_run$data,
    track_error = last_opt_run$track_error,
    error_ratio = error_ratios
  )
}
