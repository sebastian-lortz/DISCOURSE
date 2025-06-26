#' Optimize multiple simulated data sets to match target correlations and fixed-effects regression fits
#'
#' Uses the DISCOURSE algorithmic framework to simulate multiple data sets
#' such that the resulting correlations and regression coefficients
#' match specified targets under a given regression model and input parameters.
#'
#' @param parallel_start Number of independent runs (parallel or sequential) to simulate data sets.
#' @param return_best_solution Logical; return only the best run if TRUE.
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
#' @param max_starts Integer. Number of annealing restarts. Default `1`.
#' @param hill_climbs Integer or NULL. Number of hill‐climbing iterations for optional local refinement; if NULL, skips refinement. Default `NULL`.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#' @return A list of multiple `discourse.object`s, each containing:
#' \describe{
#'   \item{best_error}{Numeric. Minimum objective error achieved.}
#'   \item{data}{Data frame of optimized predictor and outcome values.}
#'   \item{inputs}{List of all input parameters for reproducibility.}
#'   \item{track_error}{Numeric vector of best error at each iteration of annealing.}
#'   \item{track_error_ratio}{Numeric vector of error ratios (cor vs. reg) per iteration.}
#' }
#'
#' @examples
#'  \dontrun{
#' # Optimize given sim_data from the Descriptives module matching means and SDs.
#' res <- parallel_lm(
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
    tolerance = 1e-6,
    prob_global_move = 0.1,
    max_starts = 1,
    hill_climbs = NULL,
    min_decimals = 1,
    progress_mode = c("console")
) {

  # input check
  if (!is.numeric(parallel_start) || length(parallel_start) != 1 ||
      parallel_start < 1 || parallel_start != as.integer(parallel_start)) {
    stop("`parallel_start` must be a single positive integer indicating the number of parallel runs.")
  }
  if (!is.logical(return_best_solution) || length(return_best_solution) != 1) {
    stop("`return_best_solution` must be a single logical value (TRUE or FALSE).")
  }
  pred       <- ncol(sim_data)
  exp_cor <- pred*(pred-1)/2
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
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("`tolerance` must be a single non-negative numeric value.")
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
     hill_climbs > 0 && hill_climbs == as.integer(hill_climbs))
  )) {
    stop("`hill_climbs` must be NULL or a single positive integer.")
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
            res <- optim_lm(
              sim_data         = sim_data,
              target_cor       = target_cor,
              target_reg       = target_reg,
              reg_equation     = reg_equation,
              target_se        = target_se,
              weight           = weight,
              max_iter         = max_iter,
              init_temp        = init_temp,
              cooling_rate     = cooling_rate,
              tolerance              = tolerance,
              prob_global_move = prob_global_move,
              max_starts       = max_starts,
              hill_climbs      = hill_climbs,
              progress_bar     = TRUE,
              min_decimals     = min_decimals
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
