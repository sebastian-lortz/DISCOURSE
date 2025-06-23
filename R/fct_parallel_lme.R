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
#' @param max_starts Integer; number of optimization restarts (default 1).
#' @param parallel_start Integer; number of parallel runs to launch.
#' @param hill_climbs Integer or NA; number of hill-climb refinements post-annealing.
#' @param return_best_solution Logical; if TRUE, return only the best fit (default FALSE).
#' @param move_prob List containing start and end probabilities of move selection
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the targets
#'
#' @importFrom foreach %dopar%
#'
#' @return A list of `discourse.object` results from each parallel start,
#'         or a single `discourse.object` if `return_best_solution = TRUE`.
#' @export
parallel_lme <- function(
    parallel_start,
    return_best_solution,
    sim_data,
    target_cor,
    target_reg,
    reg_equation,
    target_se = NULL,
    weight = NA,
    max_iter = 1e4,
    init_temp = 1,
    cooling_rate = NULL,
    tol = 1e-6,
    max_starts = 1,
    hill_climbs = NULL,
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
  if (!is.data.frame(sim_data) || nrow(sim_data) < 1) {
    stop("`sim_data` must be a non-empty data frame.")
  }
  if (!is.character(reg_equation) || length(reg_equation) != 1) {
    stop("`reg_equation` must be a single character string representing the model formula.")
  }

  if (!is.numeric(target_cor) || !any(!is.na(target_cor))) {
    stop("`target_cor` must contain at least one non-NA numeric value.")
  }
  if (!any(!is.na(target_reg))) {
    stop("`target_reg` must contain at least one non-NA numeric value.")
  }
  if (!is.null(target_se) && (!is.numeric(target_se) || length(target_se) != length(target_reg)-1)) {
    stop("`target_se`, if provided, must be a numeric vector matching the length of `target_reg` minus 1 (SD of random intercept).")
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
  required_moves <- c("residual", "k_cycle", "local", "tau")
  if (!is.list(move_prob) || !all(c("start", "end") %in% names(move_prob))) {
    stop("`move_prob` must be a list with components `start` and `end`.")
  }
  if (!is.numeric(move_prob$start) || !is.numeric(move_prob$end)) {
    stop("Both `move_prob$start` and `move_prob$end` must be numeric vectors.")
  }
  if (!setequal(names(move_prob$start), required_moves) ||
      !setequal(names(move_prob$end),   required_moves)) {
    stop(
      "`move_prob$start` and `move_prob$end` must each have exactly these names: ",
      paste(required_moves, collapse = ", ")
    )
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

  # Define packages and fct for parallel workers
  pkgs <- c("discourse", "Rcpp")

  cat("\nParallel optimization is running...\n")
  start_time <- Sys.time()

  # run optim_lme in parallel
  values <- foreach::foreach(
    i = 1:parallel_start,
    .packages = pkgs,
    .errorhandling = "pass"
  ) %dopar% {
    optim_lme(
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
      max_starts       = max_starts,
      hill_climbs      = hill_climbs,
      move_prob        = move_prob,
      progress_bar     = FALSE
    )
  }
  str(values)

  cat(" finished.\n")
  stop_time <- Sys.time()
  cat("\nParallel optimization time was", stop_time - start_time, "\n")
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
    result <- values[[idx]]
    class(result) <- "discourse.object"
    return(result)

  }

  # return all parallel results
  return(values)
}
