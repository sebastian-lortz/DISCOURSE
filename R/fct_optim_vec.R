#' Optimize a vector or matrix to match target means and SDs
#'
#' Uses the DISCOURSE algorithmic framework to simulate one or multiple
#' vectors so that each matches specified target means and standard deviations under given input parameters.
#'
#' @param N Integer. Number of values in each vector.
#' @param target_mean Named numeric vector. Desired means for each variable (names identify columns).
#' @param target_sd Named numeric vector. Desired standard deviations for each variable.
#' @param range Numeric vector of length 2 or numeric matrix. Allowed value range for all variables (vector),
#'   or per-variable bounds as a two-row matrix matching `target_mean`.
#' @param integer Logical or logical vector. If TRUE, optimize integer values; length 1 or same length as `target_mean`.
#' @param max_iter Integer. Maximum iterations for simulated annealing per start. Default `1e5`.
#' @param init_temp Numeric. Initial temperature for annealing. Default `1`.
#' @param cooling_rate Numeric or NULL. Cooling rate per iteration (0–1); if NULL, computed as `(max_iter - 10) / max_iter`.
#' @param tolerance Numeric. Error tolerance for convergence; stops early if best error < `tolerance`. Default `1e-12`.
#' @param int.probs List of numeric vectors, one per variable. Sampling probabilities for integer moves; NULL for uniform.
#' @param progress_bar Logical. Show text progress bar during optimization. Default `TRUE`.
#' @param obj_weight List of numeric vectors length 2, one per variable. Weights for mean vs. SD error. Default `list(c(1,1))`.
#' @param maxit_pso Integer. Maximum PSO iterations for continuous variables. Default `2000`.
#' @param eps Numeric. Small constant to avoid division by zero in objective. Default `1e-12`.
#' @param max_starts Integer. Number of annealing restarts. Default `3`.
#' @param checkGrim Logical. If TRUE and `integer = TRUE`, perform GRIM checks on `target_group_means`. Default is FALSE.
#' @param prob_heuristic Numeric. Probability of heuristic move vs. random swap in integer mode. Default `0.1`.
#' @param parallel Logical. If TRUE, optimize each variable in parallel. Default `FALSE`.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#' @return A `discourse.object` list containing:
#' \describe{ A list with the following elements for each variable:
#'     \item{best_error}{Numeric. Minimum objective error achieved.}
#'     \item{data}{Data.frame or matrix of optimized vectors (columns named by `target_mean`).}
#'     \item{track_error}{Numeric vector of best error at each iteration of annealing.}
#'     \item{inputs}{List of all input parameters for reproducibility.}
#'     \item{grim}{List of the GRIM results.}
#' }
#'
#' @examples
#'  \dontrun{
#' # Optimize a vector of length 100
#' res <- optim_vec(
#'   N            = 100,
#'   target_mean  = 10,
#'   target_sd    = 2,
#'   range        = c(0, 20),
#'   integer      = TRUE,
#'   max_iter     = 50000,
#'   max_starts   = 2
#' )
#' }
#' @export
optim_vec <- function(
    N,
    target_mean,
    target_sd,
    range,
    tolerance,
    integer,
    max_iter        = 1e5,
    init_temp       = 1,
    cooling_rate    = NULL,
    int.probs       = NULL,
    progress_bar    = TRUE,
    obj_weight      = list(c(1, 1)),
    maxit_pso       = 2000,
    eps             = 1e-12,
    max_starts      = 3,
    checkGrim       = TRUE,
    prob_heuristic  = 0.1,
    parallel        = FALSE,
    min_decimals = 1,
    progress_mode = "console"
) {
  ## Adjust default cooling rate
  if (is.null(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }

  # input checks
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer.")
  }
  if (!is.numeric(target_mean) || !is.numeric(target_sd) ||
      length(target_mean) != length(target_sd) || length(target_mean) < 1) {
    stop("`target_mean` and `target_sd` must be numeric vectors of the same positive length.")
  }
  if (is.null(names(target_mean)) || any(names(target_mean) == "")) {
    stop("`target_mean` must have non-empty names.")
  }
  if (!(is.numeric(range) && (length(range) == 2 || (is.matrix(range) && ncol(range) == length(target_mean))))) {
    stop("`range` must be a numeric vector of length 2 or a matrix with columns matching the length of `target_mean`.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("`tolerance` must be a single non-negative numeric value.")
  }
  if (!is.logical(integer) || length(integer) < 1 || (length(integer) != 1 & length(integer) != length(target_mean))) {
    stop("`integer` must be a logical vector (length 1 or length(target_mean)).")
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
  if (!is.null(int.probs) && (!is.list(int.probs) || length(int.probs) != length(target_mean))) {
    stop("`int.probs`, if provided, must be a list of sampling-probability vectors, one per variable.")
  }
  if (!is.logical(progress_bar) || length(progress_bar) != 1) {
    stop("`progress_bar` must be a single logical value.")
  }
  if (!is.list(obj_weight) || length(obj_weight) < length(target_mean) || !all(vapply(obj_weight, function(w) is.numeric(w) && length(w) == 2, logical(1)))) {
    stop("`obj_weight` must be a list of length = number of target variables, each a numeric weight vector with two elements. You can use weights_vec() to estimate weights.")
  }
  if (!is.numeric(maxit_pso) || length(maxit_pso) != 1 || maxit_pso <= 0) {
    stop("`maxit_pso` must be a single positive integer.")
  }
  if (!is.numeric(eps) || length(eps) != 1 || eps < 0) {
    stop("`eps` must be a single non-negative numeric value.")
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    stop("`max_starts` must be a single positive integer.")
  }
  if (!is.logical(checkGrim) || length(checkGrim) != 1) {
    stop("`checkGrim` must be a single logical value.")
  }
  if (!is.numeric(prob_heuristic) || length(prob_heuristic) != 1 ||
      prob_heuristic < 0 || prob_heuristic > 1) {
    stop("`prob_heuristic` must be a single numeric between 0 and 1.")
  }
  if (!is.logical(parallel) || length(parallel) != 1) {
    stop("`parallel` must be a single logical value.")
  }
  if (!is.numeric(min_decimals) || length(min_decimals) != 1 ||
      min_decimals < 0 || min_decimals != as.integer(min_decimals)) {
    stop("`min_decimals` must be a single non-negative integer.")
  }

  # transform input
  n_var <- length(target_mean)
  if (is.null(int.probs)) {
    int.probs <- vector("list", n_var)
    int.probs <- rep(list(NULL), n_var)
  }
  if (length(obj_weight) < n_var) {
    obj_weight <- rep(list(obj_weight), n_var)
  }
  if (length(integer) < n_var) {
    integer <- rep(integer[1], n_var)
  }

  # Single optimizatino process
  optim_vec_single <- function(
    N, target_mean, target_sd, range, tolerance,
    max_iter, init_temp, cooling_rate, int.probs,
    progress_bar, obj_weight, integer, maxit_pso,
    eps, max_starts, checkGrim, prob_heuristic
  ) {

    # get decimals
    mean_dec <- count_decimals(target_mean, min_decimals = min_decimals)
    sd_dec   <- count_decimals(target_sd, min_decimals = min_decimals)

    # GRIM
    if (checkGrim && integer) {
      grim <- check_grim(N, target_mean, mean_dec)
      cat(grim$message)
    } else {
      grim <- NULL
    }

    # Objective function
      objective <- function(x) {
        objective_cpp(x, target_mean, target_sd, obj_weight, eps, mean_dec, sd_dec)
      }

    # Optimization Integer
    if (integer) {
      allowed   <- seq(range[1], range[2])
      n_allowed <- length(allowed)
      if (is.null(int.probs)) {
      probs <- rep(1 / n_allowed, n_allowed)
      } else {
        probs <- int.probs
      }
      x_current     <- sample(allowed,   N, replace = TRUE, prob = probs)
      obj_current   <- objective(x_current)
      best_solution <- x_current
      best_obj      <- obj_current
      track_error   <- numeric(max_iter)
      temp          <- init_temp
      if (range[1] != range[2]) {
      for (start in seq_len(max_starts)) {
        if (progress_bar) {
          pb_interval <- max(floor(hill_climbs / 100), 1)
          pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
          on.exit(close(pb), add = TRUE)
        }
        for (i in seq_len(max_iter)) {
          candidate <- if (stats::runif(1) < prob_heuristic) {
            heuristic_move(x_current, target_sd, range)
          } else {
            idx <- sample.int(N, 1)
            tmp <- x_current
            sample.idx <- allowed %in% tmp[idx]
            tmp[idx] <- sample(allowed[!sample.idx], 1, prob = probs[!sample.idx])
            tmp
          }
          obj_cand <- objective(candidate)
          acc_prob <- exp((obj_current - obj_cand) / temp)
          if (obj_cand < obj_current || stats::runif(1) < acc_prob) {
            x_current   <- candidate
            obj_current <- obj_cand
            if (obj_cand < best_obj) {
              best_solution <- candidate
              best_obj      <- obj_cand
            }
          }
          temp        <- temp * cooling_rate
          track_error[i] <- best_obj
          if (progress_bar && (i %% pb_interval == 0)) {
            utils::setTxtProgressBar(pb, i)
          }
          if (best_obj < tolerance) {
            cat("\nconverged!\n")
            break
          }
        }
        if (progress_bar) {close(pb)}
        track_error <- track_error[seq_len(i)]
        x_current   <- best_solution
        if (i < max_iter) break
      }
      } else {
        best_solution <- x_current
        best_obj <- 0
        track_error   <- 0
      }
    } else {
      # Optimization continuous
      init_par   <- stats::runif(N, range[1], range[2])
      if (range[1] != range[2]) {
      cat("\nPSO is running...\n")
      pso_res <- pso::psoptim(
        par    = init_par,
        fn     = objective,
        lower  = rep(range[1], N),
        upper  = rep(range[2], N),
        control = list(
          maxit          = maxit_pso,
          maxit.stagnate = maxit_pso / 3,
          trace.stats    = TRUE,
          abstol         = tolerance
        )
      )
      cat(pso_res$message, "\n\n")
      best_solution <- pso_res$par
      best_obj      <- pso_res$value
      track_error   <- pso_res$stats$error
      } else {
        best_solution <- init_par
        best_obj      <- 0
        track_error   <- 0
      }
    }

    # combine results
    list(
      data        = best_solution,
      best_error  = best_obj,
      track_error = track_error,
      grim        = grim
    )
  }

  # storage
  solution_matrix <- matrix(NA, nrow = N, ncol = n_var)
  best_error_vec  <- vector("list",      n_var)
  track_error     <- vector("list",      n_var)
  grim_list       <- vector("list",      n_var)

  # Multiple Runs Parallel
  if (parallel) {
    old_plan <- future::plan()
    on.exit( future::plan(old_plan), add = TRUE )
    cat("There are ", future::availableCores() , "available workers. \n")
    real_cores <- future::availableCores()
    n_workers  <- min(n_var, max(real_cores-1,1))
    if (n_workers > 1L) {
      future::plan(future::multisession, workers = n_workers)
    } else {
      future::plan(future::sequential)
    }
    cat("Running with", n_workers, "worker(s). \n")
    pkgs <- c("discourse", "Rcpp")
    cat("\nParallel optimization is running...\n")
    start_time <- Sys.time()
    if(progress_mode == "shiny") {
      handler <- list(progressr::handler_shiny())
    } else {
      handler <-list(progressr::handler_txtprogressbar())
    }
    values <- progressr::with_progress({
      p <- progressr::progressor(steps = n_var)
        future.apply::future_lapply(
          X           = seq_len(n_var),
          FUN         = function(i) {
          res <- optim_vec_single(
            N,
            target_mean[i],
            target_sd[i],
            range[, i],
            tolerance,
            max_iter,
            init_temp,
            cooling_rate,
            int.probs[[i]],
            progress_bar = FALSE,
            obj_weight[[i]],
            integer[i],
            maxit_pso,
            eps,
            max_starts,
            checkGrim,
            prob_heuristic
          )
            p()
            res
          },
          future.seed = TRUE
        )},
    handlers = handler
    )

    cat(" finished.\n")
    stop_time <- Sys.time()
    cat("\nParallel optimization time was", stop_time - start_time, "seconds.\n")

    # combine results
    for (i in seq_len(n_var)) {
      sol                 <- values[[i]]$data
      solution_matrix[, i] <- sol
      best_error_vec[[i]] <- values[[i]]$best_error
      track_error[[i]]    <- values[[i]]$track_error
      grim_list[[i]]      <- values[[i]]$grim
    }
  } else {

    # Multiple Runs Sequential
      if(progress_mode == "shiny") {
        handler <- list(progressr::handler_shiny())
      } else {
        handler <-list(progressr::handler_txtprogressbar())
      }
    progressr::with_progress({
        p <- progressr::progressor(steps = n_var)
          for (i in seq_len(n_var)) {
            res <- optim_vec_single(
              N, target_mean[i], target_sd[i], range[, i], tolerance,
              max_iter, init_temp, cooling_rate, int.probs[[i]],
              progress_bar, obj_weight[[i]], integer[i], maxit_pso, eps,
              max_starts, checkGrim, prob_heuristic
            )
            solution_matrix[, i] <- res$data
            best_error_vec[[i]] <- res$best_error
            track_error[[i]]    <- res$track_error
            grim_list[[i]]      <- res$grim
            p()
          }
            },
    handlers = handler
    )
  }

  # assemble output
  colnames(solution_matrix) <- names(target_mean)
  solution_matrix <- as.data.frame(solution_matrix)
  result <- list(
    best_error  = best_error_vec,
    data        = solution_matrix,
    inputs      = list(
      target_mean  = target_mean,
      target_sd    = target_sd,
      N            = N,
      weight       = obj_weight,
      int.probs    = int.probs,
      max_iter     = max_iter,
      init_temp    = init_temp,
      cooling_rate = cooling_rate
    ),
    track_error = track_error,
    grim         = grim_list
  )
  class(result) <- "discourse.object"
  result
}
