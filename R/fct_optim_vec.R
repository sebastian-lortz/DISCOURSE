#' Optimize a vector matrix to match target means and SDs
#'
#' @param N Integer, number of values.
#' @param target_mean Named numeric vector of desired means.
#' @param target_sd Named numeric vector of desired SDs.
#' @param range Numeric of length 2 or matrix; allowed value ranges.
#' @param tolerance Numeric; stop when objective ≤ tolerance.
#' @param integer Logical; use integer values if TRUE.
#' @param max_iter Integer; max annealing iterations (default 1e5).
#' @param init_temp Numeric; starting temperature.
#' @param cooling_rate Numeric; decay rate (default auto).
#' @param init_distr Character; "uniform" or "normal" init distribution.
#' @param skew Numeric; skew for normal init (integer mode).
#' @param kurt Numeric; kurtosis for normal init (integer mode).
#' @param pb_update_interval Integer; progress‐bar refresh frequency.
#' @param obj_weight List of two numerics; weights for mean vs SD.
#' @param prior_weight Numeric; mix weight for prior sampling.
#' @param maxit_pso Integer; max PSO iterations (continuous mode).
#' @param eps Numeric; small constant to avoid zero division.
#' @param max_starts Integer; annealing restarts.
#' @param checkGrim Logical; run GRIM check for integers.
#' @param prob_heuristic Numeric; heuristic move probability.
#' @param parallel Logical; run per-variable in parallel.
#'
#' @return A `discourse.object` with:
#' * `data`: matrix of optimized vectors.
#' * `best_error`: list of final errors.
#' * `track_error`: list of error paths.
#' * `inputs`: list of input parameters.
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
    cooling_rate    = NA,
    init_distr      = "uniform",
    skew            = 0,
    kurt            = 1,
    pb_update_interval = NA,
    obj_weight      = list(c(1, 1)),
    prior_weight    = 0.5,
    maxit_pso       = 1000,
    eps             = 1e-12,
    max_starts      = 3,
    checkGrim       = TRUE,
    prob_heuristic  = 0.1,
    parallel        = FALSE
) {
  ## Adjust default cooling rate
  if (is.na(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }

  ## Validate inputs
  if (length(target_mean) != length(target_sd)) {
    stop("`target_mean` and `target_sd` must be the same length.")
  }
  if (length(names(target_mean)) < length(target_mean)) {
    stop("Names must be supplied with `target_mean`.")
  }

  n_var <- length(target_mean)
  if (length(init_distr) < n_var) {
    init_distr <- rep(init_distr[1], n_var)
  }
  if (length(skew) < n_var) {
    skew <- rep(skew[1], n_var)
  }
  if (length(kurt) < n_var) {
    kurt <- rep(kurt[1], n_var)
  }
  if (length(prior_weight) < n_var) {
    prior_weight <- rep(prior_weight[1], n_var)
  }
  if (length(obj_weight) < n_var) {
    obj_weight <- rep(obj_weight[1], n_var)
  }
  if (length(integer) < n_var) {
    integer <- rep(integer[1], n_var)
  }

  ## Single‐run optimizer
  optim_vec_single <- function(
    N, target_mean, target_sd, range, tolerance,
    max_iter, init_temp, cooling_rate, init_distr,
    skew, kurt, pb_update_interval, obj_weight,
    prior_weight, integer, maxit_pso,
    eps, max_starts, checkGrim, prob_heuristic
  ) {
    mean_dec <- count_decimals(target_mean)
    sd_dec   <- count_decimals(target_sd)

    ## Optional GRIM check
    if (checkGrim && integer) {
      grim <- check_grim(N, target_mean, mean_dec)
      cat(grim$message)
    } else {
      grim <- NULL
    }

    ## Define objective
      objective <- function(x) {
        objective_cpp(x, target_mean, target_sd, obj_weight, eps, mean_dec, sd_dec)
      }

    if (integer) {
      ## Integer‐value optimization
      allowed   <- seq(range[1], range[2])
      n_allowed <- length(allowed)

      if (init_distr == "normal") {
        probs <- sn::psn(allowed + 0.5, xi = target_mean, omega = target_sd, alpha = skew) -
          sn::psn(allowed - 0.5, xi = target_mean, omega = target_sd, alpha = skew)
        probs <- probs / sum(probs)
        if (kurt != 1) {
          probs <- probs^kurt
          probs <- probs / sum(probs)
        }
        uni   <- rep(1 / n_allowed, n_allowed)
        probs <- prior_weight * probs + (1 - prior_weight) * uni
        probs <- probs / sum(probs)
      } else {
        probs <- rep(1 / n_allowed, n_allowed)
      }

      if (is.na(pb_update_interval)) {
        pb_update_interval <- floor(max_iter / 100)
      }
      pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)

      x_current     <- sample(allowed,   N, replace = TRUE, prob = probs)
      obj_current   <- objective(x_current)
      best_solution <- x_current
      best_obj      <- obj_current
      track_error   <- numeric(max_iter)
      temp          <- init_temp

      for (start in seq_len(max_starts)) {
        for (i in seq_len(max_iter)) {
          candidate <- if (stats::runif(1) < prob_heuristic) {
            heuristic_move(x_current, target_sd, range)
          } else {
            idx <- sample.int(N, 1)
            tmp <- x_current
            tmp[idx] <- sample(allowed, 1, prob = probs)
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
          if (i %% pb_update_interval == 0) {
            utils::setTxtProgressBar(pb, i)
          }
          if (best_obj < tolerance) {
            utils::setTxtProgressBar(pb, i)
            cat("\nconverged!\n")
            break
          }
        }
        close(pb)
        track_error <- track_error[seq_len(i)]
        x_current   <- best_solution
        if (i < max_iter) break
      }
    } else {
      ## Continuous PSO optimization
      init_par   <- stats::runif(N, range[1], range[2])
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
    }

    list(
      data        = best_solution,
      best_error  = best_obj,
      track_error = track_error,
      grim        = grim
    )
  }

  ## Prepare storage
  solution_matrix <- matrix(NA, nrow = N, ncol = n_var)
  best_error_vec  <- vector("list",      n_var)
  track_error     <- vector("list",      n_var)
  grim_list       <- vector("list",      n_var)

  if (parallel) {
    cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(cores)
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

    values <- foreach::foreach(
      i = seq_len(n_var),
      .packages = pkgs
    ) %dopar% {
      optim_vec_single(
        N, target_mean[i], target_sd[i], range[, i], tolerance,
        max_iter, init_temp, cooling_rate, init_distr[i],
        skew[i], kurt[i], pb_update_interval, obj_weight[[i]],
        prior_weight[i], integer[i], maxit_pso, eps,
        max_starts, checkGrim, prob_heuristic
      )
    }
    cat(" finished.\n")
    stop_time <- Sys.time()
    cat("\nParallel optimization time was", stop_time - start_time, "seconds.\n")

    for (i in seq_len(n_var)) {
      sol                 <- values[[i]]$data
      solution_matrix[, i] <- sol
      best_error_vec[[i]] <- values[[i]]$best_error
      track_error[[i]]    <- values[[i]]$track_error
      grim_list[[i]]      <- values[[i]]$grim
    }
  } else {
    for (i in seq_len(n_var)) {
      res <- optim_vec_single(
        N, target_mean[i], target_sd[i], range[, i], tolerance,
        max_iter, init_temp, cooling_rate, init_distr[i],
        skew[i], kurt[i], pb_update_interval, obj_weight[[i]],
        prior_weight[i], integer[i], maxit_pso, eps,
        max_starts, checkGrim, prob_heuristic
      )
      solution_matrix[, i] <- res$data
      best_error_vec[[i]] <- res$best_error
      track_error[[i]]    <- res$track_error
      grim_list[[i]]      <- res$grim
    }
  }

  colnames(solution_matrix) <- names(target_mean)

  result <- list(
    best_error  = best_error_vec,
    data        = solution_matrix,
    inputs      = list(
      target_mean  = target_mean,
      target_sd    = target_sd,
      N            = N,
      grim         = grim_list,
      weight       = obj_weight,
      max_iter     = max_iter,
      init_temp    = init_temp,
      cooling_rate = cooling_rate
    ),
    track_error = track_error
  )
  class(result) <- "discourse.object"
  result
}
