#' Optimize simulated data to match target correlations and regression fits
#'
#' @param sim_data   Data frame with predictors and outcome in last column
#' @param target_cor Target vector of correlations (upper-triangular elements)
#' @param target_reg Target regression coefficients
#' @param reg_equation Regression formula as character, e.g. "Y ~ X1 + X2 + X1:X2"
#' @param target_se  Optional target standard errors for coefficients
#' @param weight     Two-element vector weighting cor vs reg error
#' @param max_iter   Max iterations for simulated annealing
#' @param init_temp  Initial temperature
#' @param cooling_rate Cooling rate per iteration (auto if NA)
#' @param tol        Error tolerance for convergence
#' @param prob_global_move Prob of global move vs local swap
#' @param progress_bar  Show progress bar if TRUE
#' @param max_starts     Number of annealing restarts
#' @param hill_climbs Optional hill-climbing iterations for refinement
#' @return A "discourse.object" with best error, data, inputs, and trace
#' @export
optim_lm <- function(
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
    progress_bar = TRUE,
    max_starts = 1,
    hill_climbs = NA
) {

  # determine rounding precision
  cor_dec <- max(count_decimals(target_cor))
  reg_dec <- max(count_decimals(target_reg))

  # derive term positions for C++ error functions
  terms_obj  <- stats::terms(stats::as.formula(reg_equation))
  design_cpp <- get_design(sim_data, reg_equation, terms_obj)$positions
  names(target_reg) <- get_design(sim_data, reg_equation, terms_obj)$target_names
  num_cols   <- ncol(sim_data)
  col_names  <- colnames(sim_data)

  # extract predictor matrix and outcome vector
  sim_mat    <- as.matrix(sim_data)
  predictors <- sim_mat[, 1:(num_cols - 1)]
  outcome    <- sim_mat[, num_cols]
  num_preds  <- ncol(predictors)
  N          <- nrow(predictors)

  # define error function based on rcpp and target_se
  if (is.null(target_se)) {
    error_function <- function(candidate) {
      error_function_cpp(
        candidate,
        outcome,
        target_cor,
        target_reg,
        weight,
        design_cpp,
        cor_dec,
        reg_dec
      )
    }
  } else {
    target_reg_se <- cbind(target_reg, target_se)
    error_function <- function(candidate) {
      error_function_cpp_se(
        candidate,
        outcome,
        target_cor,
        target_reg_se,
        weight,
        design_cpp,
        cor_dec,
        reg_dec
      )
    }
  }

  # initialize cooling schedule
  if (is.na(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }
  temp <- init_temp

  # outer restarts
  for (start in seq_len(max_starts)) {
    if (progress_bar) {
      pb_interval <- floor(max_iter / 100)
      pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
      on.exit(close(pb), add = TRUE)
    }

    track_error       <- numeric(max_iter)
    track_error_ratio <- numeric(max_iter)

    # initial candidate for first start
    if (start == 1) {
      current_candidate <- predictors
    }
    initial <- error_function(current_candidate)
    current_error <- initial$total_error
    best_candidate <- current_candidate
    best_error     <- current_error
    best_ratio     <- initial$error_ratio

    # simulated annealing loop
    for (iter in seq_len(max_iter)) {
      candidate <- current_candidate
      if (stats::runif(1) < prob_global_move) {
        perm      <- sample(N)
        candidate <- candidate[perm, ]
      } else {
        col_idx <- sample(num_preds, 1)
        idx     <- sample(N, 2)
        candidate[idx, col_idx] <- candidate[rev(idx), col_idx]
      }

      err <- error_function(candidate)
      if (err$total_error < current_error ||
          stats::runif(1) < exp((current_error - err$total_error) / temp)) {
        current_candidate <- candidate
        current_error     <- err$total_error
        if (current_error < best_error) {
          best_candidate <- current_candidate
          best_error     <- current_error
          best_ratio     <- err$error_ratio
        }
      }

      temp <- temp * cooling_rate
      track_error[iter]       <- best_error
      track_error_ratio[iter] <- best_ratio

      if (progress_bar && (iter %% pb_interval == 0)) {
        utils::setTxtProgressBar(pb, iter)
      }

      if (best_error < tol) {
        cat("\nconverged!\n")
        break
      }
    }
    cat("\nBest error in start", start, "is", best_error, "\n")
    current_candidate <- best_candidate
    temp <- init_temp / (2 ^ start)
  }

  # optional hill-climbing refinement
  if (!is.na(hill_climbs)) {
    local_opt <- hill_climb(
      current_candidate = current_candidate,
      outcome = outcome,
      error_function = error_function,
      max_iter = hill_climbs,
      mixedmodel = FALSE,
      num_preds = num_preds,
      progress_bar = progress_bar
    )
    best_error     <- local_opt$best_error
    best_candidate <- local_opt$best_candidate
  }

  # assemble final solution
  best_solution <- cbind(best_candidate, outcome)
  colnames(best_solution) <- col_names

  result <- list(
    best_error        = best_error,
    data              = best_solution,
    inputs            = as.list(environment()),
    track_error       = track_error,
    track_error_ratio = track_error_ratio
  )
  class(result) <- "discourse.object"
  result
}
