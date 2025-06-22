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
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the targets
#' @return A "discourse.object" with best error, data, inputs, and trace
#' @export
optim_lm <- function(
    sim_data,
    target_cor,
    target_reg,
    reg_equation,
    target_se = NULL,
    weight = c(1,1),
    max_iter = 1e5,
    init_temp = 1,
    cooling_rate = NULL,
    tol = 1e-6,
    prob_global_move = 0.1,
    progress_bar = TRUE,
    max_starts = 1,
    hill_climbs = 100,
    min_decimals = 2
) {
# input checks
  if (!is.data.frame(sim_data) || ncol(sim_data) < 2) {
    stop("`sim_data` must be a data frame with at least two columns (predictors and outcome).")
  }
  if (!is.character(reg_equation) || length(reg_equation) != 1) {
    stop("`reg_equation` must be a single character string giving the regression formula.")
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
  if (!is.logical(progress_bar) || length(progress_bar) != 1) {
    stop("`progress_bar` must be a single logical value.")
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
  # determine rounding precision
  cor_dec <- max(count_decimals(target_cor, min_decimals = min_decimals))
  reg_dec <- max(count_decimals(target_reg, min_decimals = min_decimals))

  # Pull DV and predictors in order of reg_equation
  frm        <- stats::as.formula(reg_equation)
  all_vars   <- all.vars(frm)
  dv_name    <- all_vars[1]
  pred_names <- all_vars[-1]
  col_names <- c(pred_names,dv_name)

  # Build matrices
  predictors <- as.matrix(sim_data[, pred_names, drop = FALSE])
  outcome    <- sim_data[[dv_name]]
  num_preds  <- ncol(predictors)
  N          <- nrow(predictors)

  # derive term positions for rcpp
  terms_obj  <- stats::terms(stats::as.formula(reg_equation))
  design_cpp <- get_design(candidate = predictors, reg_equation, terms_obj)$positions
  names(target_reg) <- get_design(candidate = predictors, reg_equation, terms_obj)$target_names

  # map the target cor
  target_cor <- remap_target_cor(target_cor, sim_data, col_names)

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
  if (is.null(cooling_rate)) {
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
  if (progress_bar) {close(pb)}

  # optional hill-climbing refinement
  if (!is.null(hill_climbs)) {
    local_opt <- hill_climb(
      current_candidate = current_candidate,
      outcome = outcome,
      N = N,
      error_function = error_function,
      hill_climbs = hill_climbs,
      LME = FALSE,
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
    data              = as.data.frame(best_solution),
    inputs            = as.list(environment()),
    track_error       = track_error,
    track_error_ratio = track_error_ratio
  )
  class(result) <- "discourse.object"
  result
}
