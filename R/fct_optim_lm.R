#' Optimize simulated data to match target correlations and fixed-effects regression estimates
#'
#' Uses the DISCOURSE algorithmic framework to simulate
#' data such that the resulting correlations and regression coefficients
#' match specified targets under a given regression model and input parameters.
#'
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
#' @param hill_climbs Integer or NULL. Number of hill‐climbing iterations for optional local refinement; if NULL, skips refinement. Default `NULL`.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#' @return A `discourse.object` list containing:
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
#' res <- optim_lm(
#'   sim_data = sim_data,
#'   target_cor = c(.23),
#'   target_reg = c(2.1, 1.2, -0.8),
#'   reg_equation = "Y ~ X1 + X2",
#'   max_iter = 10000,
#'   hill_climbs = 50
#' )
#' }
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
    tolerance = 1e-6,
    prob_global_move = 0.1,
    progress_bar = TRUE,
    max_starts = 1,
    hill_climbs = NULL,
    min_decimals = 1,
    progress_mode = "console"
) {
# input checks
  if (!is.data.frame(sim_data) || ncol(sim_data) < 2) {
    stop("`sim_data` must be a data frame with at least two columns (predictors and outcome).")
  }
  if (!is.character(reg_equation) || length(reg_equation) != 1) {
    stop("`reg_equation` must be a single character string giving the regression formula.")
  }
  pred       <- ncol(sim_data)
  exp_cor <- pred*(pred-1)/2
  term_lbls<- base::attr(stats::terms(stats::as.formula(reg_equation)), "term.labels")
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
    stop("`hill_climbs` must be NULL or a single positive integer.")
  }
  if (!is.numeric(min_decimals) || length(min_decimals) != 1 ||
      min_decimals < 0 || min_decimals != as.integer(min_decimals)) {
    stop("`min_decimals` must be a single non-negative integer.")
  }
  if (!is.character(progress_mode) ||
      length(progress_mode) != 1 ||
      !progress_mode %in% c("console", "shiny")) {
    stop("`progress_mode` must be a single string, either \"console\" or \"shiny\".")
  }
  # get decimals
  cor_dec <- max(count_decimals(target_cor, min_decimals = min_decimals))
  reg_dec <- max(count_decimals(target_reg, min_decimals = min_decimals))

  # get variable info
  frm        <- stats::as.formula(reg_equation)
  all_vars   <- all.vars(frm)
  dv_name    <- all_vars[1]
  pred_names <- all_vars[-1]
  missing_vars <- setdiff(all_vars, colnames(sim_data))
  if (length(missing_vars) > 0) {
    stop(
      "The following variables in `reg_equation` are not present in `sim_data`: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Build
  col_names <- c(pred_names,dv_name)
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

  # objective function
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

  # init parameters
  if (is.null(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }
  temp <- init_temp

  # set progressr
  if(progress_mode == "shiny") {
    handler <- list(progressr::handler_shiny())
  } else {
    handler <-list(progressr::handler_txtprogressbar())
  }
  pb_interval_sa <- max(floor(max_iter / 100), 1)
  pb_interval_hc <- if (!is.null(hill_climbs)) max(floor(hill_climbs / 100), 1) else 1
  n_sa_calls <- sum(vapply(seq_len(max_starts), function(i) {
   length(seq_len(max_iter)[seq_len(max_iter) %% pb_interval_sa == 0])
  }, integer(1)))
  n_hc_calls <- if (!is.null(hill_climbs)) {
    length(seq_len(hill_climbs)[seq_len(hill_climbs) %% pb_interval_hc == 0])
  } else 0
  total_calls <- n_sa_calls + n_hc_calls

 # Optimization process
 progressr::with_progress({
    p <- progressr::progressor(steps = total_calls)
      for (start in seq_len(max_starts)) {
        if (progress_bar) {
          pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
          on.exit(close(pb), add = TRUE)
        }
        track_error       <- numeric(max_iter)
        track_error_ratio <- numeric(max_iter)
        if (start == 1) {
          current_candidate <- predictors
        }
        initial <- error_function(current_candidate)
        current_error <- initial$total_error
        best_candidate <- current_candidate
        best_error     <- current_error
        best_ratio     <- initial$error_ratio
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
          if (progress_bar && (iter %% pb_interval_sa == 0)) {
            utils::setTxtProgressBar(pb, iter)
            p()
          }
          if (best_error < tolerance) {
            cat("\nconverged!\n")
            break
          }
        }
        cat("\nBest error in start", start, "is", best_error, "\n")
        current_candidate <- best_candidate
        temp <- init_temp / (2 ^ start)
      }
      if (progress_bar) {close(pb)}
      # hill climbing optimization
      if (!is.null(hill_climbs) && hill_climbs>0) {
        local_opt <- hill_climb(
          current_candidate = current_candidate,
          outcome = outcome,
          N = N,
          error_function = error_function,
          hill_climbs = hill_climbs,
          LME = FALSE,
          num_preds = num_preds,
          progress_bar = progress_bar,
          progressor = p,
          pb_interval       = pb_interval_hc
        )
        best_error     <- local_opt$best_error
        best_candidate <- local_opt$best_candidate
      }
     },
 handlers = handler
 )

  # combine results
  best_solution <- cbind(best_candidate, outcome)
  colnames(best_solution) <- col_names

  # assemble output
  result <- list(
    best_error        = best_error,
    data              = as.data.frame(best_solution),
    inputs            = list(
      sim_data = sim_data,
      target_cor = target_cor,
      target_reg = target_reg,
      reg_equation = reg_equation,
      target_se = target_se,
      weight = weight,
      max_iter = max_iter,
      init_temp = init_temp,
      cooling_rate = cooling_rate,
      tolerance = tolerance,
      prob_global_move = prob_global_move,
      max_starts = max_starts,
      hill_climbs = hill_climbs,
      min_decimals = min_decimals,
      progress_bar = progress_bar,
      progress_mode = progress_mode
    ),
    track_error       = track_error,
    track_error_ratio = track_error_ratio
  )
  class(result) <- "discourse.object"
  result
}
