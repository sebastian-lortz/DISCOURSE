#' Perform hill-climbing optimization
#'
#' @param N The sample size
#' @param current_candidate Initial candidate data structure
#' @param error_function Function returning list with element $total_error
#' @param hill_climbs Maximum iterations (default 1e4)
#' @param LME Logical, use mixed-model moves if TRUE
#' @param num_preds Number of predictors for standard models
#' @param progress_bar Show progress bar if TRUE
#' @param w.length The length of the LME data in wide format
#' @param neighborhood_size Moves per iteration
#' @param outcome The outcome vector in the LM module
#' @return List with best_candidate and best_error
#' @export
hill_climb <- function(current_candidate, error_function, N,
                       hill_climbs = 1e2, LME = FALSE,
                       num_preds = NULL, progress_bar = TRUE,
                       neighborhood_size = 4, w.length = NULL,
                       outcome = NULL
                       ) {
  # input checks
  if (missing(current_candidate)) {
    stop("`current_candidate` must be provided, the data frame to be optimized.")
  }
  if (!is.function(error_function)) {
    stop("`error_function` must be a function returning a list with element `$total_error`.")
  }
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer indicating the number of observations.")
  }
  if (!(
    is.null(hill_climbs) ||
    (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
     hill_climbs >= 1 && hill_climbs == as.integer(hill_climbs))
  )) {
    stop("`hill_climbs` must be NULL or a single positive integer for the number of hill climb iterations.")
  }
  if (!is.logical(LME) || length(LME) != 1) {
    stop("`LME` must be a single logical value indicating whether the data to be optimized is based on a mixed-effects design.")
  }
  if (!LME && (!is.numeric(num_preds) || length(num_preds) != 1 || num_preds < 1)) {
    stop("`num_preds` must be a single positive integer when `LME = FALSE`.")
  }
  if (!is.logical(progress_bar) || length(progress_bar) != 1) {
    stop("`progress_bar` must be a single logical value (TRUE/FALSE).")
  }
  if (!is.numeric(neighborhood_size) || length(neighborhood_size) != 1 ||
      neighborhood_size < 1 || neighborhood_size != as.integer(neighborhood_size)) {
    stop("`neighborhood_size` must be a single positive integer specifying moves per iteration.")
  }
  if (LME && (!is.numeric(w.length) || length(w.length) != 1 || w.length < 1)) {
    stop("`w.length` must be a single positive integer indicating the number of wide format columns when `LME = TRUE`.")
  }

  # initial error and best
  curr_err <- error_function(current_candidate)$total_error
  best_cand <- current_candidate; best_err <- curr_err

  # progress bar setup
  if (progress_bar) {
    pb_int <- max(floor(hill_climbs / 100), 1)
    pb <- utils::txtProgressBar(0, hill_climbs, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  # main loop
  for (i in seq_len(hill_climbs)) {
    loc_cand <- current_candidate; loc_err <- curr_err
    # generate neighborhood
    for (j in seq_len(neighborhood_size)) {
      if (LME) {
        w.candidate <- long_to_wide(current_candidate)
        # local swap
        col <- sample(1:w.length,1)
        idx <- sample(N,2)
        w.candidate[idx,col] <- w.candidate[rev(idx),col]
        cand <- wide_to_long(w.candidate)
      } else {
        cand <- current_candidate
          col <- sample(seq_len(num_preds), 1)
          idx <- sample(nrow(cand), 2)
          cand[idx, col] <- cand[rev(idx), col]
      }
      # evaluate
      cand_err <- error_function(cand)$total_error
      if (cand_err < loc_err) {
        loc_err <- cand_err; loc_cand <- cand
      }
    }
    # accept if improved
    if (loc_err < curr_err) {
      current_candidate <- loc_cand; curr_err <- loc_err
      best_cand <- loc_cand; best_err <- loc_err
    }
    # update bar
    if (progress_bar && (i %% pb_int == 0)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  cat("\nHill climbing best error:", best_err, "\n")
  list(best_candidate = best_cand, best_error = best_err)
}
