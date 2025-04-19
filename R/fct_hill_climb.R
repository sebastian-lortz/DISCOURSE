#' Perform hill-climbing optimization
#'
#' @param current_candidate Initial candidate data structure
#' @param outcome (unused) placeholder for future extension
#' @param error_function Function returning list with element $total_error
#' @param max_iter Maximum iterations (default 1e4)
#' @param mixedmodel Logical, use mixed-model moves if TRUE
#' @param candidate_cols Predictor columns for mixed models
#' @param unique_times Time groups for mixed models
#' @param num_preds Number of predictors for standard models
#' @param progress_bar Show progress bar if TRUE
#' @param neighborhood_size Moves per iteration
#' @param prob_within_move Probability of within-group move
#' @param within_names Names for within-group moves
#' @param time_indices Indices of time
#' @param p_indices Indices of participants
#' @param between_names Names for between-subject moves
#' @param pnumber Identifier vector for mixed-model subjects
#' @return List with best_candidate and best_error
#' @export
hill_climb <- function(current_candidate, outcome = NULL, error_function,
                       max_iter = 1e4, mixedmodel = FALSE,
                       candidate_cols = NULL, unique_times = NULL,
                       num_preds = NULL, progress_bar = FALSE,
                       neighborhood_size = 10, prob_within_move = .8,
                       within_names = NULL, time_indices = NULL,
                       p_indices = NULL, between_names = NULL,
                       pnumber = NULL) {
  # initial error and best
  curr_err <- error_function(current_candidate)$total_error
  best_cand <- current_candidate; best_err <- curr_err

  # progress bar setup
  if (progress_bar) {
    pb_int <- floor(max_iter / 100)
    pb <- utils::txtProgressBar(0, max_iter, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  # main loop
  for (i in seq_len(max_iter)) {
    loc_cand <- current_candidate; loc_err <- curr_err
    # generate neighborhood
    for (j in seq_len(neighborhood_size)) {
      cand <- current_candidate
      if (mixedmodel) {
        if (stats::runif(1) < prob_within_move) {
          t <- sample(unique_times, 1)
          col <- sample(within_names, 1)
          idx <- time_indices[[t]]
          pair <- sample(idx, 2)
          cand[pair, col] <- cand[rev(pair), col]
        } else {
          col <- sample(between_names, 1)
          pair <- sample(pnumber, 2)
          idx1 <- p_indices[[as.character(pair[1])]]
          idx2 <- p_indices[[as.character(pair[2])]]
          tmp <- cand[idx1, col]
          cand[idx1, col] <- cand[idx2, col]
          cand[idx2, col] <- tmp
        }
      } else {
        if (stats::runif(1) < .1) {
          cols <- sample(seq_len(num_preds), sample(seq_len(num_preds), 1))
          idx <- sample(nrow(cand), 2)
          cand[idx, cols] <- cand[rev(idx), cols]
        } else {
          col <- sample(seq_len(num_preds), 1)
          idx <- sample(nrow(cand), 2)
          cand[idx, col] <- cand[rev(idx), col]
        }
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
