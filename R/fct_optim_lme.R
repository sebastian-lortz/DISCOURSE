#' Optimize simulated longitudinal mixed-effects data to match target correlations and regression estimates
#'
#' Uses the DISCOURSE algorithmic framework to simulate
#' data such that the resulting correlations and regression coefficients including random-intercept SD
#' match specified targets under a given mixed-effects regression model and input parameters.
#'
#' @param sim_data Data.frame. Wide-format predictors and outcome columns for longitudinal data.
#' @param target_cor Numeric vector. Target upper-triangular (excluding diagonal) correlation values for predictor and outcome variables.
#' @param target_reg Numeric vector. Target fixed-effect coefficients (including intercept and random-intercept SD).
#' @param reg_equation Character or formula. Mixed-effects model formula (e.g., "Y ~ X1 + X2 + (1|ID)").
#' @param target_se Numeric vector, optional. Target standard errors for fixed-effect estimates; length matches `target_reg` minus 1 (random-intercept SD).
#' @param weight Numeric vector of length 2. Weights for correlation vs. regression error in the objective function. Default `c(1, 1)`.
#' @param max_iter Integer. Maximum iterations for simulated annealing per start. Default `1e5`.
#' @param init_temp Numeric. Initial temperature for annealing. Default `1`.
#' @param cooling_rate Numeric or NULL. Cooling rate per iteration (0–1); if NULL, computed as `(max_iter - 10) / max_iter`.
#' @param tolerance Numeric. Error tolerance for convergence; stops early if best error < `tolerance`. Default `1e-6`.
#' @param progress_bar Logical. Show text progress bar during optimization. Default `TRUE`.
#' @param max_starts Integer. Number of annealing restarts. Default `1`.
#' @param move_prob List. Start/end move probabilities for operations: residual swap, k-cycle, local swap, tau reordering.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param eps Numeric. Small constant to stabilize scaling (prevent division by zero). Default `1e-5`.
#' @param hill_climbs Integer or NULL. Number of hill‐climbing iterations for optional local refinement; if NULL, skips refinement. Default `NULL`.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#'  @return A `discourse.object` list containing:
#' \describe{
#'   \item{best_error}{Numeric. Minimum objective error achieved.}
#'   \item{data}{Data.frame. Optimized wide-format longitudinal data.}
#'   \item{inputs}{List of all input parameters for reproducibility.}
#'   \item{track_error}{Numeric vector of best error at each iteration of annealing.}
#'   \item{track_error_ratio}{Numeric vector of error ratios (cor vs. reg) per iteration.}
#'   \item{track_move_best}{Character vector. Move types that produced best improvements.}
#'   \item{track_move_acc}{Character vector. Move types accepted per iteration.}
#' }
#'
#' @examples
#'  \dontrun{
#' # Optimize data
#' optim_lme(
#'   sim_data       = sim_data,
#'   target_cor     = c(0.4),
#'   target_reg     = c(1, 0.5, 4),
#'   reg_equation   = "Y ~ X1 + (1|ID)",
#'   max_iter       = 2000,
#'   hill_climbs    = 50
#' )
#' }
#' @export
optim_lme <- function(sim_data,
                      target_cor,
                      target_reg,
                      reg_equation,
                      target_se       = NULL,
                      weight          = c(1,1),
                      max_iter        = 1e5,
                      init_temp       = 1,
                      cooling_rate    = NULL,
                      tolerance       = 1e-6,
                      progress_bar    = TRUE,
                      max_starts      = 1,
                      hill_climbs     = 100,
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
                      min_decimals = 1,
                      eps = 1e-5,
                      progress_mode = "console"
) {

  # input checks
  if (!is.data.frame(sim_data) || nrow(sim_data) < 1) {
    stop("`sim_data` must be a non-empty data frame.")
  }
  if (!is.character(reg_equation) || length(reg_equation) != 1) {
    stop("`reg_equation` must be a single character string representing the model formula.")
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

  # get data info
  reg_equation <- stats::as.formula(reg_equation)
  N <- nrow(sim_data)
  ID <- 1:nrow(sim_data)
  NA_cor <- is.na(target_cor)
  NA_reg <- is.na(target_reg)
  w.length <- ncol(sim_data)
  reg.names <- names(target_reg)

  # get decimals
  cor_dec <- max(count_decimals(target_cor, min_decimals = min_decimals))
  reg_dec <- max(count_decimals(target_reg, min_decimals = min_decimals))
  target_reg[length(target_reg)] <- round((target_reg[length(target_reg)])^2, reg_dec)
  if (!is.null(target_se)) {
    NA_se <- is.na(target_se)
    se_dec <- max(count_decimals(target_se, min_decimals = min_decimals))
  }

  # data to long format
  full_data <- cbind(ID, sim_data)
  long_data <- wide_to_long(full_data)
  long_ID <- long_data$ID
  long_num_cols <- ncol(long_data)

  # get between and within predictors
  if ((which(colnames(long_data)=="time") - 2) > 0) {
    between_cols <- 2:(which(colnames(long_data)=="time")-1)
  } else {
    between_cols <- NA
  }
  time_cols <- (which(colnames(long_data)=="time")+1):(long_num_cols-1)
  long_candidate_cols <-
    if (any(!is.na(between_cols))) c(between_cols, time_cols) else time_cols
  pred_names    <- setdiff(names(long_data[,-long_num_cols]), c("ID","time"))
  times         <- long_data$time
  unique_times <- unique(times)
  col_names     <- colnames(long_data)
  y.name <- col_names[long_num_cols]

  # get matrix
  predictors <- long_data[, long_candidate_cols]
  outcome    <- long_data[, long_num_cols]

  # intput check cor and reg
  n.col <- length(long_candidate_cols)+1
  exp_cor <- (n.col*(n.col-1))/2
  term_lbls <- attr(stats::terms(stats::as.formula(reg_equation)), "term.labels")
  exp_reg  <- length(term_lbls) + 1
  names(target_reg) <- c("(Intercept)", term_lbls[-length(term_lbls)], "ID")
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
  if (!is.null(target_se) && (!is.numeric(target_se) || length(target_se) != exp_reg-1)) {
    stop("`target_se`, if provided, must be a numeric vector matching the length of `target_reg` minus 1 (SD of random intercept).")
  }

  # tau reorder
  tau_order <- function(wide_df) {
    y.cols      <- paste0(y.name, "_", unique_times)
    col_j       <- sample(y.cols, 1)
    other_cols  <- setdiff(y.cols, col_j)
    subj_means  <- rowMeans(wide_df[, other_cols, drop = FALSE])
    vals        <- wide_df[,col_j]
    new_vals    <- sort(vals)[order(subj_means)]
    wide_df[,col_j] <- new_vals
    wide_df
  }

  # k_cycle
  max.k <- if (N/4 <= 3) {3} else {N/4}
  k_permute <- function(wide_df) {
    col_j <- sample(w.length, 1)
    k     <- sample(3:max.k, 1)
    subs  <- sample(N, k)
    wide_df[subs, col_j] <- sample(wide_df[subs, col_j])
    wide_df
  }

  # residual swap
  residual_swap <- function(wide_df) {
    col_j <- sample(w.length, 1)
    y.cols     <- paste0(y.name, "_", unique_times)
    subj_means <- rowMeans(wide_df[ , y.cols, drop=FALSE])
    g          <- subj_means - mean(subj_means)
    high_idx   <- which(g > 0)
    low_idx    <- which(g < 0)
    if (length(high_idx)==0 || length(low_idx)==0) {
      return(wide_df)
    } else {
      i <- sample(high_idx, 1, prob = g[high_idx])
      j <- sample(low_idx,  1, prob = -g[low_idx])
      pair <- c(i, j)
    }
    tmp <- wide_df[pair[1], col_j]
    wide_df[pair[1], col_j] <- wide_df[pair[2], col_j]
    wide_df[pair[2], col_j] <- tmp
    wide_df
  }

  # objective function
  candidate_cor <- function(candidate) {
      candidate_cor_cpp(
        as.matrix(candidate[, (names(candidate) %in% pred_names)]),
        candidate[,long_num_cols]
      )
    }
    if (is.null(target_se)) {
    candidate_reg <- function(candidate) {
      long_candidate <- candidate
      colnames(long_candidate) <- col_names
      tryCatch({
        model <- lme4::lmer(
          reg_equation, data = long_candidate,
          control = lme4::lmerControl(
            optimizer   = "bobyqa",
            optCtrl     = list(maxfun = 2e5),
            check.conv.singular = "ignore",
            calc.derivs         = FALSE
          )
        )
        vc<- as.data.frame(lme4::VarCorr(model))
        tau <- vc[ vc$grp=="ID" & vc$var1=="(Intercept)", "vcov" ]
        if (is.null(tau)) {
          tau <- var_tau(long_candidate, y.name)
        }
        c(lme4::fixef(model),
          tau
          )
      }, error = function(e) {
        rep(1e5, length(target_reg))
      })
    }
  error_function <- function(candidate) {
    cor_vec    <- candidate_cor(candidate)
    if (sum(!NA_cor) > 0) {
      cor_error <- sqrt(
        mean((round(cor_vec[!NA_cor], cor_dec) - target_cor[!NA_cor])^2)
      )
    } else {
      cor_error <- 0
    }
    reg_vec   <- candidate_reg(candidate)
    coef_err  <- round(reg_vec[!NA_reg], reg_dec) - target_reg[!NA_reg]
    scales   <- pmax(abs(target_reg[!NA_reg]), eps)
    coef_err <- coef_err/scales
    reg_error <- sqrt(mean(c(coef_err)^2))
    total_error<- cor_error*weight[1] + reg_error*weight[2]
    error_ratio<- cor_error/coef_err
    list(total_error=total_error, error_ratio=error_ratio)
  }
} else { # incl SE
      candidate_reg <- function(candidate) {
        long_candidate <- candidate
        colnames(long_candidate) <- col_names
        tryCatch({
        model <- lme4::lmer(
          reg_equation, data = long_candidate,
          control = lme4::lmerControl(
            optimizer   = "bobyqa",
            optCtrl     = list(maxfun = 2e5),
            check.conv.singular = "ignore",
            calc.derivs         = FALSE
          )
        )
        vc<- as.data.frame(lme4::VarCorr(model))
        tau <- vc[ vc$grp=="ID" & vc$var1=="(Intercept)", "vcov" ]
if (is.null(tau)) {
    tau <- var_tau(long_candidate, y.name)
}
        c(lme4::fixef(model),
          tau,
          as.vector(stats::coef(summary(model))[ , "Std. Error"]))
        } , error = function(e) {
          rep(1e5, length(c(target_reg, target_se)))
        })
      }
      error_function <- function(candidate) {
        cor_vec   <- candidate_cor(candidate)
        if (sum(!NA_cor) > 0) {
          cor_error <- sqrt(
            mean((round(cor_vec[!NA_cor], cor_dec) - target_cor[!NA_cor])^2)
          )
        } else {
          cor_error <- 0
        }
        reg_vec   <- candidate_reg(candidate)
        k         <- length(target_reg)
        j         <- length(target_se)
        coef_err  <- round(reg_vec[1:k][!NA_reg], reg_dec) - target_reg[!NA_reg]
        scales   <- pmax(abs(target_reg[!NA_reg]), eps)
        coef_err <- coef_err/scales
        se_err    <- round(reg_vec[(k+1):(k+j)][!NA_se], se_dec) - target_se[!NA_se]
        scales   <- pmax(abs(target_se[!NA_se]), eps)
        se_err <- se_err/ scales
        reg_error <- sqrt(mean(c(coef_err, se_err)^2))
        total_error <- cor_error * weight[1] + reg_error * weight[2]
        error_ratio <- cor_error / reg_error
        list(total_error = total_error, error_ratio = error_ratio)
      }
}

  # init parameters
  if (is.null(cooling_rate)) cooling_rate <- (max_iter-10)/max_iter
  temp <- init_temp
  if (!all(c("start","end") %in% names(move_prob))) {
    stop("`move_prob` must be a list with elements `$start` and `$end`")
  }
  p_start <- move_prob$start
  p_end   <- move_prob$end
  if (!all(names(p_start) == names(p_end))) {
    stop("`start` and `end` must have the same names")
  }
  get_move_probs <- function(i) {
    frac <- i / max_iter
    p_i  <- p_start + frac * (p_end - p_start)
    p_i / sum(p_i)
  }

  # set progressr
  if(progress_mode == "shiny") {
    handler <- list(progressr::handler_shiny())
  } else {
    handler <-list(progressr::handler_txtprogressbar())
  }
  pb_interval_sa <- floor(max_iter / 100)
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
          pb <- utils::txtProgressBar(min=0, max=max_iter, style=3)
          on.exit(base::close(pb), add=TRUE)
        }
        track_error       <- numeric(max_iter)
        track_error_ratio <- numeric(max_iter)
        track.move.best <- rep(NA,max_iter)
        track.move.acc <- rep(NA,max_iter)
        if (start==1) {
          current_candidate <- data.frame(
            ID = long_data$ID,
            time    = long_data$time,
            long_data[, pred_names],
            V4 = outcome
          )
          time_indices      <- split(seq_len(nrow(current_candidate)),current_candidate$time)
          within_names      <- pred_names
          p_indices         <- split(seq_len(nrow(long_data)),long_data$ID)
          if (any(!is.na(between_cols))) {
            between_names    <- names(long_data)[between_cols]
            within_names     <- setdiff(pred_names, between_names)
            current_candidate<- data.frame(
              long_ID,
              long_data[,between_names],
              time=times,
              long_data[,within_names],
              V4 = outcome
            )
            colnames(current_candidate) <- col_names
          }
        }
        current_error <- error_function(current_candidate)$total_error
        best_candidate<- current_candidate
        best_error    <- current_error
        best_error_ratio <- error_function(current_candidate)$error_ratio

        for (i in seq_len(max_iter)) {
          w.candidate <- long_to_wide(current_candidate)
          probs <- get_move_probs(i)
          move <- sample(names(probs), size = 1, prob = probs)
          move.name <- move
          if (move == "tau") {
            w.candidate <- tau_order(w.candidate)
          } else if (move == "local") {
            col <- sample(1:w.length,1)
            idx <- sample(N,2)
            w.candidate[idx,col] <- w.candidate[rev(idx),col]
          } else if (move == "k_cycle") {
          w.candidate <- k_permute(w.candidate)
          } else if (move == "residual") {
            w.candidate <- residual_swap(w.candidate)
          }
          candidate <- wide_to_long(w.candidate)
          best <- NA
          acc <- NA
          err_list      <- error_function(candidate)
          candidate_err <- err_list$total_error
          if (candidate_err < current_error ||
              stats::runif(1) < exp((current_error-candidate_err)/temp)) {
            current_candidate <- candidate
            current_error     <- candidate_err
            acc <- move.name
            if (current_error < best_error) {
              best_candidate <- current_candidate
              best_error     <- current_error
              best_error_ratio<- err_list$error_ratio
              best <- move.name
            }
          }
          temp             <- temp * cooling_rate
          track_error[i]   <- best_error
          track_error_ratio[i] <- best_error_ratio
          track.move.best[i] <- best
          track.move.acc[i] <- acc
          if (progress_bar && (i%%pb_interval_sa==0)) {
            utils::setTxtProgressBar(pb, i)
            p()
          }
          if (best_error < tolerance) {
            cat("\nconverged!\n")
            break
          }
        }
        cat("\nBest error in start", start, "is", best_error, "\n")
        current_candidate <- best_candidate
        temp              <- init_temp
      }
      if (progress_bar) {close(pb)}
      # hill climbing
      if (!is.null(hill_climbs)) {
        local_opt <- hill_climb(
          current_candidate = current_candidate,
          error_function    = error_function,
          N = N,
          hill_climbs       = hill_climbs,
          LME               = TRUE,
          w.length          = w.length,
          progress_bar      = progress_bar,
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
  target_reg[length(target_reg)] <- round((sqrt(target_reg[length(target_reg)])), reg_dec)
  best_solution <- best_candidate
  colnames(best_solution) <- col_names

  # assemble output
  result <- list(
    best_error       = best_error,
    data             = best_solution,
    inputs           = list(
      target_cor   = target_cor,
      target_reg   = target_reg,
      target_se    = target_se,
      reg_equation = reg_equation,
      weight       = weight,
      max_iter     = max_iter,
      init_temp    = init_temp,
      cooling_rate = cooling_rate,
      move_prob    = move_prob
    ),
    track_error       = track_error,
    track_error_ratio = track_error_ratio,
    track.move.best = track.move.best,
    track.move.acc = track.move.acc
  )
  class(result) <- "discourse.object"
  result
}
