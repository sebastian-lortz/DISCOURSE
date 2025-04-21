#' optim_lme
#'
#' @description Optimize a longitudinal mixed‐model data set via simulated annealing.
#'
#' @param sim_data Data frame (wide) of predictors and outcome.
#' @param target_cor Numeric vector of target correlations (upper‐tri of full corr matrix).
#' @param target_reg Numeric vector of target regression coefficients (excluding intercept).
#' @param reg_equation Model formula string, e.g. "Y ~ X1 + X2 + X1:X2".
#' @param target_se Optional target standard errors for regression.
#' @param weight Numeric length‐2 vector of weights for (corr, reg) error.
#' @param max_iter Maximum SA iterations per start.
#' @param init_temp Initial temperature.
#' @param cooling_rate Cooling rate per iteration (if NA, set to (max_iter-10)/max_iter).
#' @param tol Error tolerance for early stopping.
#' @param prob_global_move Probability of a global shuffle move.
#' @param progress_bar Logical; show progress bar.
#' @param starts Number of SA restarts.
#' @param hill_climbs Optional hill‐climb iterations after SA.
#' @param prob_within_move Probability of within‐timegroup swap move.
#'
#' @return A "discourse.object" with best error, data, inputs, and trace
#' @export
optim_lme <- function(sim_data,
                      target_cor,
                      target_reg,
                      reg_equation,
                      target_se       = NULL,
                      weight          = c(1,1),
                      max_iter        = 1e5,
                      init_temp       = 1,
                      cooling_rate    = NA,
                      tol             = 1e-6,
                      prob_global_move= 0.1,
                      progress_bar    = TRUE,
                      starts          = 1,
                      hill_climbs     = NA,
                      prob_within_move= 0.8) {
  pnumber <- 1:nrow(sim_data)
  NA_cor <- is.na(target_cor)
  NA_reg <- is.na(target_reg)
  names(target_reg)[length(target_reg)] <- "pnumber"

  ## decimals for rounding
  cor_dec <- max(count_decimals(target_cor))
  reg_dec <- max(count_decimals(target_reg))
  if (!is.null(target_se)) {
    NA_se <- is.na(target_se)
    se_dec <- max(count_decimals(target_se))
  }

  ## to long
  full_data <- cbind(pnumber, sim_data)
  long_data <- wide_to_long(full_data)
  long_pnumber <- long_data$pnumber
  long_num_cols<- ncol(long_data)

  ## identify between‐ and within‐predictors
  if ((which(colnames(long_data)=="time") - 2) > 0) {
    between_cols <- 2:(which(colnames(long_data)=="time")-1)
  } else {
    between_cols <- NA
  }
  time_cols <- (which(colnames(long_data)=="time")+1):(long_num_cols-1)
  long_candidate_cols <-
    if (!is.na(between_cols)) c(between_cols, time_cols) else time_cols

  pred_names    <- setdiff(names(long_data[,-long_num_cols]), c("pnumber","time"))
  unique_times  <- unique(long_data$time)
  times         <- long_data$time
  col_names     <- colnames(long_data)

  ## extract matrices
  predictors <- long_data[, long_candidate_cols]
  outcome    <- long_data[, long_num_cols]

  ### helper wrappers
    candidate_cor <- function(candidate) {
      candidate_cor_cpp(as.matrix(candidate[,-(1:2)]), outcome)
    }

    if (is.null(target_se)) {
    candidate_reg <- function(candidate) {
      long_candidate <- cbind(candidate, outcome)
      colnames(long_candidate) <- col_names
      tryCatch({
        model <- lme4::lmer(
          reg_equation,
          data    = long_candidate,
          control = lme4::lmerControl(
            check.conv.singular = "ignore",
            calc.derivs         = FALSE
          )
        )
        c(lme4::fixef(model),
          as.data.frame(lme4::VarCorr(model))$sdcor[2]
        )
      }, error = function(e) {
        rep(1e5, length(target_reg))
      })
    }
  # error function
  error_function <- function(candidate) {
    cor_vec    <- candidate_cor(candidate)
    cor_error  <- sqrt(mean((round(cor_vec[!NA_cor], cor_dec) -
                               target_cor[!NA_cor])^2))
    reg_vec    <- candidate_reg(candidate)
    reg_error  <- sqrt(mean((round(reg_vec[!NA_reg], reg_dec) -
                               target_reg[!NA_reg])^2))
    total_error<- cor_error*weight[1] + reg_error*weight[2]
    error_ratio<- cor_error/reg_error
    list(total_error=total_error, error_ratio=error_ratio)
  }
    } else {
      # including target SEs
      candidate_reg <- function(candidate) {
        long_candidate <- cbind(candidate, outcome)
        colnames(long_candidate) <- col_names
        tryCatch({
        model <- lme4::lmer(
          reg_equation, data = long_candidate,
          control = lme4::lmerControl(
            check.conv.singular = "ignore",
            calc.derivs         = FALSE
          )
        )
        c(lme4::fixef(model),
          as.data.frame(lme4::VarCorr(model))$sdcor[2],
          as.vector(coef(summary(model))[ , "Std. Error"]))
        } , error = function(e) {
          rep(1e5, length(c(target_reg, target_se)))
        })
      }
      error_function <- function(candidate) {
        cor_vec   <- candidate_cor(candidate)
        cor_error <- sqrt(mean((round(cor_vec[!NA_cor], cor_dec) -
                                  target_cor[!NA_cor])^2))

        reg_vec   <- candidate_reg(candidate)
        k         <- length(target_reg)
        j         <- length(target_se)
        coef_err  <- round(reg_vec[1:k][!NA_reg], reg_dec) - target_reg[!NA_reg]
        se_err    <- round(reg_vec[(k+1):(k+j)][!NA_se], se_dec) - target_se[!NA_se]
        reg_error <- sqrt(mean(c(coef_err, se_err)^2))

        total_error <- cor_error * weight[1] + reg_error * weight[2]
        error_ratio <- cor_error / reg_error
        list(total_error = total_error, error_ratio = error_ratio)
      }

}

  ## SA parameter
  if (is.na(cooling_rate)) cooling_rate <- (max_iter-10)/max_iter
  temp <- init_temp

  ## restart loop
  for (start in seq_len(starts)) {
    if (progress_bar) {
      pb_update_interval <- floor(max_iter/100)
      pb <- utils::txtProgressBar(min=0, max=max_iter, style=3)
      on.exit(base::close(pb), add=TRUE)
    }

    track_error       <- numeric(max_iter)
    track_error_ratio <- numeric(max_iter)

    ## init candidate
    if (start==1) {
      current_candidate <- data.frame(long_pnumber, time=times, predictors)
      time_indices      <- split(seq_len(nrow(current_candidate)),
                                 current_candidate$time)
      within_names      <- pred_names
      p_indices         <- split(seq_len(nrow(long_data)),
                                 long_data$pnumber)
      if (!is.na(between_cols)) {
        between_names    <- names(long_data)[between_cols]
        within_names     <- setdiff(pred_names, between_names)
        current_candidate<- data.frame(
          long_pnumber,
          long_data[,between_names],
          time=times,
          long_data[,within_names]
        )
        colnames(current_candidate) <- col_names[-long_num_cols]
      } else {
        prob_within_move <- 1
      }
    }

    current_error <- error_function(current_candidate)$total_error
    best_candidate<- current_candidate
    best_error    <- current_error
    best_error_ratio <- error_function(current_candidate)$error_ratio

    ## inner SA
    for (i in seq_len(max_iter)) {
      candidate <- current_candidate

      if (stats::runif(1) < temp) {
        ## global within‐group shuffle
        for (t in unique_times) {
          idx <- time_indices[[t]]
          candidate[idx, within_names] <-
            candidate[sample(idx), within_names]
        }
      } else if (stats::runif(1) < prob_within_move) {
        ## local within‐group swap
        t   <- sample(unique_times,1)
        col <- sample(within_names,1)
        idx <- time_indices[[t]]
        pair <- sample(idx,2)
        candidate[pair, col] <- candidate[rev(pair), col]
      } else {
        ## between‐participant swap
        col  <- sample(between_names,1)
        pair <- sample(pnumber,2)
        idx1 <- p_indices[[as.character(pair[1])]]
        idx2 <- p_indices[[as.character(pair[2])]]
        tmp  <- candidate[idx1, col]
        candidate[idx1, col]<-candidate[idx2, col]
        candidate[idx2, col]<-tmp
      }

      err_list     <- error_function(candidate)
      candidate_err<- err_list$total_error
      if (candidate_err < current_error ||
          stats::runif(1) < exp((current_error-candidate_err)/temp)) {
        current_candidate <- candidate
        current_error     <- candidate_err
        if (current_error < best_error) {
          best_candidate <- current_candidate
          best_error     <- current_error
          best_error_ratio<- err_list$error_ratio
        }
      }

      temp             <- temp * cooling_rate
      track_error[i]   <- best_error
      track_error_ratio[i] <- best_error_ratio

      if (progress_bar && (i%%pb_update_interval==0)) {
        utils::setTxtProgressBar(pb, i)
      }
      if (best_error < tol) {
        cat("\nconverged!\n")
        break
      }
    }

    cat("\nBest error in start", start, "is", best_error, "\n")
    current_candidate <- best_candidate
    temp              <- init_temp/(2^start)
  }

  ## optional hill‐climb
  if (!is.na(hill_climbs)) {
    local_opt <- hill_climb(
      current_candidate = current_candidate,
      outcome           = outcome,
      error_function    = error_function,
      max_iter          = hill_climbs,
      mixedmodel        = TRUE,
      candidate_cols    = pred_names,
      unique_times      = unique_times,
      progress_bar      = progress_bar,
      prob_within_move  = prob_within_move,
      within_names      = within_names,
      time_indices      = time_indices,
      between_names     = between_names,
      pnumber           = pnumber,
      p_indices         = p_indices
    )
    best_error     <- local_opt$best_error
    best_candidate <- local_opt$best_candidate
  }

  ## assemble output
  best_solution <- cbind(best_candidate, outcome)
  colnames(best_solution) <- col_names
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
      prob_within_move = .8
    ),
    track_error       = track_error,
    track_error_ratio = track_error_ratio
  )
  class(result) <- "discourse.object"
  result
}
