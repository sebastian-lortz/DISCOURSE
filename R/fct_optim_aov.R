#' Optimize simulated data to match ANOVA F-values
#'
#' Uses the DISCOURSE algorithmic framework to simulate data that
#' produce target ANOVA F-statistics under a specified factorial design given input parameters.
#'
#' @param N Integer. Total number of subjects (sum of `subgroup_sizes`).
#' @param levels Integer vector. Number of factor levels per factor in the design.
#' @param subgroup_sizes Numeric vector. Optional sizes of each between-subjects group for unbalanced designs; length must equal product of `levels` for between factors.
#' @param target_group_means Numeric vector. Desired means for each group in the design.
#' @param target_f_list List with components:
#'   \describe{
#'     \item{F}{Numeric vector of target F-statistics.}
#'     \item{effect}{Character vector of effect names matching `F`.}
#'     \item{contrast}{Optional character formula for contrasts.}
#'     \item{contrast_method}{Optional character specifying contrast method.}
#'   }
#' @param formula Formula or character. Model formula used to compute F-values (e.g., `y ~ A + B + A*B`).
#' @param factor_type Character vector. Type of each factor (`"between"` or `"within"`) matching length of `levels`.
#' @param range Numeric vector of length 2. Lower and upper bounds for candidate means.
#' @param integer Logical. If TRUE, candidate values are treated as integers, if FALSE treated as continous values.
#' @param typeSS Integer. Type of sums-of-squares for ANOVA (2 or 3). Default is 3.
#' @param df_effects Numeric vector. Degrees of freedom of the model effects. Default is `NULL`.
#' @param max_iter Integer. Maximum iterations per restart. Default is 1e3.
#' @param max_starts Integer. Number of annealing restarts. Default is 1.
#' @param init_temp Numeric. Initial temperature for annealing. Default is 1.
#' @param cooling_rate Numeric. Cooling rate per iteration (between 0 and 1); if NULL, calculated automatically as `(init_temp-10)/init_temp`.
#' @param max_step Numeric. Maximum move size as proportion of `range`. Default is 0.2.
#' @param tolerance Numeric. Error tolerance for convergence; stops early if best error < `tolerance`. Default `1e-6`.
#' @param checkGrim Logical. If TRUE and `integer = TRUE`, perform GRIM checks on `target_group_means`. Default is FALSE.
#' @param min_decimals Integer. Minimum number of decimal places for target values (including trailing zeros). Default `1`.
#' @param progress_bar Logical. Show text progress bar during optimization. Default is TRUE.
#' @param progress_mode Character. Either "console" or "shiny" for progress handler. Default `console`.
#'
#' @return A `discourse.object` list containing:
#' \describe{
#'   \item{best_error}{Numeric. Minimum error (RMSE) achieved.}
#'   \item{data}{Data frame of optimized outcome values (and grouping variables).}
#'   \item{inputs}{List of all input arguments.}
#'   \item{track_error}{Numeric vector of best error at each iteration.}
#'   \item{grim}{List of the GRIM results.}
#' }
#'
#' @example
#' # Balanced 2x2 design
#' optim_aov(
#'   N = 40,
#'   levels = c(2, 2),
#'   target_group_means = c(1, 2, 3, 4),
#'   target_f_list = list(effect = c("A", "B"),
#'                        F = c(5.6, 8.3), ),
#'   formula = y ~ A + B + A*B,
#'   factor_type = c("between", "between"),
#'   range = c(0, 5),
#'   integer = FALSE,
#'   max_iter = 1000,
#'   max_starts = 3
#' )
#'
#' @export
optim_aov <- function(
    N,
    levels,
    target_group_means,
    target_f_list,
    integer,
    range,
    formula,
    factor_type,
    subgroup_sizes = NULL,
    df_effects = NULL,
    tolerance = 1e-8,
    typeSS = 3,
    max_iter = 1e3,
    init_temp = 1,
    cooling_rate = NULL,
    max_step = .2,
    max_starts = 1,
    checkGrim = FALSE,
    min_decimals = 1,
    progress_bar = TRUE,
    progress_mode = "console"
) {

  # input checks
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer.")
  }
  if (!is.numeric(levels) || length(levels) < 1) {
    stop("`levels` must be a numeric vector of length > 0 specifying factor levels per factor.")
  }
  if (!is.numeric(target_group_means) || length(target_group_means) < 1) {
    stop("`target_group_means` must be a non-empty numeric vector of target means.")
  }
  if (!is.list(target_f_list) ||
      !is.numeric(target_f_list$F) || length(target_f_list$F) < 1) {
    stop("`target_f_list` must be a list with a numeric vector `F` of target F statistics.")
  }
  if (!is.character(target_f_list$effect) ||
      length(target_f_list$effect) != length(target_f_list$F)) {
    stop("`target_f_list$effect` must be a character vector the same length as `target_f_list$F`.")
  }
  if (!is.character(formula) || length(formula) != 1) {
    stop("`formula` must be a single character string giving the regression formula.")
  } else {
    formula <- stats::as.formula(formula)
  }
  if (!is.character(factor_type) ||
      length(factor_type) != length(levels) ||
      !all(factor_type %in% c("between", "within"))) {
    stop("`factor_type` must be a character vector ('between'/'within') matching length of `levels`.")
  }
  n_between <- prod(levels[factor_type == "between"])
  if (!is.null(subgroup_sizes) &&
      (!is.numeric(subgroup_sizes) ||
       length(subgroup_sizes) != n_between)) {
    stop("`subgroup_sizes`, if provided, must be a numeric vector matching the number of between-subject groups.")
  }
  if (N != sum(subgroup_sizes)) {
    stop("`N` must equal the sum of `subgroup_sizes` and thus, the number of subjects (not observations).")
  }
  if (!is.null(df_effects) &&
      (!is.numeric(df_effects) ||
       length(df_effects) != length(target_f_list$F))) {
    stop("`df_effects`, if provided, must be a numeric vector the same length as `target_f_list$F`.")
  }
  if (!is.numeric(range) || length(range) != 2) {
    stop("`range` must be a numeric vector of length 2 specifying allowed candidate range.")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("`tolerance` must be a single non-negative numeric value.")
  }
  if (!is.numeric(typeSS) || length(typeSS) != 1 || !typeSS %in% c(2, 3)) {
    stop("`typeSS` must be either 2 or 3.")
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    stop("`max_iter` must be a single positive integer (or numeric convertible to integer).")
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    stop("`init_temp` must be a single positive numeric value.")
  }
  if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
         cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate))) {
    stop("`cooling_rate` must be a single numeric between 0 and 1, or NULL.")
  }
  if (!is.numeric(max_step) || length(max_step) != 1 ||
      max_step <= 0 || max_step >= 1) {
    stop("`max_step` must be a single numeric between 0 and 1.")
  }
  if (!is.logical(progress_bar) || length(progress_bar) != 1) {
    stop("`progress_bar` must be a single logical value.")
  }
  if (!is.logical(integer) || length(integer) != 1) {
    stop("`integer` must be a single logical value.")
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    stop("`max_starts` must be a single positive integer.")
  }
  if (!is.logical(checkGrim) || length(checkGrim) != 1) {
    stop("`checkGrim` must be a single logical value.")
  }
  if (!is.numeric(min_decimals) || length(min_decimals) != 1 ||
      min_decimals < 0 || min_decimals != as.integer(min_decimals)) {
    stop("`min_decimals` must be a single non-negative integer.")
  }

  # configure contrasts
  if (typeSS == 3) {
    options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
  } else if (typeSS == 2) {
    options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
  }

  # design matrix
  if (all(factor_type == "between")) {
    factor_mat <- factor_matrix(N, levels, subgroup_sizes)
  } else {
    temp_mat   <- mixed_factor_matrix(N, levels, factor_type, subgroup_sizes)
    ID    <- temp_mat[, 1]
    factor_mat <- temp_mat[, -1, drop = FALSE]
  }
  uniq_mat    <- unique(factor_mat)
  group_ids   <- apply(factor_mat, 1, paste0, collapse = "")
  group_idx   <- split(seq_along(group_ids), group_ids)
  target_F    <- target_f_list$F
  group_sizes <- as.vector(table(group_ids))
  balanced <- FALSE
  if (length(unique(group_sizes)) == 1) {
    balanced = TRUE
  }

  # GRIM
  mean_dec <- count_decimals(target_group_means, min_decimals = min_decimals)
  if (checkGrim && integer) {
    for (i in seq_along(group_sizes)) {
    grim <- check_grim(n = group_sizes[i], target_mean = target_group_means[i], decimals = mean_dec[i])
    }
  }

  # F value extraction
  if (all(factor_type == "between")) {
    extract_F <- function(data, effect, contrast, contrast_method, type) {
      fit    <- stats::lm(formula, data = data)
      an_tab <- car::Anova(fit, type = type)
      rn     <- trimws(rownames(an_tab))
      main_F <- sapply(effect, function(e) an_tab[rn == e, "F value"] )
      if (!is.null(contrast)) {
        emm   <- emmeans::emmeans(fit, stats::as.formula(paste("~", contrast)))
        ct    <- emmeans::contrast(emm, method = contrast_method)
        c(main_F, summary(ct)$t.ratio^2)
      } else {
        main_F
      }
    }
  } else {
    extract_F <- function(data, effect, contrast, contrast_method, type) {
      res   <- afex::aov_car(formula = formula, data = data, factorize = TRUE, type = type)
      an_tab <- res$anova_table
      rn     <- trimws(rownames(an_tab))
      sapply(effect, function(e) an_tab[rn == e, "F"] )
    }
  }

  # objective function
  F_dec <- max(count_decimals(target_F, min_decimals = min_decimals))
  if (!all(factor_type == "between")) {
    objective <- function(x) {
      dat    <- data.frame(ID = ID, factor_mat, outcome = x)
      comp_F <- extract_F(dat,
                          target_f_list$effect,
                          target_f_list$contrast,
                          target_f_list$contrast_method,
                          typeSS)
      sqrt(mean((round(comp_F, F_dec) - target_F)^2))
    }
  } else if (!is.null(target_f_list$contrast) || !balanced) {
    objective <- function(x) {
      dat    <- data.frame(factor_mat, outcome = x)
      comp_F <- extract_F(dat,
                          target_f_list$effect,
                          target_f_list$contrast,
                          target_f_list$contrast_method,
                          typeSS)
      sqrt(mean((round(comp_F, F_dec) - target_F)^2))
    }
  } else {
      MSE <- compute_sequential_MSE(
        target_group_means,
        group_sizes,
        uniq_mat,
        target_F,
        df_effects
      )
    objective <- function(x) {
      SDs    <- sapply(group_idx, function(idx) stats::sd(x[idx]))
      pooled <- sum((group_sizes - 1) * SDs^2) / sum(group_sizes - 1)
      sqrt((pooled - MSE)^2)
    }
  }

  # heuristic move continuous data
  max_step_cont <- (range[2]-range[1])*max_step
  heuristic_move_cont <- function(candidate) {
    idx <- sample(group_idx[[sample.int(length(group_idx), 1)]], 2)
    v   <- candidate[idx]
    d   <- c(
      max(range[1] - v[1], v[2] - range[2]),
      min(range[2] - v[1], v[2] - range[1])
    )
    if (d[1] > d[2]) return(candidate)
    d[1] <- max(d[1], -max_step_cont)
    d[2] <- min(d[2],  max_step_cont)
    inc <- stats::runif(1, d[1], d[2])
    candidate[idx] <- v + c(inc, -inc)
    candidate
  }

  # heuristic move integer data
  max_step_int <- as.integer(max(1,round((range[2]-range[1])*max_step)))
  heuristic_move_int <- function(candidate) {
    idx <- sample(group_idx[[sample.int(length(group_idx), 1)]], 2)
    v   <- candidate[idx]
    d   <- c(
      max(range[1] - v[1], v[2] - range[2]),
      min(range[2] - v[1], v[2] - range[1])
    )
    if (d[1] > d[2]) return(candidate)
    d[1] <- max(d[1], -max_step_int)
    d[2] <- min(d[2],  max_step_int)
    inc <- sample(seq(d[1], d[2]), 1)
    candidate[idx] <- v + c(inc, -inc)
    candidate
  }

  # move function
  move_fun <- if (integer) heuristic_move_int else heuristic_move_cont

  # init parameters
  if (integer) {
      elements   <- mapply(
      generate_candidate_group,
      target_group_means,
      group_sizes,
      MoreArgs = list(range),
      SIMPLIFY = FALSE)
      current_candidate <- numeric(length(group_ids))
      for (j in seq_along(unique(group_ids))) {
        idxs <- group_idx[[unique(group_ids)[j]]]
        current_candidate[idxs] <- elements[[j]]
      }
    } else {
    current_candidate <- target_group_means[match(group_ids, unique(group_ids))]
  }
  if (is.null(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }
  temp     <- init_temp
  best_candidate <- current_candidate
  best_error <- objective(current_candidate)

  # set progressr
  if(progress_mode == "shiny") {
    handler <- list(progressr::handler_shiny())
  } else {
    handler <-list(progressr::handler_txtprogressbar())
  }
  pb_interval <- max(floor(max_iter / 100), 1)

  # Optimization Process
  progressr::with_progress({
    p <- progressr::progressor(steps = (max_iter*max_starts)/pb_interval)
      for (s in seq_len(max_starts)) {
        if (progress_bar) {
          pb_interval <- floor(max_iter / 100)
          pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
          on.exit(close(pb), add = TRUE)
        }
        track_error       <- numeric(max_iter)
        for (i in seq_len(max_iter)) {
          candidate <- current_candidate
          candidate <- move_fun(candidate)
          current_error   <- objective(candidate)
          prob  <- exp((best_error - current_error) / temp)
          if (current_error < best_error || stats::runif(1) < prob) {
            current_candidate    <- candidate
            if (current_error < best_error) {
            best_error <- current_error
            best_candidate <- current_candidate
            }
          }
          temp <- temp * cooling_rate
          track_error[i]       <- best_error
          if (progress_bar && (i %% pb_interval == 0)) {
            utils::setTxtProgressBar(pb, i)
            p()
          }
          if (best_error < tolerance) break
        }
        if (progress_bar) {close(pb)}
        current_candidate <- best_candidate
        if (best_error < tolerance) break
        cat("\nBest error in start", s, "is", best_error, "\n")
        temp <- init_temp / (2 ^ s)
        }
      },
  handlers = handler
  )

  # combine results
  out_data <- if (!all(factor_type == "between")) {
    data.frame(ID = ID, factor_mat, outcome = best_candidate)
  } else {
    data.frame(factor_mat, outcome = best_candidate)
  }

  # assemble output
  res <- list(
    best_error  = best_error,
    data        = out_data,
    inputs      = list(
      N = N,
      levels = levels,
      target_group_means = target_group_means,
      target_f_list = target_f_list,
      integer = integer,
      range = range,
      formula = formula,
      factor_type = factor_type,
      subgroup_sizes = subgroup_sizes,
      df_effects = df_effects,
      tolerance = tolerance,
      typeSS = typeSS,
      max_iter = max_iter,
      init_temp = init_temp,
      cooling_rate = cooling_rate,
      max_step = max_step,
      max_starts = max_starts,
      checkGrim = checkGrim,
      min_decimals = min_decimals,
      progress_bar = progress_bar,
      progress_mode = progress_mode
    ),
    track_error = track_error,
    grim        = grim
  )
  class(res) <- "discourse.object"
  res
}
