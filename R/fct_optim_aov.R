#' Optimize group means via simulated annealing to match ANOVA F-values
#'
#'
#' @param N Total number of subjects
#' @param levels Vector of factor levels per factor
#' @param subgroup_sizes Optional subgroup sizes for unbalanced designs
#' @param target_group_means Numeric vector of target means for each group
#' @param target_f_list List with:
#'   - F: target F statistics vector
#'   - effect: character vector of effect names
#'   - contrast: (optional) contrast formula
#'   - contrast_method: (optional) contrast method name
#' @param df_effects Degrees of freedom for each effect (consider removing)
#' @param range Numeric vector length 2 specifying allowed candidate range
#' @param formula Model formula for F extraction
#' @param tolerance Convergence tolerance for optimization
#' @param factor_type Factor types ("between"/"within") for mixed models
#' @param typeSS Type of sums of squares (2 or 3)
#' @param max_iter Maximum iterations per start
#' @param init_temp Initial temperature for annealing
#' @param cooling_rate Cooling rate per iteration
#' @param max_step Proportion; The maximum step size for modifications as proportion of the range
#' @param progress_bar  Show progress bar if TRUE
#' @param integer Logical; treat candidate values as integers
#' @param max_starts Number of annealing restarts
#' @param checkGrim Logical; perform GRIM checks on target means
#' @param min_decimals Integer; then minimum number of decimals (trailing zeros) of the target
#' @return A "discourse.object" list with optimized data, error, inputs, and trace
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
    progress_bar = TRUE,
    max_starts = 1,
    checkGrim = FALSE,
    min_decimals = 1
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
  # prepare design matrix
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
  # GRIM consistency checks
  mean_dec <- count_decimals(target_group_means, min_decimals = min_decimals)
  if (checkGrim && integer) {
    for (i in seq_along(group_sizes)) {
      check_grim(n = group_sizes[i], target_mean = target_group_means[i], decimals = mean_dec[i])
    }
  }

  # F-value extraction function
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
  max_step_cont <- (range[2]-range[1])*max_step

  # continous move
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
  # set max integer step
  max_step_int <- as.integer(max(1,round((range[2]-range[1])*max_step)))

  # integer move
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

  # set move
  move_fun <- if (integer) heuristic_move_int else heuristic_move_cont

  # initial solution
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

  # simulated annealing
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

      #cat("Means:", tapply(candidate, group_ids, mean), "\n")
      #cat("SDs:", tapply(candidate, group_ids, sd), "\n")
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
      }
      if (best_error < tolerance) break
    }
    if (progress_bar) {close(pb)}
    current_candidate <- best_candidate
    if (best_error < tolerance) break
    cat("\nBest error in start", s, "is", best_error, "\n")
    temp <- init_temp / (2 ^ s)
    }

  # result assembly
  out_data <- if (!all(factor_type == "between")) {
    data.frame(ID = ID, factor_mat, outcome = best_candidate)
  } else {
    data.frame(factor_mat, outcome = best_candidate)
  }

  res <- list(
    best_error  = best_error,
    data        = out_data,
    inputs      = as.list(environment()),
    track_error = track_error
  )
  class(res) <- "discourse.object"
  res
}
