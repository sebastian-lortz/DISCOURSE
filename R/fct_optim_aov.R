#' Optimize group means via simulated annealing to match ANOVA F-values
#'
#' @param N Total number of observations
#' @param levels Vector of factor levels per factor
#' @param subgroup_sizes Optional subgroup sizes for unbalanced designs
#' @param target_group_means Numeric vector of target means for each group
#' @param target_f_list List with:
#'   - F: target F statistics vector
#'   - effect: character vector of effect names
#'   - contrast: (optional) contrast formula
#'   - contrast_method: (optional) contrast method name
#' @param df_effects Degrees of freedom for each effect
#' @param range Numeric vector length 2 specifying allowed candidate range
#' @param formula Model formula for F extraction
#' @param tolerance Convergence tolerance for optimization
#' @param factor_type Factor types ("between"/"within") for mixed models
#' @param typeSS Type of sums of squares (2 or 3)
#' @param max_iter Maximum iterations per start
#' @param init_temp Initial temperature for annealing
#' @param cooling_rate Cooling rate per iteration
#' @param pb_update_interval Progress bar update interval; auto if NA
#' @param integer Logical; treat candidate values as integers
#' @param max_starts Number of annealing restarts
#' @param checkGrim Logical; perform GRIM checks on target means
#' @return A "discourse.object" list with optimized data, error, inputs, and trace
#' @export
optim_aov <- function(
    N,
    levels,
    subgroup_sizes = NULL,
    target_group_means,
    target_f_list,
    df_effects,
    integer,
    range,
    formula,
    tolerance,
    factor_type = "between",
    typeSS = 3,
    max_iter = 1e3,
    init_temp = 1,
    cooling_rate = NA,
    pb_update_interval = NA,
    max_starts = 3,
    checkGrim = TRUE
) {
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
    pnumber    <- temp_mat[, 1]
    factor_mat <- temp_mat[, -1, drop = FALSE]
  }

  uniq_mat    <- unique(factor_mat)
  group_ids   <- apply(factor_mat, 1, paste0, collapse = "")
  group_idx   <- split(seq_along(group_ids), group_ids)
  target_F    <- target_f_list$F
  group_sizes <- as.vector(table(group_ids))
  if (length(unique(group_sizes)) == 1) {
    balanced = TRUE
  }
  # GRIM consistency checks
  mean_dec <- count_decimals(target_group_means)
  if (checkGrim && integer) {
    for (i in seq_along(group_sizes)) {
      check_grim(group_sizes[i], target_group_means[i], mean_dec)
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
  F_dec <- count_decimals(target_F)
  if (!all(factor_type == "between")) {
    objective <- function(x) {
      dat    <- data.frame(pnumber = pnumber, factor_mat, outcome = x)
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

  # neighbor generation
  heuristic_move_cont <- function(candidate) {
    idx <- sample(group_idx[[sample.int(length(group_idx), 1)]], 2)
    v   <- candidate[idx]
    d   <- c(
      max(range[1] - v[1], v[2] - range[2]),
      min(range[2] - v[1], v[2] - range[1])
    )
    if (d[1] > d[2]) return(candidate)
    inc <- stats::runif(1, d[1], d[2])
    candidate[idx] <- v + c(inc, -inc)
    candidate
  }

  heuristic_move_int <- function(candidate) {
    idx <- sample(group_idx[[sample.int(length(group_idx), 1)]], 2)
    v   <- candidate[idx]
    d   <- c(
      max(range[1] - v[1], v[2] - range[2]),
      min(range[2] - v[1], v[2] - range[1])
    )
    if (d[1] > d[2]) return(candidate)
    inc <- sample(seq(d[1], d[2]), 1)
    candidate[idx] <- v + c(inc, -inc)
    candidate
  }

  move_fun <- if (integer) heuristic_move_int else heuristic_move_cont

  # initial solution
  if (integer) {
    sizes   <- as.vector(table(group_ids))
    current_candidate   <- unlist(mapply(
      generate_candidate_group,
      target_group_means,
      sizes,
      MoreArgs = list(range),
      SIMPLIFY = FALSE
    ))
  } else {
    current_candidate <- target_group_means[match(group_ids, unique(group_ids))]
  }

  if (is.na(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }
  temp     <- init_temp
  best_candidate <- current_candidate
  best_error <- objective(current_candidate)
  if (is.na(pb_update_interval)) pb_update_interval <- floor(max_iter / 100)

  # simulated annealing
  for (s in seq_len(max_starts)) {
    pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
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
      if (i %% pb_update_interval == 0) utils::setTxtProgressBar(pb, i)
      if (best_error < tolerance) break
    }
    close(pb)
    current_candidate <- best_candidate
    if (best_error < tolerance) break
    cat("\nBest error in start", s, "is", best_error, "\n")
  }

  # result assembly
  out_data <- if (!all(factor_type == "between")) {
    data.frame(pnumber = pnumber, factor_mat, outcome = best_candidate)
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
