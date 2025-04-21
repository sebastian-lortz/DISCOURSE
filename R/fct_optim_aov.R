#' Optimize group means via simulated annealing to match ANOVA F-values
#'
#' @param sample_size Total number of observations or participants
#' @param levels Vector of factor levels per factor
#' @param subgroup_sizes Optional subgroup sizes for unbalanced designs
#' @param target_group_means Numeric vector of target means for each group
#' @param target_f_vec List with:
#'   - F: target F statistics vector
#'   - effect: character vector of effect names
#'   - contrast: (optional) contrast formula
#'   - contrast_method: (optional) contrast method name
#' @param df_effects Degrees of freedom for each effect
#' @param marginal_means (unused) precomputed marginal means
#' @param MSE Optional mean square error; computed if NULL
#' @param range Numeric vector length 2 specifying allowed candidate range
#' @param formula Model formula for F extraction
#' @param tolerance Convergence tolerance for optimization
#' @param mixedmodel Logical; TRUE for mixed‑model (long format)
#' @param factor_type Factor types ("between"/"within") for mixed models
#' @param balanced Logical; assume balanced design if TRUE
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
    sample_size,
    levels,
    subgroup_sizes = NULL,
    target_group_means,
    target_f_vec,
    df_effects,
    MSE = NULL,
    range,
    formula,
    tolerance,
    mixedmodel = FALSE,
    factor_type = NULL,
    balanced = FALSE,
    typeSS = 3,
    max_iter = 1e3,
    init_temp = 1,
    cooling_rate = NA,
    pb_update_interval = NA,
    integer = TRUE,
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
  if (!mixedmodel) {
    factor_mat <- factor_matrix(sample_size, levels, subgroup_sizes)
  } else {
    temp_mat   <- mixed_factor_matrix(sample_size, levels, factor_type, subgroup_sizes)
    pnumber    <- temp_mat[, 1]
    factor_mat <- temp_mat[, -1, drop = FALSE]
  }

  uniq_mat    <- unique(factor_mat)
  group_ids   <- apply(factor_mat, 1, paste0, collapse = "")
  group_idx   <- split(seq_along(group_ids), group_ids)
  target_F    <- target_f_vec$F
  group_sizes <- as.vector(table(group_ids))

  # GRIM consistency checks
  mean_dec <- count_decimals(target_group_means)
  if (checkGrim && integer) {
    for (i in seq_along(group_sizes)) {
      check_grim(group_sizes[i], target_group_means[i], mean_dec)
    }
  }

  # F-value extraction function
  if (!mixedmodel) {
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
  if (mixedmodel) {
    objective <- function(x) {
      dat    <- data.frame(pnumber = pnumber, factor_mat, outcome = x)
      comp_F <- extract_F(dat,
                          target_f_vec$effect,
                          target_f_vec$contrast,
                          target_f_vec$contrast_method,
                          typeSS)
      sqrt(mean((round(comp_F, count_decimals(comp_F)) - target_F)^2))
    }
  } else if (!is.null(target_f_vec$contrast) || !balanced) {
    objective <- function(x) {
      dat    <- data.frame(factor_mat, outcome = x)
      comp_F <- extract_F(dat,
                          target_f_vec$effect,
                          target_f_vec$contrast,
                          target_f_vec$contrast_method,
                          typeSS)
      sqrt(mean((round(comp_F, count_decimals(comp_F)) - target_F)^2))
    }
  } else {
    if (is.null(MSE)) {
      MSE <- compute_sequential_MSE(
        target_group_means,
        group_sizes,
        uniq_mat,
        target_F,
        df_effects
      )
    }
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
    x_cur   <- unlist(mapply(
      generate_candidate_group,
      target_group_means,
      sizes,
      MoreArgs = list(range),
      SIMPLIFY = FALSE
    ))
  } else {
    x_cur <- target_group_means[match(group_ids, unique(group_ids))]
  }

  if (is.na(cooling_rate)) {
    cooling_rate <- (max_iter - 10) / max_iter
  }
  temp     <- init_temp
  best_sol <- x_cur
  best_err <- objective(x_cur)
  if (is.na(pb_update_interval)) pb_update_interval <- floor(max_iter / 100)

  # simulated annealing
  for (s in seq_len(max_starts)) {
    pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)
    track_error       <- numeric(max_iter)
    for (i in seq_len(max_iter)) {
      x_new <- move_fun(x_cur)
      err   <- objective(x_new)
      prob  <- exp((best_err - err) / temp)
      if (err < best_err || stats::runif(1) < prob) {
        x_cur    <- x_new
        best_err <- err
        best_sol <- x_cur
      }
      temp <- temp * cooling_rate
      track_error[i]       <- best_err
      if (i %% pb_update_interval == 0) utils::setTxtProgressBar(pb, i)
      if (best_err < tolerance) break
    }
    close(pb)
    x_cur <- best_sol
    if (best_err < tolerance) break
    cat("\nBest error in start", s, "is", best_err, "\n")
  }

  # result assembly
  out_data <- if (mixedmodel) {
    data.frame(pnumber = pnumber, factor_mat, outcome = best_sol)
  } else {
    data.frame(factor_mat, outcome = best_sol)
  }

  res <- list(
    best_error  = best_err,
    data        = out_data,
    inputs      = as.list(environment()),
    track_error = track_error
  )
  class(res) <- "discourse.object"
  res
}
