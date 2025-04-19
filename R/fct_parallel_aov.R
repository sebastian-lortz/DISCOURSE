#' parallel_aov: parallel ANOVA optimization
#' @description Run ANOVA optimization across multiple parallel workers
#' @param sample_size Total sample size of observations or participants
#' @param levels Vector specifying number of levels for each factor
#' @param subgroup_sizes Optional subgroup sizes for unbalanced designs
#' @param target_group_means Vector of target means for each group
#' @param target_f_vec List with target F-values and related parameters
#' @param df_effects Degrees of freedom for each effect
#' @param marginal_means Optional precomputed marginal means
#' @param MSE Optional mean square error; if NULL, computed internally
#' @param range Numeric vector of length 2 specifying candidate value range
#' @param formula Model formula for computing F statistics
#' @param tolerance Convergence tolerance for the optimization
#' @param mixedmodel Logical; handle data as mixed model if TRUE
#' @param lme Logical; use lmer() when mixedmodel is TRUE
#' @param factor_type Vector indicating factor types ("between"/"within")
#' @param balanced Logical; assume a balanced design if TRUE
#' @param typeSS Type of sums of squares (e.g., 3 for Type III)
#' @param max_iter Maximum iterations for simulated annealing
#' @param init_temp Initial temperature for annealing
#' @param cooling_rate Cooling rate per iteration
#' @param pb_update_interval Progress bar update interval (NA = auto)
#' @param rcpp Logical; use Rcpp implementations if TRUE
#' @param integer Logical; treat candidate values as integers if TRUE
#' @param max_starts Number of restart cycles
#' @param checkGrim Logical; perform GRIM consistency checks if TRUE
#' @param parallel_start Number of independent parallel runs
#' @param return_best_solution Logical; return only the best run if TRUE
#' @export
parallel_aov <- function(
    sample_size,
    levels,
    subgroup_sizes = NULL,
    target_group_means,
    target_f_vec,
    df_effects,
    marginal_means = NULL,
    MSE = NULL,
    range,
    formula,
    tolerance,
    mixedmodel = FALSE,
    lme = FALSE,
    factor_type = NULL,
    balanced = FALSE,
    typeSS = 3,
    max_iter = 1e3,
    init_temp = 1,
    cooling_rate = 0.99,
    pb_update_interval = NA,
    rcpp = TRUE,
    integer = TRUE,
    max_starts = 3,
    checkGrim = TRUE,
    parallel_start = 3,
    return_best_solution = FALSE
) {
  # detect cores and make cluster
  cores <- parallel::detectCores() - 1
  cl    <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)

  # ensure cleanup on exit
  on.exit({
    parallel::stopCluster(cl)
    gc()
  })

  # packages to load on workers
  pkgs <- c("discourse", "Rcpp")

  # parallel optim_aov runs
  values <- foreach::foreach(
    i = seq_len(parallel_start),
    .packages = pkgs,
    .errorhandling = "pass"
  ) %dopar% {
    discourse::optim_aov(
      sample_size       = sample_size,
      levels            = levels,
      subgroup_sizes    = subgroup_sizes,
      target_group_means= target_group_means,
      target_f_vec      = target_f_vec,
      df_effects        = df_effects,
      marginal_means    = marginal_means,
      MSE               = MSE,
      range             = range,
      formula           = formula,
      tolerance         = tolerance,
      mixedmodel        = mixedmodel,
      lme               = lme,
      factor_type       = factor_type,
      balanced          = balanced,
      typeSS            = typeSS,
      max_iter          = max_iter,
      init_temp         = init_temp,
      cooling_rate      = cooling_rate,
      pb_update_interval= pb_update_interval,
      rcpp              = rcpp,
      integer           = integer,
      max_starts        = max_starts,
      checkGrim         = checkGrim
    )
  }

  # return best solution if requested
  if (return_best_solution) {
    errors <- vapply(
      values,
      function(x) if (inherits(x, "error")) NA_real_ else x$best_error,
      numeric(1)
    )
    idx <- which.min(errors)
    return(values[[idx]])
  }

  # return all results
  return(values)
}
