#' Estimate weighting factors for mean and SD errors
#'
#' Computes quasi-optimal weights balancing mean and standard deviation errors for multiple variables
#' by running Monte Carlo optimization to estimate relative baseline contribution of each term in the objective function.
#'
#' @description Compute weighting factors to match a target mean and SD across variables.
#' @param N Integer. Number of values in each vector.
#' @param target_mean Named numeric vector. Desired means for each variable (names identify columns).
#' @param target_sd Named numeric vector. Desired standard deviations for each variable.
#' @param range Numeric vector of length 2 or numeric matrix. Allowed value range for all variables (vector),
#'   or per-variable bounds as a two-row matrix matching `target_mean`.
#' @param obj_weight List of numeric vectors length 2, one per variable. Weights for mean vs. SD error. Default `c(1,1)`.
#' @param int.probs List of numeric vectors, one per variable. Sampling probabilities for integer moves; NULL for uniform.
#' @param integer Logical vector: TRUE for integer-valued vectors.
#' @param eps Numeric. Small constant to avoid division by zero in objective. Default `1e-12`.
#' @param est_iter Number of Monte Carlo iterations to estimate weights.
#' @param max_weight Maximum allowed weight magnitude.
#' @param metric Character: "mean" or "median" for summarizing estimated weights.
#'
#' @return A list of length equal to `target_mean`, where each element is a numeric vector
#'   of length 2 containing the estimated weights for mean vs. SD error for that variable.
#'
#' @example
#' # Estimate weights
#' weights_vec(
#'   N = 100,
#'   target_mean = c(5,10),
#'   target_sd   = c(2,3),
#'   integer     = c(TRUE,TRUE),
#'   range       = matrix(c(0,15, 0,20), nrow = 2),
#' )
#'
#' @export
#'
weights_vec <- function(N, target_mean, target_sd, range,
                        obj_weight = c(1, 1),
                        integer,
                        int.probs = NULL,
                        est_iter = 1000,
                        eps = .001,
                        max_weight = 10000,
                        metric = "mean") {

  # input checks
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer (sample size).")
  }
  if (!is.numeric(target_mean) || length(target_mean) < 1) {
    stop("`target_mean` must be a non-empty numeric vector of desired means.")
  }
  if (!is.numeric(target_sd) || length(target_sd) != length(target_mean)) {
    stop("`target_sd` must be a numeric vector of the same length as `target_mean`.")
  }
  if (!is.matrix(range) || nrow(range) != 2 ||
      ncol(range) != length(target_mean) || !is.numeric(range)) {
    stop("`range` must be a 2 x length(target_mean) numeric matrix specifying min/max for each variable.")
  }
  if (!is.numeric(obj_weight) || length(obj_weight) != 2) {
    stop("`obj_weight` must be a numeric vector of length 2 (weights for mean vs. SD errors).")
  }
  if (!is.logical(integer) || length(integer) < 1) {
    stop("`integer` must be a logical vector (length 1 or matching length of `target_mean`).")
  }
  if (!is.null(int.probs) && (!is.list(int.probs) || length(int.probs) != length(target_mean))) {
    stop("`int.probs`, if provided, must be a list of length equal to `target_mean`, each element a numeric probability vector.")
  }
  if (!is.numeric(est_iter) || length(est_iter) != 1 || est_iter <= 0 || est_iter != as.integer(est_iter)) {
    stop("`est_iter` must be a single positive integer (number of Monte Carlo iterations).")
  }
  if (!is.numeric(eps) || length(eps) != 1 || eps < 0) {
    stop("`eps` must be a single non-negative numeric constant.")
  }
  if (!is.numeric(max_weight) || length(max_weight) != 1 || max_weight <= 0) {
    stop("`max_weight` must be a single positive numeric value.")
  }
  if (!is.character(metric) || length(metric) != 1 || !metric %in% c("mean","median")) {
    stop("`metric` must be either \"mean\" or \"median\".")
  }

  weights_vec_single <- function(N, target_mean, target_sd, range,
                                 obj_weight = c(1, 1),
                                 integer,
                                 int.probs = int.probs,
                                 est_iter = est_iter,
                                 eps = eps,
                                 max_weight = max_weight,
                                 metric = metric) {
    result <- numeric(est_iter)
    for (i in seq_len(est_iter)) {
      # Generate candidate vector x_current
      if (integer) {
        allowed <- seq(range[1], range[2])
        n_allowed <- length(allowed)

        if (is.null(int.probs)) {
          probs <- rep(1 / n_allowed, n_allowed)
        }

        x_current <- sample(allowed, N, replace = TRUE, prob = probs)
      } else {
        # Continuous uniform draw
        x_current <- stats::runif(N, min = range[1], max = range[2])
      }
      # Compute standardized errors
      denom_mean <- max(abs(target_mean), eps)
      denom_sd <- max(abs(target_sd), eps)
      mean_err <- sqrt(((mean(x_current) - target_mean) / denom_mean)^2)
      sd_err   <- sqrt(((stats::sd(x_current) - target_sd) / denom_sd)^2)
      # Ratio of errors as weight estimate
      result[i] <- mean_err / sd_err
    }

    # Summarize weight distribution
    w_est <- switch(metric,
                    mean = mean(result),
                    median = stats::median(result))
    sd_weight <- min(max(round(w_est, log10(max_weight)), 1 / max_weight), max_weight)
    # Return normalized weights for mean and SD
    weights <- c(1, sd_weight) / min(1, sd_weight)
    return(weights)
  }

  # Vectorize over multiple variables
  n_var <- length(target_mean)
  obj_weight   <- rep(obj_weight[1], n_var)
  if (is.null(int.probs)) {
    int.probs <- vector("list", n_var)
    int.probs <- rep(list(NULL), n_var)
  }
  if (length(integer) < n_var) {
    integer <- rep(integer[1], n_var)
  }

  weights <- vector("list", n_var)
  for (i in seq_len(n_var)) {
    weights[[i]] <- weights_vec_single(
      N = N,
      target_mean = target_mean[i],
      target_sd   = target_sd[i],
      range       = range[, i],
      obj_weight  = obj_weight,
      integer     = integer[i],
      int.probs   = int.probs[[i]],
      est_iter    = est_iter,
      eps         = eps,
      max_weight  = max_weight,
      metric      = metric
    )
  }
  return(weights)
}
