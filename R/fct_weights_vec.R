#' weights_vec
#'
#' @description Compute weighting factors to match a target mean and SD across variables.
#' @param N Sample size for generated vectors.
#' @param target_mean Numeric vector of desired means.
#' @param target_sd Numeric vector of desired standard deviations.
#' @param range Numeric matrix (2 x length(target_mean)) specifying [min, max] for each variable.
#' @param obj_weight Weighting for objective terms (mean vs. SD).
#' @param int.probs Numeric; a list with a vector of sampling probabilities for each variable.
#' @param integer Logical vector: TRUE for integer-valued vectors.
#' @param est_iter Number of Monte Carlo iterations to estimate weights.
#' @param eps Small constant to avoid division by zero.
#' @param max_weight Maximum allowed weight magnitude.
#' @param metric Character: "mean" or "median" for summarizing estimated weights.
#' @return A list of weight pairs (mean, SD) for each target variable.
#' @export
weights_vec <- function(N, target_mean, target_sd, range,
                        obj_weight = c(1, 1),
                        integer,
                        int.probs = NULL,
                        est_iter = 1000,
                        eps = .001,
                        max_weight = 10000,
                        metric = "mean") {

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
