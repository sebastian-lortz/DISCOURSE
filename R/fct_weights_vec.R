#' weights_vec
#'
#' @description Compute weighting factors to match a target mean and SD across variables.
#' @param N Sample size for generated vectors.
#' @param target_mean Numeric vector of desired means.
#' @param target_sd Numeric vector of desired standard deviations.
#' @param range Numeric matrix (2 x length(target_mean)) specifying [min, max] for each variable.
#' @param init_distr Initialization distribution: "uniform" or "normal" (integers only).
#' @param skew Skewness parameter for the skew-normal init_distr.
#' @param kurt Kurtosis adjustment factor for init_distr.
#' @param obj_weight Weighting for objective terms (mean vs. SD).
#' @param prior_weight Blend factor between prior and uniform for init_distr.
#' @param integer Logical vector: TRUE for integer-valued vectors.
#' @param est_iter Number of Monte Carlo iterations to estimate weights.
#' @param eps Small constant to avoid division by zero.
#' @param max_weight Maximum allowed weight magnitude.
#' @param metric Character: "mean" or "median" for summarizing estimated weights.
#' @return A list of weight pairs (mean, SD) for each target variable.
#' @export
weights_vec <- function(N, target_mean, target_sd, range,
                        init_distr = "uniform",   # Options: "uniform", "normal"
                        skew = 0,
                        kurt = 1,
                        obj_weight = c(1, 1),
                        prior_weight = 0.5,
                        integer = NULL,
                        est_iter = 1000,
                        eps = .001,
                        max_weight = 10000,
                        metric = "mean") {

  weights_vec_single <- function(N, target_mean, target_sd, range,
                                 init_distr = "uniform",
                                 skew = 0,
                                 kurt = 1,
                                 obj_weight = c(1, 1),
                                 prior_weight = 0.5,
                                 integer = TRUE,
                                 est_iter = 1000,
                                 eps = .001,
                                 max_weight = 10000,
                                 metric = "mean") {
    result <- numeric(est_iter)
    for (i in seq_len(est_iter)) {
      # Generate candidate vector x_current
      if (integer) {
        allowed <- seq(range[1], range[2])
        n_allowed <- length(allowed)
        if (init_distr == "normal") {
          # Skew-normal probabilities over integer bins
          probs <- sn::psn(allowed + 0.5, xi = target_mean, omega = target_sd, alpha = skew) -
            sn::psn(allowed - 0.5, xi = target_mean, omega = target_sd, alpha = skew)
          probs <- probs / sum(probs)
          if (kurt != 1) {
            # Adjust for kurtosis
            probs <- probs^kurt / sum(probs)
          }
          # Blend with uniform prior
          probs_uniform <- rep(1 / n_allowed, n_allowed)
          probs <- prior_weight * probs + (1 - prior_weight) * probs_uniform
          probs <- probs / sum(probs)
        } else {
          # Uniform init distribution
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
      mean_err <- ((mean(x_current) - target_mean) / denom_mean)^2
      sd_err   <- ((stats::sd(x_current) - target_sd) / denom_sd)^2
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
  init_distr   <- rep(init_distr[1], n_var)
  skew         <- rep(skew[1], n_var)
  kurt         <- rep(kurt[1], n_var)
  prior_weight <- rep(prior_weight[1], n_var)
  obj_weight   <- rep(obj_weight[1], n_var)
  integer      <- rep(integer[1], n_var)

  weights <- vector("list", n_var)
  for (i in seq_len(n_var)) {
    weights[[i]] <- weights_vec_single(
      N = N,
      target_mean = target_mean[i],
      target_sd   = target_sd[i],
      range       = range[, i],
      init_distr  = init_distr[i],
      skew        = skew[i],
      kurt        = kurt[i],
      obj_weight  = obj_weight,
      prior_weight= prior_weight[i],
      integer     = integer[i],
      est_iter    = est_iter,
      eps         = eps,
      max_weight  = max_weight,
      metric      = metric
    )
  }
  return(weights)
}
