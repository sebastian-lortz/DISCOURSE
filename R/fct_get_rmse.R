#' Compute RMSE for a single discourse.object result
#'
#' Calculates root-mean-square error (RMSE) metrics for a single `discourse.object` output,
#' comparing model estimates of the simulated data against original target inputs.
#'
#' @param result A `discourse.object` produced by analysis functions (e.g., `optim_vec`, `optim_aov()`, `optim_lm()`, `optim()`).
#' @return A named `list` of RMSE values. Possible elements:
#' \describe{
#'   \item{rmse_cor}{Numeric. RMSE of correlation estimates for LM- and LME-based objects.}
#'   \item{rmse_reg}{Numeric. RMSE of regression coefficient estimates for LM- and LME-based objects.}
#'   \item{rmse_se}{Numeric. RMSE of standard error estimates for LM- and LME-based objects.}
#'   \item{rmse_F}{Numeric. RMSE of F-values for ANOVA-based objects.}
#'   \item{rmse_mean}{Numeric. RMSE of means for vector-based objects.}
#'   \item{rmse_sd}{Numeric. RMSE of standard deviations for vector-based objects.}
#' }
#' @example
#' # Regression-based result
#' result <- optim_lm(args = ..., ...)
#' get_rmse(result)
#'
#' @export
get_rmse <- function(result) {
  if (!inherits(result, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  # lm and lme module
  if (!is.null(result$inputs$target_reg)) {
    tc <- result$inputs$target_cor;  tr <- result$inputs$target_reg;  ts <- result$inputs$target_se
    dc <- max(count_decimals(tc));  dr <- max(count_decimals(tr))
    ds <- if (!is.null(ts)) max(count_decimals(ts)) else NULL
    rmse <- function(x, y) sqrt(mean((x - y)^2))
    st <- get_stats(result)
    cor_v <- round(st$cor[!is.na(tc)], dc);  reg_v <- round(st$reg[!is.na(tr)], dr)
    se_v  <- if (!is.null(ts)) round(st$se[!is.na(ts)], ds) else NULL
    rc <- rmse(cor_v, tc[!is.na(tc)])
    rr <- rmse(reg_v, tr[!is.na(tr)])
    rs <- if (!is.null(se_v)) rmse(se_v, ts[!is.na(ts)]) else NA
    return(list(rmse_cor = rc, rmse_reg = rr, rmse_se = rs))
  } else if (!is.null(result$inputs$target_f_list)) {
    # aov module
    tf <- result$inputs$target_f_list$F
    df <- count_decimals(tf)
    f_v <- round(get_stats(result)$F_value, df)
    rf  <- sqrt(mean((f_v - tf)^2))
    return(list(rmse_F = rf))
  } else {
    # vec module
    tm <- result$inputs$target_mean;  td <- count_decimals(tm)
    tsd <- result$inputs$target_sd;   dsd <- count_decimals(tsd)
    s  <- get_stats(result)
    mv <- round(s$mean, td);  sv <- round(s$sd, dsd)
    rm <- sqrt(mean((mv - tm)^2));  rsd <- sqrt(mean((sv - tsd)^2))
    return(list(rmse_mean = rm, rmse_sd = rsd))
  }
}
