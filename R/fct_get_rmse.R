#' Compute RMSE for a single discourse.object result
#'
#' @param result A discourse.object with optimized data and inputs
#' @return Named list of RMSE values
#' @export
get_rmse <- function(result) {
  # branch: regression models
  if (!is.null(result$inputs$target_reg)) {
    # targets and precision
    tc <- result$inputs$target_cor;  tr <- result$inputs$target_reg;  ts <- result$inputs$target_se
    dc <- max(count_decimals(tc));  dr <- max(count_decimals(tr))
    ds <- if (!is.null(ts)) max(count_decimals(ts)) else NULL
    rmse <- function(x, y) sqrt(mean((x - y)^2))

    # compute and round
    st <- get_stats(result)
    cor_v <- round(st$cor[!is.na(tc)], dc);  reg_v <- round(st$reg[!is.na(tr)], dr)
    se_v  <- if (!is.null(ts)) round(st$se[!is.na(ts)], ds) else NULL

    # RMSE outputs
    rc <- rmse(cor_v, tc[!is.na(tc)])
    rr <- rmse(reg_v, tr[!is.na(tr)])
    rs <- if (!is.null(se_v)) rmse(se_v, ts[!is.na(ts)]) else NA
    return(list(rmse_cor = rc, rmse_reg = rr, rmse_se = rs))

    # branch: ANOVA-based
  } else if (!is.null(result$inputs$target_f_list)) {
    # target and precision
    tf <- result$inputs$target_f_list$F
    df <- count_decimals(tf)

    # compute F and RMSE
    f_v <- round(get_stats(result)$F_value, df)
    rf  <- sqrt(mean((f_v - tf)^2))
    return(list(rmse_F = rf))

    # branch: mean/sd vectors
  } else {
    # targets and precision
    tm <- result$inputs$target_mean;  td <- count_decimals(tm)
    tsd <- result$inputs$target_sd;   dsd <- count_decimals(tsd)

    # compute stats
    s  <- get_stats(result)
    mv <- round(s$mean, td);  sv <- round(s$sd, dsd)

    # RMSE outputs
    rm <- sqrt(mean((mv - tm)^2));  rsd <- sqrt(mean((sv - tsd)^2))
    return(list(rmse_mean = rm, rmse_sd = rsd))
  }
}

# Example:
# res <- get_rmse(obj)
# str(res)
