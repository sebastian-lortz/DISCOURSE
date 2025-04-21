#' Extract statistics from a single discourse.object
#'
#' @param result A discourse.object with data and inputs
#' @return List containing model fit and summary stats or ANOVA/F stats or vectors
#' @export
get_stats <- function(result) {
  # prepare data frame
  data_df <- as.data.frame(result$data)

  # regression-based branch
  if (!is.null(result$inputs$target_reg)) {
    eq <- result$inputs$reg_equation
    if (!is.null(result$inputs$prob_within_move)) {
      # mixed-effects model
      model <- lme4::lmer(eq, data = data_df,
                          control = lme4::lmerControl(check.conv.singular = "ignore"))
      fe      <- lme4::fixef(model)
      re_sd   <- as.data.frame(lme4::VarCorr(model))$sdcor[2]
      names(re_sd) <- "std.pnumber"
      reg     <- c(fe, re_sd)
      se      <- summary(model)$coef[, "Std. Error"]
      wide    <- long_to_wide(data_df)
      means   <- colMeans(wide[, -1])
      sds     <- apply(wide[, -1], 2, stats::sd)
    } else {
      # simple linear model
      model <- stats::lm(eq, data = data_df)
      reg   <- stats::coef(model)
      se    <- summary(model)$coef[, 2]
      means <- colMeans(data_df)
      sds   <- apply(data_df, 2, stats::sd)
    }
    # correlations
    cor_mat  <- stats::cor(data_df[, !names(data_df) %in% c("pnumber", "time")])
    cor_vals <- cor_mat[upper.tri(cor_mat)]

    return(list(
      model = model,
      reg   = reg,
      se    = se,
      cor   = cor_vals,
      mean  = means,
      sd    = sds
    ))

    # ANOVA-based branch
  } else if (!is.null(result$inputs$target_f_vec)) {
    if (!requireNamespace("afex", quietly = TRUE)) {
      stop("Package 'afex' required for ANOVA tests.")
    }
    an_tab <- afex::aov_car(
      formula  = result$inputs$formula,
      data     = data_df,
      factorize= TRUE,
      type     = result$inputs$typeSS
    )$anova_table
    rn    <- trimws(rownames(an_tab))
    eff   <- result$inputs$target_f_vec$effect
    F_val <- sapply(eff, function(e) an_tab[rn == e, "F"])

    return(list(
      model   = an_tab,
      F_value = as.vector(F_val),
      mean    = result$inputs$target_group_means
    ))

    # vector-based branch
  } else {
    m   <- apply(result$data, 2, mean)
    sdv <- apply(result$data, 2, stats::sd)
    return(list(
      mean = m,
      sd   = sdv
    ))
  }
}

# Example:
# res <- get_stats(obj)
# str(res)
