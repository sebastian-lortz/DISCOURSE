#' Extract statistics from a single discourse.object
#'
#' Computes and returns key analytical outputs from one `discourse.object`, including model fits,
#' parameter estimates, and summary statistics appropriate for regression, ANOVA, or vector-based analyses.
#'
#' @param result A `discourse.object` produced by analysis functions (e.g., `optim_vec`, `optim_aov()`, `optim_lm()`, `optim()`).
#'
#' @return A named `list` containing elements dependent on the input type:
#' \describe{
#'   \item{model}{Model object or ANOVA table used for estimating parameters.}
#'   \item{reg}{Numeric vector of regression coefficients (fixed effects) and random intercept (last element, for LME only) for regression-based objects.}
#'   \item{se}{Numeric vector of standard errors corresponding to `reg` (excluding the random intercept for LME).}
#'   \item{cor}{Numeric vector of bivariate correlations for regression-based objects.}
#'   \item{mean}{Numeric vector of means: for regression-based, variable means (columns in wide format); for vector-based, variable means; for ANOVA, (sub)group means.}
#'   \item{sd}{Numeric vector of standard deviations for regression- and vector-based objects.}
#'   \item{F_value}{Numeric vector of F-statistics for ANOVA-based objects.}
#' }
#'
#' @examples
#'  \dontrun{
#' get_stats(discourse.object)
#' }
#' @export
get_stats <- function(result) {
  if (!inherits(result, "discourse.object")) {
    stop("Input must be a discourse.object.")
  }
  data_df <- as.data.frame(result$data)

  # LM and LME module
  if (!is.null(result$inputs$target_reg)) {
    eq <- result$inputs$reg_equation
    if (!is.null(result$inputs$move_prob)) {
      # LME
      model <- lme4::lmer(eq, data = data_df,
                          control = lme4::lmerControl(check.conv.singular = "ignore"))
      fe      <- lme4::fixef(model)
      vc <- as.data.frame(lme4::VarCorr(model))
      re_sd <- vc[vc$grp == "ID" & vc$var1 == "(Intercept)", "sdcor"]
      names(re_sd) <- "std.ID"
      reg     <- c(fe, re_sd)
      se      <- summary(model)$coef[, "Std. Error"]
      wide    <- long_to_wide(data_df)
      means   <- colMeans(wide[, -1])
      sds     <- apply(wide[, -1], 2, stats::sd)
    } else {
      # LM
      model <- stats::lm(eq, data = data_df)
      reg   <- stats::coef(model)
      se    <- summary(model)$coef[, 2]
      means <- colMeans(data_df)
      sds   <- apply(data_df, 2, stats::sd)
    }
    cor_mat  <- stats::cor(data_df[, !names(data_df) %in% c("ID", "time")])
    cor_vals <- cor_mat[upper.tri(cor_mat)]
    return(list(
      model = model,
      reg   = reg,
      se    = se,
      cor   = cor_vals,
      mean  = means,
      sd    = sds
    ))
  } else if (!is.null(result$inputs$target_f_list)) {
    # aov Module
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
    eff   <- result$inputs$target_f_list$effect
    F_val <- sapply(eff, function(e) an_tab[rn == e, "F"])
    return(list(
      model   = an_tab,
      F_value = as.vector(F_val),
      mean    = result$inputs$target_group_means
    ))
  } else {
    # vec module
    m   <- apply(result$data, 2, mean)
    sdv <- apply(result$data, 2, stats::sd)
    return(list(
      mean = m,
      sd   = sdv
    ))
  }
}
