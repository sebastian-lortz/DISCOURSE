#' Perform GRIM test on a mean
#'
#' @param n Integer sample size
#' @param target_mean Numeric target mean to test
#' @param decimals Integer number of decimal places
#' @param tol.r Numeric tolerance for rounding errors
#' @return A list with components:
#'   - test: Logical, TRUE if plausible
#'   - grim_mean: Numeric, plausible mean
#' @export
check_grim <- function(n, target_mean, decimals, tol.r = .Machine$double.eps^0.5) {
  total_points   <- round(target_mean * n)
  possible_mean  <- total_points / n
  diff           <- abs(target_mean - possible_mean)
  allowed_margin <- (0.1^decimals) / 2 + tol.r

  if (diff > allowed_margin) {
    adjusted_mean <- round(possible_mean, decimals)
    cat("\nMean", target_mean, "fails GRIM test. The adjusted mean", adjusted_mean, "is plausible.\n")
    return(list(test = FALSE, grim_mean = adjusted_mean))
  }

  cat("\nMean", target_mean, "passed GRIM test. The mean is plausible.\n")
  list(test = TRUE, grim_mean = target_mean)
}

# Example usage:
# res <- check_grim(10, 2.35, 2)
# print(res)
