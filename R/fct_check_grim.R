#' Check plausibility of a reported mean with the GRIM test (Brown & Heathers 2007)
#'
#' Performs the GRIM (Granularity-Related Inconsistency of Means) test to assess
#' whether a reported mean is numerically possible given the sample size.
#'
#' @param n Integer. Sample size; a positive whole number.
#' @param target_mean Numeric. Reported mean to be tested for plausibility.
#' @param decimals Integer. Number of decimal places in the reported mean.
#' @param tol.r Numeric. Tolerance for rounding errors; a non-negative value. Default is the square root of machine double precision epsilon.
#' @return A list with components:
#' \describe{
#'   \item{test}{Logical. TRUE if the reported mean is plausible.}
#'   \item{grim_mean}{Numeric. The adjusted mean that is numerically plausible (rounded to `decimals`).}
#' }
#' @examples
#' \dontrun{
#' check_grim(10, 3.7, 1)
#' }
#' @export
check_grim <- function(n, target_mean, decimals, tol.r = .Machine$double.eps^0.5) {
  # input check
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }
  if (!is.numeric(target_mean) || length(target_mean) != 1) {
    stop("`target_mean` must be a single numeric value.")
  }
  if (!is.numeric(decimals) || length(decimals) != 1 || decimals < 0 || decimals != as.integer(decimals)) {
    stop("`decimals` must be a single non-negative integer.")
  }
  if (!is.numeric(tol.r) || length(tol.r) != 1 || tol.r < 0) {
    stop("`tol.r` must be a single non-negative numeric value.")
  }

  total_points <- round(target_mean * n)
  possible_mean <- total_points / n
  diff <- abs(target_mean - possible_mean)
  allowed_margin <- (0.1 ^ decimals) / 2 + tol.r

  if (diff > allowed_margin) {
    adjusted_mean <- round(possible_mean, decimals)
    cat("\nMean", target_mean, "fails GRIM test. The adjusted mean", adjusted_mean, "is plausible.\n")
    return(list(test = FALSE, grim_mean = adjusted_mean))
  } else {
    cat("\nMean", target_mean, "passed GRIM test. The mean is plausible.\n")
    return(list(test = TRUE, grim_mean = target_mean))
  }
}
