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

  # Compute the nearest possible sum (the total number of "points")
  total_points <- round(target_mean * n)

  # Calculate the mean that would result from that total
  possible_mean <- total_points / n

  # Determine the absolute difference between the target mean and the possible mean
  diff <- abs(target_mean - possible_mean)

  # Define the allowed margin (half the smallest increment plus a tolerance for rounding errors)
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
