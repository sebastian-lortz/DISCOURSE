#' Aggregate statistics across discourse.object runs
#'
#' Computes summary metrics (mean, median, standard deviation, minimum, and maximum)
#' for each numeric output component across multiple `discourse.object` results.
#'
#' @param object_list A `list` of `discourse.object` instances. Outputs from functions (e.g., `optim_vec`, `optim_aov()`, `optim_lm()`, `optim()`).
#'
#' @return A `list` of `data.frame` objects. Each data.frame corresponds to one numeric component (e.g., `reg`, `se`, `cor`, `mean`, `sd`), and contains columns:
#' \describe{
#'   \item{mean}{Numeric. Mean of the component across runs.}
#'   \item{med}{Numeric. Median of the component across runs.}
#'   \item{sd}{Numeric. Standard deviation across runs.}
#'   \item{min}{Numeric. Minimum value observed across runs.}
#'   \item{max}{Numeric. Maximum value observed across runs.}
#' }
#'
#' @examples
#'  \dontrun{
#' # Single-threaded example
#' result <- parallel_lm(args = ..., ...)
#' get_stats_parallel(result)
#' }
#' @export
get_stats_parallel <- function(object_list) {
  # validate input
  if (!is.list(object_list)) stop("`object_list` must be a list of discourse.object")
  if (!all(vapply(object_list, function(x) inherits(x, "discourse.object"), logical(1L)))) {
    stop("All elements of `object_list` must be discourse.object")
  }
  stats_list <- lapply(object_list, get_stats)
  comps <- names(stats_list[[1]])[vapply(stats_list[[1]], is.numeric, logical(1L))]
  out <- stats::setNames(lapply(comps, function(comp) {
    mat <- do.call(cbind, lapply(stats_list, `[[`, comp))
    data.frame(
      mean = rowMeans(mat, na.rm=TRUE),
      med  = apply(mat, 1, stats::median, na.rm=TRUE),
      sd   = apply(mat, 1, stats::sd, na.rm=TRUE),
      min  = apply(mat, 1, min, na.rm=TRUE),
      max  = apply(mat, 1, max, na.rm=TRUE)
    )
  }), comps)
  out
}
