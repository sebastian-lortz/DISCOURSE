#' Aggregate stats across discourse.object runs
#'
#' @param object_list List of discourse.object
#' @return List of data.frames with mean, median, sd, min, max per component
#' @export
get_stats_parallel <- function(object_list) {
  # validate input
  if (!is.list(object_list)) stop("`object_list` must be a list of discourse.object")
  if (!all(vapply(object_list, function(x) inherits(x, "discourse.object"), logical(1L)))) {
    stop("All elements of `object_list` must be discourse.object")
  }

  # compute per-run stats
  stats_list <- lapply(object_list, get_stats)

  # numeric components
  comps <- names(stats_list[[1]])[vapply(stats_list[[1]], is.numeric, logical(1L))]

  # aggregate metrics
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

# Example:
# res <- get_stats_parallel(list(obj1, obj2))
# str(res)
