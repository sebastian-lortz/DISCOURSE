#' @import future
#' @importFrom future multisession
#' @importFrom future plan
.onLoad <- function(libname, pkgname) {
  future::plan(
    future::multisession,
    workers = max(1, parallel::detectCores() - 1)
    )
}
