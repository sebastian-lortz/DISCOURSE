#' @import future promises parallel
NULL

library(future)
library(promises)
plan(multisession, workers = parallel::detectCores() - 1)
