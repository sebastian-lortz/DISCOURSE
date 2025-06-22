#' server
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# info logo in name html wrapper
name_with_info <- function(name, description) {
  HTML(
    '<div title="',
    description,
    '">',
    name,
    '<i class="fas fa-info"></i>',
    '</div>')
}

# define . function
if (getRversion() >= "2.15.1") utils::globalVariables(".")




# lme module
all_inputs_lme <- c(
  "match_descr",
  "corr_table",
  "lme_outcome",
  "lme_predictors",
  "lme_interactions",
  "coef_table_ht",
  "tolerance",
  "max_iter",
  "init_temp",
  "cooling_rate",
  "hill_climbs",
  "max_starts",
  "parallel_start",
  "return_best",
  "weight_table",
  "estimate_weights"
)
disable_inputs_lme <- function() {
  lapply(all_inputs_lme, function(id_) shinyjs::disable(ns(id_)))
}
enable_inputs_lme <- function() {
  lapply(all_inputs_lme, function(id_) shinyjs::enable(ns(id_)))
}

