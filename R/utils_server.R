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

