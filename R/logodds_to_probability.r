#' logodds_to_probability
#'
#' This function converts log odds probability values.
#' @param x The integer(s) to be converted
#' @export
#' @examples
#' logodds_to_probability()

logodds_to_probability <- function(x) {
  exp(x)/(1 + exp(x))
}
