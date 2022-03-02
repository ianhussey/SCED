#' Format p values better for printing and reporting
#' 
#' Tries to do a better job that base R's format.pval
#'
#' @param value The p value to be formatted
#' @export
#' @examples
#' format_pval_better(0.00000001)
#' format_pval_better(0.01)
#' format_pval_better(0.7)

format_pval_better <- function(p){
  p_formatted <- case_when(p > 0.9999 ~ "> 0.9999",
                           p >= 0.0001 ~ paste("=", round(p, 4)),
                           p < 0.0001 ~ "< .0001")
  p_formatted <- gsub(pattern = "0.", replacement = ".", x = p_formatted, fixed = TRUE)
  
  # add a trailing zero so that it is always at least 2 decimal places
  p_formatted <- ifelse(nchar(p_formatted) == 4, paste0(p_formatted, "0"), p_formatted)
  
  return(p_formatted)
}
