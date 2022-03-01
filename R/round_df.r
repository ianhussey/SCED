#' round_df
#'
#' This function rounds all numeric variables in a dataframe to the desired number of places. Non-numeric variables will be ignored.
#' @param df Dataframe to be rounded.
#' @param digits Number of decimal places to round to.
#' @export
#' @examples
#' round_df(my_data, digits = 2)

round_df <- function(df, digits = 3){
  df %>% mutate_if(is.numeric, round, digits = digits)
}
