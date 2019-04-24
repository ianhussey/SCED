#' round_df
#'
#' This function rounds all numeric variables in a dataframe to the desired number of places. Non-numeric variables will be ignored.
#' @param df Dataframe to be rounded.
#' @param digits Number of decimal places to round to.
#' @export
#' @examples
#' round_df(my_data, digits = 2)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}
