#' Ruscio's A
#'
#' This function calculates a permutation test version of Ruscio's A effect size (2008).
#' Unlike McGraw & Wong's original method, which they called the Common Language Effect Size and which used an algebraic/normal approximation, this version makes no assumptions about the distribution of the data.
#' Each X observation is paired to each Y observation. The proportion of XY pairs where X is higher than Y is then computed and output. Ties count as 0.5*TRUE.
#' Code is adapted from https://janhove.github.io/reporting/2016/11/16/common-language-effect-sizes
#' @param runs max number of permutations
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignement of group 1
#' @param value2 assignement of group 2
#' @param data data
#' @return esA: Effect size A (Ruscio, 2008). The probability that a randomly selected timepoint in condition B is larger than a randomly selected timepoint in condition A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/the probability of superiority but with no parametric assumptions. A Cohen's d of 1.5 corrisponds to an Ruscio's A of 0.85.
#' @export
#' @examples
#' A("Score", "Condition", data = data)

ruscios_A <- function(variable, group, data, value1 = 1, value2 = 0, runs = 10000) {

  # Ensure data is a data frame (e.g., not a tbl_data)
  data <- as.data.frame(data)

  # Select the observations for group 1
  x <- data[data[[group]] == value1, variable]

  # Select the observations for group 2
  y <- data[data[[group]] == value2, variable]

  # Matrix with difference between XY for all pairs (Guillaume Rousselet's suggestion)
  m <- outer(x,y,FUN="-")

  # Convert to booleans; count ties as half true.
  m <- ifelse(m==0, 0.5, m>0)

  # Return proportion of TRUEs
  ruscios_A <- round(mean(m), 3)

  return(as.numeric(ruscios_A))
}
