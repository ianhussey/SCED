#' Ruscio's A - the probability of superiority
#'
#' This function bootstraps confidence intervals for Ruscio's A effect size (2008). 
#' Code adapted from adapted from John Ruscio's original implementation of his metric: https://ruscio.pages.tcnj.edu/quantitative-methods-program-code/
#' @param data data
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignment of group 1
#' @param value2 assignment of group 2
#' @return Ruscio's A.
#' @export
#' @examples
#' ruscios_A(data = simulated_data, variable = "Score", group = "Condition", value1 = "B", value2 = "A")
#' 

ruscios_A <- function(data, variable, group, value1 = 1, value2 = 0, adjust_ceiling = FALSE) {
  
  # Fast calculation of the A statistic
  ruscios_A_function <- function(x, y) {
    nx <- length(x)
    ny <- length(y)
    rx <- sum(rank(c(x, y))[1:nx])
    A = (rx / nx - (nx + 1) / 2) / ny
    
    # if adjust_ceiling == TRUE & A == 0 or 1, rescore it as if a single data point was inferior to a single second data point between conditions. 
    # Ie., use the lowest granularity allowed by the data for rescoring. More data points will result in a higher adjusted A.
    if(adjust_ceiling == TRUE & A == 1){
      A <- ruscios_A_function(c(rep(4, length(x)), 2), c(rep(1, length(y)), 3))
    } else if(adjust_ceiling == TRUE & A == 0){
      A <- 1 - ruscios_A_function(c(rep(4, length(x)), 2), c(rep(1, length(y)), 3))
    }
    
    return(A)
  }
  
  # Ensure data is a data frame (e.g., not a tbl_data)
  data <- as.data.frame(data)
  
  # Select the observations for group 1
  x <- data[data[[group]] == value1, variable]
  
  # Select the observations for group 2
  y <- data[data[[group]] == value2, variable]
  
  
  # initialize variables
  nx <- length(x)
  ny <- length(y)
  A.obs <- ruscios_A_function(x, y)
  
  return(A.obs)
}
