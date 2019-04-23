#' Bootstrapped Ruscio's A with 95 percent CIs and standard error
#'
#' This function bootstraps confidence intervals for Ruscio's A effect size (2008). 
#' Code adapted from adapted from John Ruscio's original implementation of his metric: https://ruscio.pages.tcnj.edu/quantitative-methods-program-code/
#' @param data data
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignment of group 1
#' @param value2 assignment of group 2
#' @param Conf.Level 1 - alpha value (e.g., .95).
#' @param seed seed value for reproducability
#' @param B Number of boostrapped resamples
#' @param adjust_ceiling Should Ruscio's A estimates of 0 and 1 be adjusted so that they can be converted to finite odds ratios? This is done by rescoring a single data point as being was inferior to a single second data point between the conditions. Ie., it uses the best granularity allowed by the data, as more data points will result in a more extreme possible values of A.
#' @return ruscios_A_estimate Ruscio's A.
#' @return ruscios_A_se Standard error of bootstrapped Ruscio's A values.
#' @return ruscios_A_ci_lwr Lower 95% bootstrapped confidence interval via the BCA method
#' @return ruscios_A_ci_upr Upper 95% bootstrapped confidence interval via the BCA method
#' @export
#' @examples
#' ruscios_A_boot(data = simulated_data, variable = "Score", group = "Condition", value1 = "B", value2 = "A")
#' 

ruscios_A_boot <- function(data, variable, group, value1 = 1, value2 = 0, 
                           B = 2000, Conf.Level = .95, seed = 1,
                           adjust_ceiling = FALSE) {
  
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
  set.seed(seed)
  nx <- length(x)
  ny <- length(y)
  A.obs <- ruscios_A_function(x, y)
  Alpha <- 1 - Conf.Level
  CI.Lower <- CI.Upper <- pi
  
  # perform bootstrap to generate B values of A
  BS.Values <- rep(0, B)
  for (i in 1:B) {
    BS.Values[i] <- ruscios_A_function(sample(x, replace = T), sample(y, replace = TRUE))
  }
  
  BS.Values <- sort(BS.Values)
  
  # if all bootstrap samples yield same value for A, use it for both ends of CI
  if (min(BS.Values) == max(BS.Values)) {
    CI.Lower <- CI.Upper <- BS.Values[1]
  }
  
  # if sample value not within range of bootstrap values, revert to percentile CI
  if ((A.obs < min(BS.Values)) | (A.obs > max(BS.Values))) {
    CI.Lower <- BS.Values[round((Alpha / 2) * B)]
    CI.Upper <- BS.Values[round((1 - Alpha / 2) * B)]
  }
  
  # otherwise, use BCA CI
  if ((CI.Lower == pi) & (CI.Upper == pi)) {
    # calculate bias-correction and acceleration parameters (z0 and a)
    z0 <- qnorm(mean(BS.Values < A.obs))
    
    jk <- rep(0, (nx + ny))
    for (i in 1:nx) {
      jk[i] <- ruscios_A_function(x[-i], y)
    }
    
    for (i in 1:ny) {
      jk[nx + i] <- ruscios_A_function(x, y[-i])
    }
    
    Diff <- mean(jk) - jk
    a <- sum(Diff ^ 3) / (6 * (sum(Diff ^ 2)) ^ 1.5)
    
    # adjust location of endpoints
    Alpha1 <- pnorm(z0 + (z0 + qnorm(Alpha/2)) / (1 - a * (z0 + qnorm(Alpha/2))))
    Alpha2 <- pnorm(z0 + (z0 - qnorm(Alpha/2)) / (1 - a * (z0 - qnorm(Alpha/2))))
    
    # if either endpoint undefined, replace it with value for percentile CI
    if (is.na(Alpha1)) {Alpha1 <- Alpha / 2}
    if (is.na(Alpha2)) {Alpha2 <- 1 - Alpha / 2}
    
    if (round(Alpha1 * B) < 1) {CI.Lower <- BS.Values[1]}
    else {
      CI.Lower <- BS.Values[round(Alpha1 * B)]
      CI.Upper <- BS.Values[round(Alpha2 * B)]	
    }
  }
  
  # return A, SE of A, lower limit of CI, upper limit of CI
  results <- data.frame(ruscios_A        = round(A.obs,         3),
                        ruscios_A_se     = round(sd(BS.Values), 3),
                        ruscios_A_ci_lwr = round(CI.Lower,      3),
                        ruscios_A_ci_upr = round(CI.Upper,      3))
  
  return(results)
}
