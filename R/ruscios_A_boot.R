#' Bootstrapped Ruscio's A with 95 percent CIs and standard error
#'
#' This function bootstraps confidence intervals for Ruscio's A effect size (2008). 
#' Code adapted from adapted from John Ruscio's original implementation of his metric: https://ruscio.pages.tcnj.edu/quantitative-methods-program-code/
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom RProbSup A
#' @param data data
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignment of group 1 (usually B, after intervention)
#' @param value2 assignment of group 2 (usually A, before intervention)
#' @param Conf.Level 1 - alpha value (e.g., .95).
#' @param seed seed value for reproducability
#' @param B Number of boostrapped resamples
#' @param adjust_ceiling (currently not in use) Should Ruscio's A estimates of 0 and 1 be adjusted so that they can be converted to finite odds ratios? This is done by rescoring a single data point as being was inferior to a single second data point between the conditions. Ie., it uses the best granularity allowed by the data, as more data points will result in a more extreme possible values of A.
#' @return ruscios_A_estimate Ruscio's A.
#' @return ruscios_A_se Standard error of bootstrapped Ruscio's A values.
#' @return ruscios_A_ci_lwr Lower 95% bootstrapped confidence interval via the BCA method
#' @return ruscios_A_ci_upr Upper 95% bootstrapped confidence interval via the BCA method
#' @export
#' @examples
#' \dontrun{
#' ruscios_A_boot(data = simulated_data)
#' }
ruscios_A_boot <- function(data,
                           variable = "Score", 
                           group = "Condition", 
                           value1 = "B", 
                           value2 = "A",
                           B = 1999, 
                           Conf.Level = .95, 
                           seed = 1){ 
                           # adjust_ceiling = TRUE
  
  data_wrangled <- data |>
    dplyr::mutate(Score = {Score},
           Condition = {Condition}) |>
    dplyr::mutate(Condition = dplyr::case_when(Condition == value2 ~ 1,
                                               Condition == value1 ~ 2)) |>
    dplyr::select(Score, Condition) |>
    as.matrix()
  
  fit <- 
    RProbSup::A(
      data = data_wrangled, 
      design = 1, # 1=between
      statistic = 1, # Ruscio's A
      ref = 1, # reference group = 1
      n.bootstrap = B,
      conf.level = Conf.Level,
      seed = seed
    )
  
  res <- tibble(ruscios_A = fit$A,
                ruscios_A_se = fit$SE,
                ruscios_A_ci_lwr = fit$ci.lower,
                ruscios_A_ci_upr = fit$ci.upper)
  
  return(res)
}
