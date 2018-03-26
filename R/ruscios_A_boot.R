#' Bootstrapped Ruscio's A
#'
#' This function bootstraps confidence intervals for Ruscio's A effect size (2008).
#' @param data data
#' @param variable continuous variable
#' @param group dichotomous group
#' @param value1 assignement of group 1
#' @param value2 assignement of group 2
#' @param B Number of boostrapped resamples
#' @return ruscios_A_median Median boostrapped estimation of Ruscio's A.
#' @return ruscios_A_ci_lwr Lower 95% boostrapped confidence interval
#' @return ruscios_A_ci_upr Upper 95% boostrapped confidence interval
#' @export
#' @examples
#' ruscios_A_boot(variable = "Score", group = "Condition", value1 = "B", value2 = "A", runs = 1000, data = simulated_data)

ruscios_A_boot <- function(data, variable, group, value1, value2, runs = 1000) {
  require(tidyverse)
  require(broom)

  ruscios_A_boot <- data %>%
    broom::bootstrap(runs) %>%
    do(broom::tidy(SCED::ruscios_A(variable = variable,
                                   group = group,
                                   value1 = value1,
                                   value2 = value2,
                                   data = .))) %>%
    ungroup() %>%
    dplyr::summarize(ruscios_A_median = round(median(x, na.rm = TRUE), 3),
                     ruscios_A_ci_lwr = round(quantile(x, 0.025, na.rm = TRUE), 3),
                     ruscios_A_ci_upr = round(quantile(x, 0.975, na.rm = TRUE), 3))

  return(ruscios_A_boot)
}
