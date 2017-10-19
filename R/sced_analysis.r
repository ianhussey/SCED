#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data
#' @keywords SCED
#' @export
#' @examples
#' sced_analysis(simulated_data)

sced_analysis <- function(data) {
  require(tidyverse)
  require(coin)
  require(effsize)

  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    group_by(participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(B = 10000),
                                    data = .))) %>%
    ungroup() %>%
    mutate(p = as.numeric(p),
           p = ifelse(p < .00001, "< .00001", round(p, 5)))

  # nonparametric effect size "A"
  esA_by_participant <- data %>%
    group_by(participant) %>%
    do(esA = esA(variable = "Score",
                 group = "Condition",
                 data = .,
                 value1 = "A",
                 value2 = "B",
                 runs = 1000)) %>%
    ungroup() %>%
    mutate(esA = 1 - round(as.numeric(esA), 3))  # needs to be reverse scored

  # Hedges' g effect size (parametric, but familiar)
  g_by_participant <- data %>%
    group_by(participant) %>%
    do(g = cohen.d(Score ~ Condition,
                   data = .,
                   pooled = TRUE,
                   hedges.correction = TRUE)$estimate) %>%
    ungroup() %>%
    mutate(g = round(as.numeric(g), 2)*-1)  # needs to be reverse scored

  # combine results
  results <- p_by_participant %>%
    left_join(esA_by_participant, by = "participant") %>%
    left_join(g_by_participant, by = "participant")

  return(results)
}
