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

  A <- function(variable, group, data, value1 = 1, value2 = 0, runs = 10000) {
    # Ensure data is a data frame (e.g., not a tbl_data)
    data <- as.data.frame(data)
    # Select the observations for group 1
    x <- data[data[[group]] == value1, variable]
    # Select the observations for group 2
    y <- data[data[[group]] == value2, variable]
    # Matrix with difference between XY for all pairs (Guillaume Rousselet's suggestion)
    m <- outer(x, y, FUN = "-")
    # Convert to booleans; count ties as half true.
    m <- ifelse(m==0, 0.5, m>0)
    # Return proportion of TRUEs
    A <- mean(m)
    return(as.numeric(A))
  }

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
  A_by_participant <- data %>%
    group_by(participant) %>%
    do(A = A(variable = "Score",
                   group = "Condition",
                   data = .,
                   value1 = "A",
                   value2 = "B",
                   runs = 1000)) %>%
    ungroup() %>%
    mutate(A = 1 - round(as.numeric(A), 3))  # needs to be reverse scored

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
    left_join(A_by_participant, by = "participant") %>%
    left_join(g_by_participant, by = "participant")

  return(results)
}
