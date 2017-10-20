#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @return p: Hypothesis test p value via permutation test. Calculated via Monte-Carlo simulation (10000 runs) rather than brute force.
#' @return A: Effect size A (Ruscio, 2008). The probability that a randomly selected timepoint in condition B is larger than a randomly selected timepoint in condition A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/the probability of superiority but with no parametric assumptions. A Cohen's d of 1.5 corrisponds to an A of 0.85.
#' @return g: Effect size Hedge's g effect size, a version of Cohen's d that is bias corrected for small sample sizes. Identical range, interpretation and cutoffs as Cohen's d. Included here for familiarity: it's parametric assumtions (normal distribution, equal variances) and sensitivity to equal number of timepoints in A and B make it somewhat unrobust in many SCED contexts.
#' @export
#' @examples
#' sced_analysis(data = simulated_data)

sced_analysis <- function(data) {
  require(tidyverse)
  require(coin)
  require(effsize)
  data(simulated_data)

  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    group_by(Participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(B = 10000),
                                    data = .))) %>%
    ungroup() %>%
    mutate(p = as.numeric(p),
           p = ifelse(p < .00001, "< .00001", round(p, 5)))

  # nonparametric effect size "A"
  A_by_participant <- data %>%
    group_by(Participant) %>%
    do(A = esA(variable = "Score",
               group = "Condition",
               data = .,
               value1 = "A",
               value2 = "B",
               runs = 1000)) %>%
    ungroup() %>%
    mutate(A = 1 - round(as.numeric(A), 3))  # needs to be reverse scored

  # Hedges' g effect size (parametric, but familiar)
  g_by_participant <- data %>%
    group_by(Participant) %>%
    do(g = cohen.d(Score ~ Condition,
                   data = .,
                   pooled = TRUE,
                   hedges.correction = TRUE)$estimate) %>%
    ungroup() %>%
    mutate(g = round(as.numeric(g), 2)*-1)  # needs to be reverse scored

  # combine results
  results <- p_by_participant %>%
    left_join(A_by_participant, by = "Participant") %>%
    left_join(g_by_participant, by = "Participant")

  return(results)
}
