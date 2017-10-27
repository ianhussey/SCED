#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @return p: Hypothesis test p value via permutation test. Calculated via Monte-Carlo simulation (10000 runs) rather than brute force.
#' @return ruscios_A: Effect size A (Ruscio, 2008). The probability that a randomly selected timepoint in condition B is larger than a randomly selected timepoint in condition A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/the probability of superiority but with no parametric assumptions. A Cohen's d of 1.5 corrisponds to an A of 0.85.
#' @return hedges_g: Effect size Hedge's g effect size via bootstrapping, a version of Cohen's d that is bias corrected for small sample sizes. Identical range, interpretation and cutoffs as Cohen's d. Included here for familiarity: it's parametric assumtions (equal variances) and sensitivity to equal number of timepoints in A and B make it somewhat unrobust in many SCED contexts. In order to relax the assumption of normality a bootstrapped implemenation is employed.
#' @export
#' @examples
#' sced_analysis(data = simulated_data)

sced_analysis <- function(data) {
  require(tidyverse)
  require(coin)
  require(effsize)
  require(bootES)
  data(simulated_data)

  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    group_by(Participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(B = 100000),
                                    data = .))) %>%
    ungroup() %>%
    mutate(p = as.numeric(p),
           p = ifelse(p < .00001, "< .00001", round(p, 5)))

  # nonparametric effect size "A"
  ruscios_A_by_participant <- data %>%
    group_by(Participant) %>%
    do(ruscios_A = ruscios_A(variable = "Score",
                             group = "Condition",
                             data = .,
                             value1 = "A",
                             value2 = "B",
                             runs = 10000)) %>%
    ungroup() %>%
    mutate(ruscios_A = 1 - as.numeric(ruscios_A))  # needs to be reverse scored

  # bootstrapped Hedges' g effect size (removes assumption of normality but not equality of variances or equal N per condition)
  hedges_g_by_participant <- data %>%
    group_by(Participant) %>%
    do(hedges_g = bootES(.,
                         R = 100,
                         data.col = "Score",
                         group.col = "Condition",
                         contrast = c(A = 1, B = -1),
                         effect.type = "hedges.g",
                         ci.type = "bca",
                         ci.conf = 0.95)$t0) %>%
    ungroup() %>%
    mutate(hedges_g = round(as.numeric(hedges_g), 2)*-1)  # needs to be reverse scored

  hedges_g_ci_lwr_by_participant <- data %>%
    group_by(Participant) %>%
    do(hedges_g_ci_lwr = bootES(.,
                                R = 100,
                                data.col = "Score",
                                group.col = "Condition",
                                contrast = c(A = 1, B = -1),
                                effect.type = "hedges.g",
                                ci.type = "bca",
                                ci.conf = 0.95)$bounds[2]) %>%
    ungroup() %>%
    mutate(hedges_g_ci_lwr = round(as.numeric(hedges_g_ci_lwr), 2)*-1)  # needs to be reverse scored

  hedges_g_ci_upr_by_participant <- data %>%
    group_by(Participant) %>%
    do(hedges_g_ci_upr = bootES(.,
                                R = 100,
                                data.col = "Score",
                                group.col = "Condition",
                                contrast = c(A = 1, B = -1),
                                effect.type = "hedges.g",
                                ci.type = "bca",
                                ci.conf = 0.95)$bounds[1]) %>%
    ungroup() %>%
    mutate(hedges_g_ci_upr = round(as.numeric(hedges_g_ci_upr), 2)*-1)  # needs to be reverse scored

  # combine results
  results <- p_by_participant %>%
    left_join(ruscios_A_by_participant, by = "Participant") %>%
    left_join(hedges_g_by_participant, by = "Participant") %>%
    left_join(hedges_g_ci_lwr_by_participant, by = "Participant") %>%
    left_join(hedges_g_ci_upr_by_participant, by = "Participant")

  return(results)
}
