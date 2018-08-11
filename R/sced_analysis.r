#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @return p: Hypothesis test p value via permutation test. Calculated via Monte-Carlo simulation (10000 runs) rather than brute force.
#' @return median_difference: Difference between the median value of A and the median value of B. Because Ruscio's A is non-parametric it can notionally suffer from ceiling effects, e.g., where A = 1.00 but the real difference between the groups are larger or smaller. It can therefore be useful to report the unstandardized effect size as well as the standardized effect sizes. Median difference is used here over mean difference given its greater robustness.
#' @return ruscios_A: Effect size A (Ruscio, 2008). The probability that a randomly selected timepoint in condition B is larger than a randomly selected timepoint in condition A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/the probability of superiority but with no parametric assumptions. A Cohen's d of 1.5 corrisponds to an A of 0.85.
#' @return hedges_g: Effect size Hedge's g effect size via bootstrapping, a version of Cohen's d that is bias corrected for small sample sizes. Identical range, interpretation and cutoffs as Cohen's d. Included here for familiarity: it's parametric assumtions (equal variances) and sensitivity to equal number of timepoints in A and B make it somewhat unrobust in many SCED contexts. In order to relax the assumption of normality a bootstrapped implemenation is employed.
#' @export
#' @examples
#' sced_results <- sced_analysis(data = simulated_data)

sced_analysis <- function(data, n_boots = 10000) {
  require(tidyverse)
  require(coin)
  require(effsize)
  require(bootES)
  data(simulated_data)
  
  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(B = n_boots),
                                    data = .))) %>%
    ungroup() %>%
    dplyr::mutate(p = as.numeric(p),
                  p = ifelse(p < .00001, "< .00001", round(p, 5)))
  
  median_change <- data %>%
    group_by(Participant) %>%
    dplyr::summarize(median_a = median(Score[Condition == "A"]),
                     median_b = median(Score[Condition == "B"]),
                     median_difference = median_b - median_a,
                     na.rm = TRUE) %>%
    dplyr::select(Participant, median_difference)
  
  # # consider adding bootstrapped unstandardised median difference scores
  # median_diff <- function(data, runs = n_boots) {
  #   data %>%
  #     group_by(Participant) %>%
  #     broom::bootstrap(B) %>%
  #     do(broom::tidy(summarize(data = .,
  #                              median_a = median(Score[Condition == "A"]),
  #                              median_b = median(Score[Condition == "B"]),
  #                              median_difference = median_b - median_a) %>%
  #                      dplyr::select(Participant, median_difference))) %>%
  #     ungroup()
  # }
  # median_diff(data = simulated_data, runs = 1000)
  
  # bootstrapped Ruscio's nonparametric effect size A
  ruscios_A_by_participant <- data %>%
    group_by(Participant) %>%
    do(ruscios_A = ruscios_A(variable = "Score",
                             group = "Condition",
                             data = .,
                             value1 = "A",
                             value2 = "B")) %>%
    ungroup() %>%
    mutate(ruscios_A = as.numeric(ruscios_A))
  
  ruscios_A_boot_by_participant <- data %>%
    group_by(Participant) %>%
    do(ruscios_A = ruscios_A_boot(variable = "Score",
                                  group = "Condition",
                                  data = .,
                                  value1 = "A",
                                  value2 = "B",
                                  runs = n_boots)) %>%
    ungroup() %>%
    # I should do list-flattening here, but the below hacky solution works
    dplyr::mutate(ruscios_A = str_replace(ruscios_A, "list\\(", ""),
                  ruscios_A = str_replace(ruscios_A, "ruscios_A_ci_lwr = ", ""),
                  ruscios_A = str_replace(ruscios_A, "ruscios_A_ci_upr = ", ""),
                  ruscios_A = str_replace(ruscios_A, "\\)", "")) %>%
    tidyr::separate(ruscios_A, into = c("ruscios_A_ci_lwr", "ruscios_A_ci_upr"), sep = ",") %>%
    dplyr::mutate(temp1  = round(as.numeric(ruscios_A_ci_lwr), 3),  # the upr and lwr CIs are then swapped so that the ES can be inverted
                  temp2  = round(as.numeric(ruscios_A_ci_upr), 3)) %>%
    dplyr::select(-ruscios_A_ci_lwr, -ruscios_A_ci_upr) %>%
    dplyr::rename(ruscios_A_ci_lwr = temp2,
                  ruscios_A_ci_upr = temp1) %>%
    # dplyr::mutate(ruscios_A_ci_lwr = ifelse(is.na(ruscios_A_ci_lwr), 1, ruscios_A_ci_lwr),
    #               ruscios_A_ci_upr = ifelse(is.na(ruscios_A_ci_upr), 1, ruscios_A_ci_upr)) %>%
    dplyr::select(Participant, ruscios_A_ci_lwr, ruscios_A_ci_upr)
  
  # bootstrapped Hedges' g effect size (removes assumption of normality but not equality of variances or equal N per condition)
  hedges_g_by_participant <- data %>%
    group_by(Participant) %>%
    do(hedges_g = bootES(.,
                         R = n_boots,
                         data.col = "Score",
                         group.col = "Condition",
                         contrast = c(A = 1, B = -1),
                         effect.type = "hedges.g",
                         ci.type = "bca",
                         ci.conf = 0.95)$t0) %>%
    ungroup() %>%
    dplyr::mutate(hedges_g = round(as.numeric(hedges_g), 2)*-1)  # needs to be reverse scored
  
  hedges_g_ci_lwr_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(hedges_g_ci_lwr = bootES(.,
                                R = n_boots,
                                data.col = "Score",
                                group.col = "Condition",
                                contrast = c(A = 1, B = -1),
                                effect.type = "hedges.g",
                                ci.type = "bca",
                                ci.conf = 0.95)$bounds[2]) %>%
    ungroup() %>%
    dplyr::mutate(hedges_g_ci_lwr = round(as.numeric(hedges_g_ci_lwr), 2)*-1)  # needs to be reverse scored
  
  hedges_g_ci_upr_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(hedges_g_ci_upr = bootES(.,
                                R = n_boots,
                                data.col = "Score",
                                group.col = "Condition",
                                contrast = c(A = 1, B = -1),
                                effect.type = "hedges.g",
                                ci.type = "bca",
                                ci.conf = 0.95)$bounds[1]) %>%
    ungroup() %>%
    dplyr::mutate(hedges_g_ci_upr = round(as.numeric(hedges_g_ci_upr), 2)*-1)  # needs to be reverse scored
  
  # combine results
  results <- p_by_participant %>%
    left_join(median_change, by = "Participant") %>%
    left_join(ruscios_A_by_participant, by = "Participant") %>%
    left_join(ruscios_A_boot_by_participant, by = "Participant") %>%
    left_join(hedges_g_by_participant, by = "Participant") %>%
    left_join(hedges_g_ci_lwr_by_participant, by = "Participant") %>%
    left_join(hedges_g_ci_upr_by_participant, by = "Participant")
  
  return(results)
}
