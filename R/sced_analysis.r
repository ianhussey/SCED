#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @param n_boots: number of bootstrapped resamples for Hedges' g and Ruscio's A. N for p value permutation is n_boots*10. 
#' @param invert_effect_sizes: Effect sizes are reported assuming that scores in timepoint B are expected to be higher than timepoint A (i.e., that the intervention causes scores to increase). If invert_effect_sizes == TRUE then effect sizes are inverted, e.g., if the intervention is expected to causes scores to decrease.
#' @param adjust_probability_ceiling: Should Ruscio's A estimates of 0 and 1 be adjusted so that they can be converted to finite odds ratios? This is done by rescoring a single data point as being was inferior to a single second data point between the conditions. Ie., it uses the best granularity allowed by the data, as more data points will result in a more extreme possible values of A.  
#' @return Baseline trend: standardized beta OLS regression coefficient for the slope between the timepoint A data points. Treats the timepoints as equally spaced integers (e.g., rather than modelling them as dates). Can be used to exclude participants from consideration in meta analysis, e.g., on the basis that improvements at followup are due to improvement trends at baseline.
#' @return Intervention trend: standardized beta OLS regression coefficient for the slope between the timepoint B data points. Treats the timepoints as equally spaced integers (e.g., rather than modelling them as dates). 
#' @return p: Hypothesis test p value via permutation test. Calculated via Monte-Carlo simulation (10000 runs) rather than brute force.
#' @return median_difference: Difference between the median value of A and the median value of B. Because Ruscio's A is non-parametric it can notionally suffer from ceiling effects, e.g., where A = 1.00 but the real difference between the groups are larger or smaller. It can therefore be useful to report the unstandardized effect size as well as the standardized effect sizes. Median difference is used here over mean difference given its greater robustness.
#' @return ruscios_A: Effect size A (Ruscio, 2008). The probability that a randomly selected timepoint in condition B is larger than a randomly selected timepoint in condition A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/the probability of superiority but with no parametric assumptions. A Cohen's d of 1.5 corrisponds to an A of 0.85.
#' @return hedges_g: Effect size Hedge's g effect size via bootstrapping, a version of Cohen's d that is bias corrected for small sample sizes. Identical range, interpretation and cutoffs as Cohen's d. Included here for familiarity: it's parametric assumtions (equal variances) and sensitivity to equal number of timepoints in A and B make it somewhat unrobust in many SCED contexts. In order to relax the assumption of normality a bootstrapped implemenation is employed.
#' @export
#' @examples
#' sced_results <- sced_analysis(data = simulated_data)

sced_analysis <- function(data, n_boots = 2000, invert_effect_sizes = FALSE, adjust_probability_ceiling = TRUE) {
  require(tidyverse)
  require(coin)
  require(effsize)
  require(bootES)
  data(simulated_data)
  
  # trends at baseline and post intervention
  baseline_trend <- data %>%
    filter(Condition == "A") %>%
    # standardize data
    mutate(standardized_score = as.numeric(scale(Score))) %>%
    # fit linear model for each participant
    group_by(Participant) %>%
    mutate(timepoint_integer          = row_number(),
           timepoint_integer_centered = as.numeric(scale(timepoint_integer))) %>%
    do(tidy(lm(standardized_score ~ timepoint_integer_centered, data = .))) %>%
    ungroup() %>%
    # extract standardized beta estimates
    filter(term == "timepoint_integer_centered") %>%
    # tidy names
    mutate(`Baseline trend`    = round(estimate, 2)) %>%
    select(Participant, `Baseline trend`)
  
  post_intervention_trend <- data %>%
    filter(Condition == "B") %>%
    # standardize data
    mutate(standardized_score = as.numeric(scale(Score))) %>%
    # fit linear model for each participant
    group_by(Participant) %>%
    mutate(timepoint_integer = row_number(),
           timepoint_integer_centered = as.numeric(scale(timepoint_integer))) %>%
    do(tidy(lm(standardized_score ~ timepoint_integer_centered, data = .))) %>%
    ungroup() %>%
    # extract standardized beta estimates
    filter(term == "timepoint_integer_centered") %>%
    # tidy names
    mutate(`Intervention trend` = round(estimate, 2)) %>%
    select(Participant, `Intervention trend`)
  
  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(B = n_boots*10), # needs more than other tests
                                    data = .))) %>%
    ungroup() %>%
    dplyr::mutate(p = as.numeric(p),
                  p = format_pval_better(p))
  
  median_change <- data %>%
    group_by(Participant) %>%
    dplyr::summarize(median_a          = median(Score[Condition == "A"]),
                     median_b          = median(Score[Condition == "B"]),
                     median_difference = median_b - median_a,
                     na.rm             = TRUE) %>%
    dplyr::select(Participant, median_difference)
  
  # bootstrapped Ruscio's nonparametric effect size A
  # function defined elsewhere in this package
  ruscios_A_boot_by_participant <- data %>%
    group_by(Participant) %>%
    do(ruscios_A_boot(variable = "Score",
                      group = "Condition",
                      data = .,
                      value1 = "B",
                      value2 = "A",
                      B = n_boots,
                      adjust_ceiling = adjust_probability_ceiling)) %>%
    ungroup()

  # bootstrapped Hedges' g effect size (removes assumption of normality but not equality of variances or equal N per condition)
  hedges_g_boot <- function(data) {
    require(bootES)
    require(tidyverse)
    
    fit <- data %>%
      bootES(.,
             R           = n_boots,
             data.col    = "Score",
             group.col   = "Condition",
             contrast    = c(A = -1, B = 1),
             effect.type = "hedges.g",
             ci.type     = "bca",
             ci.conf     = 0.95)
    
    results <- data.frame(hedges_g        = round(fit$t0,        3),
                          hedges_g_se     = round(sd(fit$t),     3),
                          hedges_g_ci_lwr = round(fit$bounds[1], 3),
                          hedges_g_ci_upr = round(fit$bounds[2], 3))
  }
  
  hedges_g_by_participant <- data %>%
    group_by(Participant) %>%
    do(hedges_g_boot(data = .)) %>%
    ungroup()
  
  # combine results
  results <- baseline_trend %>%
    left_join(post_intervention_trend,       by = "Participant") %>%
    left_join(p_by_participant,              by = "Participant") %>%
    left_join(median_change,                 by = "Participant") %>%
    left_join(ruscios_A_boot_by_participant, by = "Participant") %>%
    left_join(hedges_g_by_participant,       by = "Participant")
    
  # conditionally invert effect sizes results
  if (invert_effect_sizes == TRUE){
    results <- results %>%
      mutate(ruscios_A = 1 - ruscios_A,
             ruscios_A_ci_lwr = 1 - ruscios_A_ci_lwr,
             ruscios_A_ci_upr = 1 - ruscios_A_ci_upr,
             hedges_g = hedges_g*-1,
             hedges_g_ci_lwr = hedges_g_ci_lwr*-1,
             hedges_g_ci_upr = hedges_g_ci_upr*-1)
  }  
  
  return(results)
}
