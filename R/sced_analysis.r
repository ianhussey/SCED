#' Analyse data
#'
#' Analyse data from an AB design SCED experiment using non-parametric frequentist tests
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @param n_boots: number of bootstrapped resamples for Hedges' g and Ruscio's A. N for p value permutation is n_boots*10. 
#' @param invert_effect_sizes: Effect sizes are reported assuming that scores in timepoint B are expected to be higher than timepoint A (i.e., that the intervention causes scores to increase). If invert_effect_sizes == TRUE then effect sizes are inverted, e.g., if the intervention is expected to causes scores to decrease.
#' @param adjust_probability_ceiling: Should Ruscio's A estimates of 0 and 1 be adjusted so that they can be converted to finite odds ratios? This is done by rescoring a single data point as being was inferior to a single second data point between the conditions. Ie., it uses the best granularity allowed by the data, as more data points will result in a more extreme possible values of A.  
#' @return n_A: Number of data points in phase A
#' @return n_B: Number of data points in phase B
#' @return deviation_A: Median Absolute Deviation of data points in phase A
#' @return deviation_B: Median Absolute Deviation of data points in phase B
#' @return deviation_A_likely_outlier: It can be useful to know if a participant demonstrates poorer consistency within the baseline phase (phase A). There is a tradition within the visual analysis of SCED data to exclude participants who demonstrate such low 'consistency'. In addition to this, the meta analysis of the unstandardized effect size between participants (i.e., the calculation of the median median-difference between phases) tacitly relies on equal variances between participants. Although, it should be noted that both standardized effect sizes (Ruscio's A and Hedges' g) do *not* rely on the assumption of equal variance between participants (indeed, this is one of the rationales for standardization), and so no exclusions based on baseline consistency are necessary for the correct interpretation of them. In order to highlight outliers, this package relies on between subject meta analysis of median absolute deviations of the data in phase A. The metafor package's influence.rma.uni() function is used to conduct leave one out analyses. Four separate metrics that can indicate whether a participant are calculated, and if one or more flag the participant as a potential outlier this variable is returned with a star ("*"). See the metafor package's documentation for more details. This variable should not be used to thoughtlessly exclude participants, but should be combined with insepction of the SCED plot and best judgement. Sensitivity analyses run with and wihtout any outliers can also be useful.
#' @return trend_A: ordinal regression slope between the phase A data points by timepoint. Treats the timepoints as ordinally spaced integers (e.g., rather than modelling them as dates). Can be used to exclude participants from consideration in meta analysis, e.g., on the basis that differences between phases can be difficult to interpret if improvement trends are observed in phase A. Sensitivity analyses can be conducted by excluding participants with phase A trends greater than a given absolute value (e.g., +/-0.3). 
#' @return trend_B: ordinal regression slope between the phase B data points by timepoint. Can indicate ongoing improvement in phase B.
#' @return p: Hypothesis test p value via permutation test assessing the differences in scores between the two phases. Calculated via Monte-Carlo simulation (10000 runs) rather than brute force.
#' @return median_difference: Difference in scores between the median values in the two phases. Because Ruscio's A effect size is probability based (see below) it can suffer from ceiling effects. I.e., when all timepoints in phase B are larger than all timepoints in phase A, Ruscio's A cannot distinguish futher between large and very large effect sizes. It can therefore be useful to report the unstandardized effect size as well as the standardized effect sizes. 
#' @return ruscios_A: Differences in scores between the two phases using Ruscio's A (Ruscio, 2008). The probability that a randomly selected timepoint in phase B is larger than a randomly selected timepoint in phase A. Ranges from 0 to 1, where 0.5 is equal chance. Highly similar to Area Under the Curve (AUC)/the Common Language Effect Size (CLES)/and the probability of superiority. A Cohen's d of 1.5 corrisponds to an A of 0.85.
#' @return hedges_g: Differences in scores between the two phases using Hedge's g effect size via bootstrapping, a version of Cohen's d that is bias corrected for small sample sizes. Identical range, interpretation and cutoffs as Cohen's d. Included here for familiarity: it's parametric assumtions (equal variances) and sensitivity to equal number of timepoints in A and B make it somewhat unrobust in many SCED contexts. In order to relax the assumption of normality a bootstrapped implemenation is employed.
#' @export
#' @examples
#' sced_results <- sced_analysis(data = simulated_data)

sced_analysis <- function(data, n_boots = 2000, invert_effect_sizes = FALSE, adjust_probability_ceiling = TRUE) {
  
  require(tidyverse)
  require(broom)
  require(coin)
  require(effsize)
  require(bootES)
  require(metafor)

  data(simulated_data)
  
  # drop NAs from relevant columns
  data <- data %>%
    drop_na(Timepoint, Score, Participant, Condition)
  
  # trends at baseline and post intervention
  trends <- data %>%
    # fit linear model for each participant and condition
    dplyr::group_by(Participant, Condition) %>%
    dplyr::mutate(timepoint_integer = row_number()) %>%
    do(tidy(lm(rank(Score) ~ rank(timepoint_integer), data = .))) %>%
    dplyr::ungroup() %>%
    # extract standardized beta estimates
    dplyr::filter(term == "rank(timepoint_integer)") %>%
    # tidy names
    dplyr::select(Participant, Condition, estimate, std.error) %>%
    dplyr::mutate(trend_ci_lwr = estimate-std.error*1.96,
                  trend_ci_upr = estimate+std.error*1.96) %>%
    round_df(2)
  
  trend_A <- trends %>%
    filter(Condition == "A") %>%
    rename(trend_A = estimate,
           trend_A_ci_lwr = trend_ci_lwr,
           trend_A_ci_upr = trend_ci_upr) %>%
    select(Participant, trend_A, trend_A_ci_lwr, trend_A_ci_upr)
  
  trend_B <- trends %>%
    filter(Condition == "B") %>%
    rename(trend_B = estimate,
           trend_B_ci_lwr = trend_ci_lwr,
           trend_B_ci_upr = trend_ci_upr) %>%
    select(Participant, trend_B, trend_B_ci_lwr, trend_B_ci_upr)

  # median absolute deviation for each participant and condition to assess consistency within that phase
  deviations <- data %>%
    dplyr::group_by(Participant, Condition) %>%
    dplyr::summarize(mad = mad(Score)) %>%
    dplyr::select(Participant, Condition, mad) %>% 
    spread(Condition, mad) %>%
    dplyr::rename(deviation_A = A,
                  deviation_B = B) %>%
    round_df(2)
  
  timepoints <- data %>%
    dplyr::group_by(Participant, Condition) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::select(Participant, Condition, n) %>% 
    spread(Condition, n) %>%
    dplyr::rename(n_A = A,
                  n_B = B)
  
  deviations_and_timepoints <- left_join(timepoints, deviations, by = "Participant")
  
  # find likely phase A deviation outliers
  ## fit meta analysis of phase A deviations
  deviation_meta_fit <- deviations_and_timepoints %>%
    dplyr::mutate(yi = deviation_A,
                  vi = deviation_A^2/(2*n_A)) %>% # variance of deviation
    metafor::rma(yi = yi, 
                 vi = vi,
                 data = .)
  
  deviation_outliers <- metafor::influence.rma.uni(deviation_meta_fit)$inf %>%
    as.data.frame() %>%
    rownames_to_column(var = "Participant") %>%
    round_df(2) %>%
    dplyr::rename(deviation_A_likely_outlier = `inf`) %>%
    dplyr::select(deviation_A_likely_outlier)
  
  deviations_and_timepoints_and_outliers <- bind_cols(deviations_and_timepoints, deviation_outliers)
  
  # p values via non-parametric permutation tests
  p_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(p = pvalue(independence_test(Score ~ as.factor(Condition),
                                    distribution = approximate(nresample = n_boots*10), # needs more than other tests
                                    data = .))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p = as.numeric(p),
                  p = format_pval_better(p))
  
  median_change <- data %>%
    dplyr::group_by(Participant) %>%
    dplyr::summarize(median_a          = median(Score[Condition == "A"]),
                     median_b          = median(Score[Condition == "B"]),
                     median_difference = median_b - median_a,
                     na.rm             = TRUE) %>%
    dplyr::select(Participant, median_difference)
  
  # bootstrapped Ruscio's nonparametric effect size A
  # function defined elsewhere in this package
  ruscios_A_boot_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(ruscios_A_boot(variable = "Score",
                      group = "Condition",
                      data = .,
                      value1 = "B",
                      value2 = "A",
                      B = n_boots,
                      adjust_ceiling = adjust_probability_ceiling)) %>%
    dplyr::ungroup()
  
  # bootstrapped Hedges' g effect size (removes assumption of normality but not equality of variances or equal N per condition)
  hedges_g_boot <- function(data) {
    require(bootES)
    require(tidyverse)
    
    fit <- data %>%
      bootES::bootES(.,
                     R           = n_boots,
                     data.col    = "Score",
                     group.col   = "Condition",
                     contrast    = c(A = -1, B = 1),
                     effect.type = "hedges.g",
                     ci.type     = "bca",
                     ci.conf     = 0.95)
    
    results <- data.frame(hedges_g        = fit$t0,
                          hedges_g_se     = sd(fit$t),
                          hedges_g_ci_lwr = fit$bounds[1],
                          hedges_g_ci_upr = fit$bounds[2]) %>%
      round_df(2)
    
  }
  
  hedges_g_by_participant <- data %>%
    dplyr::group_by(Participant) %>%
    do(hedges_g_boot(data = .)) %>%
    dplyr::ungroup()
  
  # combine results
  results <- deviations_and_timepoints_and_outliers %>%
    dplyr::left_join(trend_A,                       by = "Participant") %>%
    dplyr::left_join(trend_B,                       by = "Participant") %>%
    dplyr::left_join(p_by_participant,              by = "Participant") %>%
    dplyr::left_join(median_change,                 by = "Participant") %>%
    dplyr::left_join(ruscios_A_boot_by_participant, by = "Participant") %>%
    dplyr::left_join(hedges_g_by_participant,       by = "Participant") %>%
    ungroup()
  
  # conditionally invert effect sizes results
  if (invert_effect_sizes == TRUE){
    results <- results %>%
      dplyr::mutate(ruscios_A = 1 - ruscios_A,
                    ruscios_A_ci_lwr = 1 - ruscios_A_ci_lwr,
                    ruscios_A_ci_upr = 1 - ruscios_A_ci_upr,
                    hedges_g = hedges_g*-1,
                    hedges_g_ci_lwr = hedges_g_ci_lwr*-1,
                    hedges_g_ci_upr = hedges_g_ci_upr*-1)
  }  
  
  return(results)
}
