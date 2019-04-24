#' Meta analyse standardized effect sizes across single cases
#'
#' Meta analyse the standardized effect sizes (Ruscio's A or Hedges' g) provided by sced_analyse.
#' Uses the metafor package under the hood. 
#' Please note that the CIs reported in the meta analysis may differ from those reported by sced_analyse(), as the meta analysis uses the standard error for estimation rather than the empirical CIs, which can be asymmetric. When combined with the next point below, this means that the CIs in the meta analysis can be < 0 or > 1, outside the bounds of the observable values for Ruscio's A. If the meta analysed effect size intervals are < 0 or > 1 they can be reported as 0 or 1 instead.
#' Meta analysis is done via a maximum likelihood random effects model using a gaussian link function for both Ruscio's A and Hedges' g effect sizes. In the case of Ruscio's A, this means that effect sizes are non parametric at the individual level, but the underlying effect is assumed to vary normally between participants. Other strategies for the meta analysis of probability are possible (e.g., some have employed logit link functions), but due to its relative rarity no best practices have yet emerged. 
#' @param results Output of sced_analysis().
#' @param effect_size Which standardized effect size should be meta analysed. Must be set to either 'ruscios_A', 'A', 'hedges_g', or 'g'. Ruscio's A is more robust; Hedges' g is included for familiarity.
#' @param baseline_trend_exclusion_criterion If set to a numeric value, cases with a baseline trend (ie standardized beta OLS regression value for the timepoint A scores) with an absolute value greater than this numeric value will be excluded from the meta analysis.
#' @export
#' @examples
#' # meta analysis
#' meta_analysis <- sced_meta_analysis(results = results, effect_size = "ruscios_A")
#' 
#' # return results
#' meta_analysis$meta_effect_size
#' meta_analysis$meta_heterogeneity
#' 
#' # forest plot
#' forest(meta_analysis$model_fit,
#'        xlab = "Probability of superiority (Ruscio's A)",
#'        digits = 2,
#'        addcred = TRUE,
#'        refline = 0.5)
#' 

sced_meta_analysis <- function(results, effect_size, baseline_trend_exclusion_criterion = NULL) {
  require(tidyverse)
  require(metafor)
  
  # exclude participants due to baseline trends
  if (is.numeric(baseline_trend_exclusion_criterion)) {
    
    results <- results %>%
      filter(abs(`Baseline trend`) <= baseline_trend_exclusion_criterion)
    
  }
  
  # get data
  if (effect_size %in% c("ruscios_A", "A")) {
    
    # convert OR CIs to log OR SE http://www.metafor-project.org/doku.php/tips:assembling_data_or
    results_temp <- results %>%
      dplyr::mutate(log_OR = log(ruscios_A / (1 - ruscios_A)),
                    OR_lwr = ruscios_A_ci_lwr / (1 - ruscios_A_ci_lwr),
                    OR_upr = ruscios_A_ci_upr / (1 - ruscios_A_ci_upr),
                    log_OR_se = (log(OR_lwr) - log(OR_upr)) / (2*1.96))
    
    yi <- results_temp %>%
      pull(log_OR)
    
    vi <- results_temp %>%
      pull(log_OR_se)
    
    es_label <- "Meta analysed Generalized Odds Ratio"
    
  } else if (effect_size %in% c("hedges_g", "g")) {
    
    yi <- results$hedges_g
    vi <- results$hedges_g_se^2
    
    es_label <- "Meta analysed Hedges' g"
    es_label_2 <- ", Hedges' g = "
    
  } else {
    print("effect_size must be set to 'ruscios_A', 'A', 'hedges_g', or 'g'")
  }
  
  data_for_meta_analysis <- results %>%
    dplyr::mutate(yi = yi,
                  vi = vi)  
  
  # robust unstandardized effect size: median (median difference between conditions)
  
  mdn_mdn_diff <- results %>%
    dplyr::summarize(mdn_mdn_diff = median(median_difference)) %>%
    pull(mdn_mdn_diff)
  
  # fit Random Effects model using metafor package
  meta_fit <- rma(yi = yi, 
                  vi = vi,
                  data = data_for_meta_analysis,
                  slab = paste(Participant))
  
  # make predictions
  predictions <-
    predict(meta_fit) %>%
    as.data.frame() %>%
    gather() %>%
    round_df(3) %>%
    dplyr::rename(metric = key,
                  estimate = value) %>%
    mutate(metric = dplyr::recode(metric,
                                  "pred" = es_label,
                                  "ci.lb" = "95% CI lower",
                                  "ci.ub" = "95% CI upper",
                                  "cr.lb" = "95% CR lower",
                                  "cr.ub" = "95% CR upper"))
  
  
  if (effect_size %in% c("ruscios_A", "A")) {
    # exponentiate the log odds to get odds ratios
    predictions <- predictions %>%
      dplyr::mutate(estimate = exp(estimate))
    
    # convert generalized odds ratios to probabilities
    predictions_as_probabilities <- predictions %>%
      dplyr::mutate(estimate = estimate/(1 + estimate),
             metric = str_replace(metric, "Generalized Odds Ratio", "Ruscio's A")) %>%
      round_df(3) %>%
      filter(metric != "se")
    
    # only round odds ratios after probabilities have been calculated to prevent information loss
    predictions <- round_df(predictions, 2)
    
  } else {
    predictions_as_probabilities <- NULL
  }
  
  # create results strings
  if (effect_size %in% c("ruscios_A", "A")) {
    meta_effect_string <- 
      paste0("Meta analysis: N = ", meta_fit$k,  # number of single cases
             ", unstandardized effect size (median median-difference) = ", mdn_mdn_diff, # median (median difference between conditions) across participants
             ", Ruscio's A = ", predictions_as_probabilities$estimate[1],  # standardized ES
             ", 95% CI [", predictions_as_probabilities$estimate[2], ", ", predictions_as_probabilities$estimate[3], "]",  # 95% Confidence interval (long run contains mean standardized ES)
             ", 95% CR [", predictions_as_probabilities$estimate[4], ", ", predictions_as_probabilities$estimate[5], "]",  # 95% Credibility interval (long run contains observed standardized ES)
             ", Generalized OR = ", predictions$estimate[1],  # standardized ES
             ", 95% CI [", predictions$estimate[3], ", ", predictions$estimate[4], "]",  # 95% Confidence interval (long run contains mean standardized ES)
             ", 95% CR [", predictions$estimate[5], ", ", predictions$estimate[6], "]",  # 95% Credibility interval (long run contains observed standardized ES)
             ", p ", format_pval_better(meta_fit$pval))
  } else {
    meta_effect_string <- 
      paste0("Meta analysis: N = ", meta_fit$k,  # number of single cases
             ", unstandardized effect size (median median-difference) = ", mdn_mdn_diff, # median (median difference between conditions) across participants
             es_label_2, predictions$estimate[1],  # standardized ES
             ", 95% CI [", predictions$estimate[3], ", ", predictions$estimate[4], "]",  # 95% Confidence interval (long run contains mean standardized ES)
             ", 95% CR [", predictions$estimate[5], ", ", predictions$estimate[6], "]",  # 95% Credibility interval (long run contains observed standardized ES)
             ", p ", format_pval_better(meta_fit$pval))
  }
  
  meta_heterogeneity_string <- 
    paste0("Heterogeneity tests: Q(df = ", meta_fit$k - 1, ") = ", round(meta_fit$QE, 2), 
           ", p ",     format_pval_better(meta_fit$QEp),
           ", tau^2 = ", round(meta_fit$tau2, 2), 
           ", I^2 = ",   round(meta_fit$I2, 2),
           ", H^2 = ",   round(meta_fit$H2, 2))
  
  # returns list of results
  return(list(data = results,
              median_median_difference_unstandardized_effect_size = mdn_mdn_diff,
              model_fit = meta_fit,
              meta_analysed_standardized_effect_size = predictions,
              meta_analysed_standardized_effect_size_as_probability = predictions_as_probabilities,
              meta_effect_size = meta_effect_string,
              meta_heterogeneity = meta_heterogeneity_string))
  
}


