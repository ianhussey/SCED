#' Meta analyse standardized effect sizes across single cases
#'
#' Meta analyse the standardized effect sizes (Ruscio's A or Hedges' g) provided by sced_analyse.
#' Uses the metafor package under the hood. 
#' Please note that the CIs reported in the meta analysis may differ from those reported by sced_analyse(), as the meta analysis uses the standard error for estimation rather than the empirical CIs, which can be asymmetric. When combined with the next point below, this means that the CIs in the meta analysis can be < 0 or > 1, outside the bounds of the observable values for Ruscio's A. If the meta analysed effect size intervals are < 0 or > 1 they can be reported as 0 or 1 instead.
#' Meta analysis is done via a maximum likelihood random effects model using a gaussian link function. 
#' @import dplyr
#' @import metafor
#' @importFrom finalfit round_tidy
#' @param results Output of sced_analysis().
#' @param baseline_trend_exclusion_criterion If set to a numeric value, cases with a baseline trend (ie standardized beta OLS regression value for the timepoint A scores) with an absolute value greater than this numeric value will be excluded from the meta analysis.
#' @param baseline_variance_exclude_outliers If TRUE, participants whose variance at baseline is flagged as an outlier by the sced_analysis() function are excluded from the meta-analysis. Useful for sensitivity analysis, for excluding participants from whom a stable baseline was not established relative to other participants.
#' @export
#' @examples
#' \dontrun{
#' sced_results <- sced_analysis(data = simulated_data)
#' 
#' # fit meta
#' sced_meta_fit <- sced_meta_analysis(results = sced_results)
#' 
#' # return results
#' sced_meta_fit$meta_effect_size
#' sced_meta_fit$meta_heterogeneity
#' 
#' # forest plot
#' metafor::forest(sced_meta_fit$model_fit,
#'                 xlab = "Standardized Mean Difference",
#'                 digits = 2,
#'                 addcred = TRUE,
#'                 refline = 0,
#'                 header = c("Participant", "SMD [95% CI]"))
#' }

sced_meta_analysis <- function(results, 
                               baseline_trend_exclusion_criterion = NULL, 
                               baseline_variance_exclude_outliers = FALSE) {
  
  # exclude participants due to baseline trends
  if (is.numeric(baseline_trend_exclusion_criterion)) {
    
    results <- results %>%
      dplyr::filter(abs(trend_A) <= baseline_trend_exclusion_criterion)
    
  }
  
  # exclude participants due to baseline variance
  if (is.numeric(baseline_variance_exclude_outliers)) {
    
    results <- results %>%
      dplyr::filter(deviation_A_likely_outlier != "*")
    
  }

  # robust unstandardized effect size: median (median difference between conditions)
  mdn_mdn_diff <- results %>%
    dplyr::summarize(mdn_mdn_diff = median(median_difference)) %>%
    dplyr::pull(mdn_mdn_diff)
  
  # fit Random Effects model using metafor package
  meta_fit <- rma(yi = hedges_g, 
                  vi = hedges_g_se^2,
                  data = results,
                  slab = paste(Participant))
  
  # make predictions
  predictions <-
    predict(meta_fit) %>%
    as.data.frame() 
  
  # create results strings
  meta_effect_string <- 
    paste0("Meta analysis: N = ", meta_fit$k,  # number of single cases
           ", unstandardized effect size (median median-difference) = ", mdn_mdn_diff, # median (median difference between conditions) across participants
           ", Hedges' g = ", finalfit::round_tidy(predictions$pred, 2),  # standardized ES
           ", 95% CI [", finalfit::round_tidy(predictions$ci.lb, 2), ", ", finalfit::round_tidy(predictions$ci.ub, 2), "]",  # 95% Confidence interval (long run contains mean standardized ES)
           ", 95% PI [", finalfit::round_tidy(predictions$pi.lb, 2), ", ", finalfit::round_tidy(predictions$pi.ub, 2), "]",  # 95% Prediction interval (long run contains observed standardized ES given heterogeneity)
           ", p ", format_pval_better(meta_fit$pval))
  
  meta_heterogeneity_string <- 
    paste0("Heterogeneity tests: Q(df = ", meta_fit$k - 1, ") = ", finalfit::round_tidy(meta_fit$QE, 2), 
           ", p ",     format_pval_better(meta_fit$QEp),
           ", tau^2 = ", finalfit::round_tidy(meta_fit$tau2, 2), 
           ", I^2 = ",   finalfit::round_tidy(meta_fit$I2, 2),
           ", H^2 = ",   finalfit::round_tidy(meta_fit$H2, 2))
  
  # returns list of results
  return(list(data = results,
              median_median_difference_unstandardized_effect_size = mdn_mdn_diff,
              model_fit = meta_fit,
              meta_analysed_standardized_effect_size = predictions,
              meta_effect_size = meta_effect_string,
              meta_heterogeneity = meta_heterogeneity_string))
}


