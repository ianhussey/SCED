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
#' meta_analysis$meta_effect
#' meta_analysis$meta_heterogeneity
#' 
#' # forest plot
#' forest(meta_analysis$model_fit,
#'        xlab = "Ruscio's A",
#'               digits = 2,
#'               addcred = TRUE,
#'               refline = 0.5)
#' ## add column headings to the plot
#' text(-6.3, 6.8, "Participant", pos = 4)
#' text( 8.4, 6.8, "Ruscio's A [95% CI]", pos = 2)
#' 

sced_meta_analysis <- function(results, effect_size, baseline_trend_exclusion_criterion = NULL) {
  require(tidyverse)
  require(metafor)
  require(timesavers)
  
  # exclude participants due to baseline trends
  if (is.numeric(baseline_trend_exclusion_criterion)) {
    
    results <- results %>%
      filter(abs(`Baseline trend`) <= baseline_trend_exclusion_criterion)
    
  }
  
  # get data
  if (effect_size %in% c("ruscios_A", "A")) {
    
    yi <- results$ruscios_A
    vi <- results$ruscios_A_se^2
    
    es_label <- "Meta analysed Ruscio's A"
    es_label_2 <- ", Ruscio's A = "
    
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
                  vi = vi)  # convert SE to variance
  
  # robust unstandardized effect size: median (median difference between conditions)
  
  mdn_mdn_diff <- results %>%
    summarize(mdn_mdn_diff = median(median_difference)) %>%
    pull(mdn_mdn_diff)
  
  # fit Random Effects model using metafor package
  meta_fit <- rma(yi, vi,
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
  
  # create results strings
  meta_effect_string <- 
    paste0("Meta analysis: N = ", meta_fit$k,  # number of single cases
           ", unstandardized effect size (median median-difference) = ", mdn_mdn_diff, # median (median difference between conditions) across participants
           es_label_2, predictions$estimate[1],  # standardized ES
           ", 95% CI [", predictions$estimate[3], ", ", predictions$estimate[4], "]",  # 95% Confidence interval (long run contains mean standardized ES)
           ", 95% CR [", predictions$estimate[5], ", ", predictions$estimate[6], "]")  # 95% Credibility interval (long run contains observed standardized ES)
  
  meta_heterogeneity_string <- 
    paste0("Heterogeneity tests: Q(df = ", meta_fit$k - 1, ") = ", round(meta_fit$QE, 2), 
           ", p = ", ifelse(meta_fit$pval < 0.001, "< .001", as.character(round(meta_fit$pval, 3))),
           ", tau^2 = ", round(meta_fit$tau2, 2), 
           ", I^2 = ",   round(meta_fit$I2, 2),
           ", H^2 = ",   round(meta_fit$H2, 2))
  
  # returns list of results
  return(list(data = results,
              median_median_difference_unstandardized_effect_size = mdn_mdn_diff,
              model_fit = meta_fit,
              meta_analysed_standardized_effect_size = predictions,
              meta_effect = meta_effect_string,
              meta_heterogeneity = meta_heterogeneity_string))
  
}


