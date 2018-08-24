#' Meta analyse probability values (Ruscio's A)
#'
#' Meta analyse the robust effect sizes (Ruscio's A) provided by sced_analyse
#' @param results Output of sced_analysis().
#' @export
#' @examples
#' # meta analysis
#' meta_analysis <- sced_meta_analysis(results = results)
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

sced_meta_analysis <- function(results) {
  require(tidyverse)
  require(metafor)
  require(timesavers)
  
  # get data
  data_for_meta_analysis <- results %>%
    dplyr::mutate(
      yi = ruscios_A,
      # convert SE to variance
      vi = ruscios_A_se^2
    )
  
  # fit Random Effects model using metafor package
  meta_fit <- rma(yi, vi,
                  data = data_for_meta_analysis,
                  slab = paste(Participant))
  
  # make predictions converting to odds ratios
  predictions <-
    predict(meta_fit) %>%
    as.data.frame() %>%
    gather() %>%
    round_df(3) %>%
    dplyr::rename(metric = key,
                  estimate = value) %>%
    mutate(metric = dplyr::recode(metric,
                                  "pred" = "Meta analysed Ruscio's A",
                                  "ci.lb" = "95% CI lower",
                                  "ci.ub" = "95% CI upper",
                                  "cr.lb" = "95% CR lower",
                                  "cr.ub" = "95% CR upper"))
  
  meta_effect_string <- 
    paste0("Meta analysis: k = ", meta_fit$k, 
           ", Ruscio's A = ", predictions$estimate[1],
           ", 95% CI [", predictions$estimate[3], ", ", predictions$estimate[4], "]",
           ", 95% CR [", predictions$estimate[5], ", ", predictions$estimate[6], "]")
  
  meta_heterogeneity_string <- 
    paste0("Heterogeneity tests: Q(df = ", meta_fit$k - 1, ") = ", round(meta_fit$QE, 2), 
           ", p = ", ifelse(meta_fit$pval < 0.001, "< .001", as.character(round(meta_fit$pval, 3))),
           ", tau^2 = ", round(meta_fit$tau2, 2), 
           ", I^2 = ",   round(meta_fit$I2, 2),
           ", H^2 = ",   round(meta_fit$H2, 2))
  
  # print results
  return(list(model_fit = meta_fit,
              meta_analysed_effect_size = predictions,
              meta_effect = meta_effect_string,
              meta_heterogeneity = meta_heterogeneity_string))
}
