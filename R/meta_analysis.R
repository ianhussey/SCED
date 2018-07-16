#' Meta analyse probability values (Ruscio's A)
#'
#' Meta analyse the robust effect sizes (Ruscio's A) provided by sced_analyse
#' @param results Output of sced_analyse.
#' @export
#' @examples
#' sced_meta_analysis(results = sced_results)

sced_meta_analysis <- function(results) {
  require(tidyverse)
  require(metafor)

  # get data
  data_for_meta_analysis <- results %>%
    mutate(
      # convert probabilties to odds ratios
      or = ruscios_A_median / (1 - ruscios_A_median),
      or_ci_lower = ruscios_A_ci_lwr / (1 - ruscios_A_ci_lwr),
      or_ci_upper = ruscios_A_ci_upr / (1 - ruscios_A_ci_upr),
      # convert odds ratios to log odds
      yi = log(or),
      # convert CIs to SEs
      sei = (log(or_ci_upper) - log(or_ci_lower)) / (2*1.96),
      # convert SE to variance
      vi = sei^2
    )
  
  # fit Random Effects model using metafor package
  fit <- rma(yi, vi, 
             data = data_for_meta_analysis,
             slab = paste(Participant))
  
  # make predictions converting to odds ratios
  Ruscios_A_predictions <- 
    predict(fit, transf = exp, digits = 5) %>% 
    as.data.frame() %>%
    gather() %>%
    mutate(value = value/(1 + value)) %>%
    round_df(3) %>%
    rename(Estimate = key,
           Value = value) %>%
    mutate(Estimate = recode(Estimate,
                             "pred" = "Meta analysed Ruscio's A",
                             "ci.lb" = "95% CI lower",
                             "ci.ub" = "95% CI upper",
                             "cr.lb" = "95% CR lower",
                             "cr.ub" = "95% CR upper")) 
  
  # print results
  return(list(model_fit = fit, 
              meta_analysed_effect_size = Ruscios_A_predictions))
}
