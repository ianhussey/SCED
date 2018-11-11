#' Create forest plot from meta analysis results 
#'
#' Meta analyse the standardized effect sizes (Ruscio's A or Hedges' g) provided by sced_analyse.
#' @param meta_analysis Output of sced_meta_analysis().
#' @param baseline_trend_exclusion_criterion If set to a numeric value, cases with a baseline trend (ie standardized beta OLS regression value for the timepoint A scores) with an absolute value greater than this numeric value will be excluded from the meta analysis.
#' @export
#' @examples
#' # meta analysis
#' meta_analysis <- sced_meta_analysis(results = results, effect_size = "ruscios_A")
#' 
#' # forest plot
#' forest_plot(meta_analysis)
#' 

forest_plot <- function(meta_analysis_results, 
                        effect_size,
                        baseline_trend_exclusion_criterion = NULL) {
  
  # exclude participants due to baseline trends
  if (is.numeric(baseline_trend_exclusion_criterion)) {
    plotting_data <- meta_analysis_results$data %>%
      filter(abs(`Baseline trend`) <= baseline_trend_exclusion_criterion)
  } else {
    plotting_data <- meta_analysis_results$data
  }
  
  # credibility intervals
  CR_lwr <- meta_analysis_results$meta_analysed_standardized_effect_size %>%
    filter(metric == "95% CR lower") %>%
    pull(estimate)
  
  CR_upr <- meta_analysis_results$meta_analysed_standardized_effect_size %>%
    filter(metric == "95% CR upper") %>%
    pull(estimate)
  
  
  # produce plot for appropriate effect size stat
  if (effect_size %in% c("ruscios_A", "A")) {
    
    combined_plotting_data <- meta_analysis_results$data %>%
      select(Participant, ruscios_A, ruscios_A_se, ruscios_A_ci_lwr, ruscios_A_ci_upr) %>%
      rbind(data.frame(Participant      = "Meta analysis",
                       ruscios_A        = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[1],
                       ruscios_A_se     = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[2],
                       ruscios_A_ci_lwr = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[3],
                       ruscios_A_ci_upr = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[4])) %>%
      mutate(Participant      = fct_rev(Participant),
             size             = max(ruscios_A_se*100) - ruscios_A_se*100 + 1,
             ruscios_A_cr_lwr = ifelse(as.character(Participant) == "Meta analysis", CR_lwr, NA),
             ruscios_A_cr_upr = ifelse(as.character(Participant) == "Meta analysis", CR_upr, NA),
             results_string   = paste0(format(round(ruscios_A, digits), nsmall = digits), " [",
                                       format(round(ruscios_A_ci_lwr, digits), nsmall = digits), ", ",
                                       format(round(ruscios_A_ci_upr, digits), nsmall = digits), "]"),
             results_string   = ifelse(as.character(Participant) == "Meta analysis", 
                                       paste0(results_string, "\n        [",
                                              format(round(ruscios_A_cr_lwr, digits), nsmall = digits), ", ",
                                              format(round(ruscios_A_cr_upr, digits), nsmall = digits), "]"),
                                       results_string))
    
    p1 <- 
      ggplot(combined_plotting_data, aes(Participant, ruscios_A)) +
      geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
      geom_linerange(aes(ymin = ruscios_A_ci_lwr, ymax = ruscios_A_ci_upr)) +
      geom_linerange(aes(ymin = ruscios_A_cr_lwr, ymax = ruscios_A_cr_upr), linetype = "dotted") +
      geom_point(shape = "square", size = combined_plotting_data$size) +
      coord_flip() +
      scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
      labs(x = "Participant", y = "Ruscio's A") +
      brmstools::theme_forest()
    
    p2 <- 
      ggplot(combined_plotting_data, aes(Participant, 1)) +
      geom_text(aes_string(label = "results_string"),
                hjust = "inward", 
                size = 3) +
      coord_flip() +
      theme_void() + 
      theme(panel.grid = element_blank(), panel.border = element_blank())
    
    # combine plots
    plot <- p1 + p2 + plot_layout(nrow = 1, widths = c(3, 1))
    
    return(plot)
    
  } else if (effect_size %in% c("hedges_g", "g")) {
    
    combined_plotting_data <- meta_analysis_results$data %>%
      select(Participant, hedges_g, hedges_g_se, hedges_g_ci_lwr, hedges_g_ci_upr) %>%
      rbind(data.frame(Participant     = "Meta analysis",
                       hedges_g        = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[1],
                       hedges_g_se     = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[2],
                       hedges_g_ci_lwr = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[3],
                       hedges_g_ci_upr = meta_analysis_results$meta_analysed_standardized_effect_size$estimate[4])) %>%
      mutate(Participant      = fct_rev(Participant),
             size             = max(hedges_g_se*10) - hedges_g_se*10 + 1,
             hedges_g_cr_lwr  = ifelse(as.character(Participant) == "Meta analysis", CR_lwr, NA),
             hedges_g_cr_upr  = ifelse(as.character(Participant) == "Meta analysis", CR_upr, NA),
             results_string   = paste0(format(round(hedges_g, digits), nsmall = digits), " [",
                                       format(round(hedges_g_ci_lwr, digits), nsmall = digits), ", ",
                                       format(round(hedges_g_ci_upr, digits), nsmall = digits), "]"),
             results_string   = ifelse(as.character(Participant) == "Meta analysis", 
                                       paste0(results_string, "\n        [",
                                              format(round(hedges_g_cr_lwr, digits), nsmall = digits), ", ",
                                              format(round(hedges_g_cr_upr, digits), nsmall = digits), "]"),
                                       results_string))
    
    p1 <- 
      ggplot(combined_plotting_data, aes(Participant, hedges_g)) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_linerange(aes(ymin = hedges_g_ci_lwr, ymax = hedges_g_ci_upr)) +
      geom_linerange(aes(ymin = hedges_g_cr_lwr, ymax = hedges_g_cr_upr), linetype = "dotted") +
      geom_point(shape = "square", size = combined_plotting_data$size) +
      coord_flip() +
      labs(x = "Participant", y = "Hedges' g") +
      brmstools::theme_forest()
    
    p2 <- 
      ggplot(combined_plotting_data, aes(Participant, 1)) +
      geom_text(aes_string(label = "results_string"),
                hjust = "inward", 
                size = 3) +
      coord_flip() +
      theme_void() + 
      theme(panel.grid = element_blank(), panel.border = element_blank())
    
    # combine plots
    plot <- p1 + p2 + plot_layout(nrow = 1, widths = c(3, 1))
    
    return(plot)
    
  } else {
    print("effect_size must be set to 'ruscios_A', 'A', 'hedges_g', or 'g'")
  }

}


