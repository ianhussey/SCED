#' Plot data
#'
#' Plot data from an AB design SCED experiment, reordering the participants by the order in which they change from A to B condition.
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @export
#' @examples
#' sced_plot(data = simulated_data)

sced_plot <- function(data, show_MAD_interval = FALSE) {
  require(tidyverse)
  require(forcats)
  
  data_with_condition_change <- data %>% 
    dplyr::group_by(Participant) %>% 
    dplyr::summarize(condition_change = max(Timepoint[Condition == "A"], na.rm = TRUE) + 0.5) %>% 
    dplyr::right_join(data, by = "Participant") %>% 
    dplyr::group_by(Participant, Condition) %>% 
    mutate(median_score = median(Score, na.rm = TRUE),
           mad = mad(Score, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  intervention_point <- data %>% 
    dplyr::filter(Condition == "B") %>% 
    dplyr::group_by(Participant) %>% 
    dplyr::summarize(intervention_point = min(Timepoint, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  plot_data <- data_with_condition_change %>% 
    dplyr::left_join(intervention_point, by = "Participant") %>% 
    dplyr::mutate(Participant = as.factor(Participant),
                  Participant = forcats::fct_reorder(Participant, intervention_point))
  
  if(show_MAD_interval == TRUE){
    plot <- ggplot(plot_data) + 
      geom_ribbon(aes(x = Timepoint, ymin = median_score - mad, ymax = median_score + mad, group = Condition), 
                  fill = "lightgrey") +
      geom_smooth(aes(x = Timepoint, y = Score, group = Condition),
                  method = "lm", alpha = 0.0, colour = "black", size = 0.5) +
      geom_line(aes(x = Timepoint, y = median_score, group = Condition), linetype = "dashed") + 
      geom_point(aes(x = Timepoint, y = Score, group = Condition)) + 
      geom_line(aes(x = Timepoint, y = Score, group = Condition)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
      geom_vline(aes(xintercept = condition_change), linetype = "dotted", 
                 color = "black") + theme_classic() + scale_color_manual(values = c("#000000", "#bc1414")) + 
      theme(legend.position = "none") + 
      facet_wrap(~Participant, ncol = 1) +
      ylab("Score") 
  } else {
    plot <- ggplot(plot_data) + 
      geom_smooth(aes(x = Timepoint, y = Score, group = Condition),
                  method = "lm", alpha = 0.0, colour = "black", size = 0.5) +
      geom_line(aes(x = Timepoint, y = median_score, group = Condition), linetype = "dashed") + 
      geom_point(aes(x = Timepoint, y = Score, group = Condition)) + 
      geom_line(aes(x = Timepoint, y = Score, group = Condition)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
      geom_vline(aes(xintercept = condition_change), linetype = "dotted", 
                 color = "black") + theme_classic() + scale_color_manual(values = c("#000000", "#bc1414")) + 
      theme(legend.position = "none") + 
      facet_wrap(~Participant, ncol = 1) +
      ylab("Score") 
  }
  
  return(plot)
}
