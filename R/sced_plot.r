#' Plot data
#'
#' Plot data from an AB design SCED experiment
#' @param data Experiment data
#' @examples
#' sced_plot(data = simulated_data)

sced_plot <- function(data) {
  require(tidyverse)
  require(ggplot2)

  data_with_condition_change <- data %>%
    group_by(participant) %>%
    summarize(condition_change = max(Timepoint[Condition == "A"]) + 0.5) %>%
    right_join(simulated_data, by = "participant")

  plot <-
    ggplot(data_with_condition_change) +
    geom_smooth(aes(x = Timepoint, y = Score, color = Condition), method = "lm", alpha = 0.2) +  # or loess
    geom_point(aes(x = Timepoint, y = Score, color = Condition)) +
    geom_line(aes(x = Timepoint, y = Score, color = Condition)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_vline(xintercept = data_with_condition_change$condition_change, linetype = "dashed", color = "grey") +
    theme_bw()+
    scale_color_manual(values=c("#09445a", "#cf6f77")) +
    facet_wrap(~participant_string, ncol = 1, scales = "free_y")

  return(plot)
}
