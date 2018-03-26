#' Plot data
#'
#' Plot data from an AB design SCED experiment
#' @param data Experiment data. This must contain columns named "Participant", "Timepoint" (integer), "Score" (numeric; your DV), and "Condition" (must include only "A" and "B" as a string or factor). See the included simulated_data dataset for an example using \code{View(simulated_data)}.
#' @export
#' @examples
#' sced_plot(data = simulated_data)

sced_plot <- function(data) {
  require(tidyverse)

  data_with_condition_change <- data %>%
    group_by(Participant) %>%
    dplyr::summarize(condition_change = max(Timepoint[Condition == "A"]) + 0.5) %>%
    right_join(data, by = "Participant") %>%
    group_by(Participant, Condition) %>%
    mutate(median_score = median(Score)) %>%
    ungroup()
    
  plot <- 
    ggplot(data_with_condition_change) +
    geom_smooth(aes(x = Timepoint, y = Score, group = Condition), method = "lm", alpha = 0.15, colour = "black") +  # or loess
    geom_line(aes(x = Timepoint, y = median_score, group = Condition), linetype = "dashed") +
    geom_point(aes(x = Timepoint, y = Score, group = Condition)) +  # , color = is_median_color
    geom_line(aes(x = Timepoint, y = Score, group = Condition)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_vline(aes(xintercept = data_with_condition_change$condition_change), linetype = "dotted", color = "black") +
    theme_classic() +
    scale_color_manual(values = c("#000000", "#bc1414")) +  # second color for median highlights, to be added.
    theme(legend.position = "none") +
    facet_wrap(~Participant, ncol = 1)

  return(plot)
}
