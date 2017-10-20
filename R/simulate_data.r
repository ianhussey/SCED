#' Simulate AB SCED data
#'
#' Simulate data for an AB SCED design using simstudy
#' An arbitrary number of timepoints can be simulated, and an arbitrary true effect size.
#' @param participants number of participants to simulate
#' @param timepoints_a number of timepoints to simulate for condition A
#' @param timepoints_a number of timepoints to simulate for condition B
#' @param cohens_d the true effect size to simulate. Default of Cohen's d = 1.5, which corrisponds to a Ruscio's A of 0.85. NB this is stated in Cohen's d rather than Hedges' g because g is sample size dependent where d is not.
#' @export
#' @examples
#' # True effect size of Cohen's d = 1.5 / A = 0.85.
#' # simulate data using these parameters
#' sim <- simulate_ab_data(participants = 10, timepoints = 15, cohens_d = 1.5)

simulate_data <- function(participants = 10, timepoints_a = 15, timepoints_b = 15, cohens_d = 1.5) {
  require(simstudy)
  require(tidyverse)

  parameters_a <-
    defData(varname = "Score", dist = "normal", formula = 0,   variance = 1, id = "idnum")

  parameters_b <-
    defData(varname = "Score", dist = "normal", formula = cohens_d, variance = 1, id = "idnum")

  # generate required number of data points using above parameters
  data_a <- genData(participants*timepoints_a, parameters_a) %>%
    # create a participant variable
    mutate(Participant = ceiling(idnum/timepoints_a),
           Condition = "A") %>%
    # create a timepoint variable
    rename(Timepoint = idnum) %>%
    group_by(Participant) %>%
    mutate(Timepoint = row_number()) %>%
    ungroup()

  data_b <- genData(participants*timepoints_b, parameters_b) %>%
    # create a participant variable
    mutate(Participant = ceiling(idnum/timepoints_b),
           Condition = "B") %>%
    # create a timepoint variable
    rename(Timepoint = idnum) %>%
    group_by(Participant) %>%
    mutate(Timepoint = row_number() + timepoints_a) %>%
    ungroup()

  data <- rbind(data_a, data_b) %>%
    mutate(Score = round(Score, 2)) %>%
    arrange(Participant, Condition, Timepoint)

  return(data)
}
