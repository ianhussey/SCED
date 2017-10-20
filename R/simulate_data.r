#' Simulate AB SCED data
#'
#' Simulate data for an AB SCED design using simstudy
#' An arbitrary number of timepoints can be simulated, and an arbitrary true effect size. An equal number of time points are created for the A and B conditions.
#' @param participants number of participants to simulate
#' @param timepoints number of timepoints to simulate for both the A and B conditions
#' @param cohens_d the true effect size to simulate. Default of Cohen's d = 1.5, which corrisponds to a Ruscio's A of 0.85. NB this is stated in Cohen's d rather than Hedges' g because g is sample size dependent where d is not.
#' @export
#' @examples
#' # True effect size of Cohen's d = 1.5 / A = 0.85.
#' # simulate data using these parameters
#' sim <- simulate_ab_data(participants = 10, timepoints = 15, cohens_d = 1.5)

simulate_data <- function(participants = 10, timepoints = 15, cohens_d = 1.5) {
  require(simstudy)
  require(tidyverse)

  parameters <-
    defData(varname = "A", dist = "normal", formula = 0,   variance = 1, id = "idnum") %>%
    defData(varname = "B", dist = "normal", formula = cohens_d, variance = 1, id = "idnum")

  # generate required number of data points using above parameters
  genData(participants*timepoints, parameters) %>%
    # create a participant variable
    mutate(Participant = ceiling(idnum/timepoints)) %>%
    # create a timepoint variable
    rename(Timepoint = idnum) %>%
    group_by(Participant) %>%
    mutate(Timepoint = row_number()) %>%
    ungroup() %>%
    # reshape
    gather(Condition, Score, c("A", "B")) %>%
    mutate(Timepoint = ifelse(Condition == "B", Timepoint + timepoints, Timepoint)) %>%
    arrange(Participant)
}
