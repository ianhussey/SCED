#' Simulate AB SCED data
#'
#' Simulate data for an AB SCED design using simstudy
#' An arbitrary number of timepoints can be simulated, and an arbitrary true effect size. An equal number of time points are created for the A and B conditions.
#' @param participants number of participants to simulate
#' @param timepoints number of timepoints to simulate for both the A and B conditions
#' @param parameters parameters of the data to be simulated (e.g., true effect size)
#' @examples
#' # True effect size of Cohen's *d* = 1.5 and *A* = 0.85.
#' parameters_d_1.5 <-
#'   defData(varname = "A", dist = "normal", formula = 0,   variance = 1, id = "idnum") %>%
#'   defData(varname = "B", dist = "normal", formula = 1.5, variance = 1, id = "idnum")
#'
#' sim <- simulate_ab_data(participants = 1000, timepoints = 15, parameters = parameters_d_1.5)

simulate_ab_data <- function(participants, timepoints, parameters) {
  # generate required number of data points using above parameters
  genData(participants*timepoints, parameters) %>%
    # create a participant variable
    mutate(participant = ceiling(idnum/timepoints)) %>%
    # create a timepoint variable
    rename(Timepoint = idnum) %>%
    group_by(participant) %>%
    mutate(Timepoint = row_number()) %>%
    ungroup() %>%
    # reshape
    gather(Condition, Score, c("A", "B")) %>%
    mutate(Timepoint = ifelse(Condition == "B", Timepoint + timepoints, Timepoint)) %>%
    arrange(participant) %>%
    mutate(participant_string = paste("Participant", participant))
}
