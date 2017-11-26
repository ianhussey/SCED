#' Summarise sced_analysis output in a table
#'
#' Summarise output of sced_analysis in a table for printing.
#' @param results Output of \code{View(sced_analysis)}.
#' @return a data frame with formatted results
#' @export
#' @examples
#' sced_summary(sced_results)

sced_summary <- function(results) {
  require(timesavers)
  require(tidyverse)
  require(stringr)

  results %>%
    dplyr::rename(`Median difference` = median_difference) %>%
    mutate(`Ruscio's A` = paste(ruscios_A_median, " [", ruscios_A_ci_lwr, ", ", ruscios_A_ci_upr, "]", sep = ""),
           `Hedges' g` = paste(hedges_g, " [", hedges_g_ci_lwr, ", ", hedges_g_ci_upr, "]", sep = "")) %>%
    dplyr::select(Participant, `Median difference`, `Ruscio's A`, `Hedges' g`, p)
}
