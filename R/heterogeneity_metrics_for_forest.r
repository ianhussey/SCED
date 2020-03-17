#' heterogeneity_metrics_for_forest
#'
#' This function converts a metafor metaanalysis fit object to a string that contains the I^2 and H^2 heterogeneity statistics so that it can easily be added to a metafor forest plot.  
#' @param fit The meta analysis fit object outputted by sced_meta_analysis()
#' @export
#' @examples
#' # calculate participant level results
#' results <- sced_analysis(data = data)
#' 
#' # fit meta
#' sced_meta_fit <- sced_meta_analysis(results = results, effect_size = "ruscios_A")
#' 
#' # forest plot
#' metafor::forest(sced_meta_fit$model_fit,
#'                 xlab = "Probability of superiority (Ruscio's A)",
#'                 transf = SCED::logodds_to_probability,  #' convert log odds back to probabilities (ie Ruscio's A)
#'                 mlab = SCED::heterogeneity_metrics_for_forest(sced_meta_fit$model_fit),
#'                 digits = 2,
#'                 addcred = TRUE,
#'                 refline = 0.5)

heterogeneity_metrics_for_forest <- function(fit) {
  bquote(paste("RE Model (", 
               italic('I')^"2", " = ", .(formatC(format(round(fit$I2, 1), nsmall = 1))),
               "%, ", italic('H')^"2", " = ", .(formatC(format(round(fit$H2, 1), nsmall = 1))), ")"))
}
