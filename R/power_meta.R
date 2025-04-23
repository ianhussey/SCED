#' A priori power analysis for meta analysis
#'
#' Rough approximation of power for the between participant meta analysis of effects, using equations derived from Valentine, Pigott, & Rothstein (2010, doi: 10.3102/1076998609346961), derived by Quintana (2017: https://towardsdatascience.com/how-to-calculate-statistical-power-for-your-meta-analysis-e108ee586ae8), and applied to within-subject SCED designs here.
#' @param yi Meta-analyzed hedges' g effect size. NBs 1) Although Cohen's rules of thumb (.2 = small, .5 = medium, .8 = large), larger effect sizes are often observed in SCED due to the within subject designs (e.g., above 1.0). 2) Valentine's equations assume a between-groups design but here we apply to within-subjects effect sizes. Power is therefore likely to be underestimated. 3) Valentine's equations employ Cohen's d effect size but we use the Hedges' g effect size which has been bias corrected for small samples. 
#' @param ni The average number of data points per phase (i.e., in each of A and B, before or after the intervention). Assuming equal number of total time points for all participants, this is roughly equal to half the total number of time points. NB again, Valentine's equations assume a between-groups design but here we apply to within-subjects effect sizes. Power is therefore likely to be underestimated.
#' @param k The number of participants.
#' @param tau2 The tau^2 metric of between participant heterogeneity, as produced by the meta analysis function. If data from previous studies is not available, historically reccommended cutoffs are .33 for small, 1.0 for moderate, & 3.0 for large.
#' @return Statistical power (i.e., 1 - Beta), the probability of observing a significant result given the parameters. 
#' @export
#' @examples
#' \dontrun{
#' power_meta(yi = 0.5, 
#'            ni = 25,
#'            k  = 6,
#'            tau2 = 1)
#' }

power_meta <- function(yi, ni, k, tau2){
  
  eq1 <- ((ni + ni)/((ni)*(ni))) + ((yi^2)/(2*(ni + ni)))
  eq2 <- tau2*(eq1)
  eq3 <- eq2+eq1
  eq4 <- eq3/k
  eq5 <- (yi/sqrt(eq4))
  power <- (1 - pnorm(1.96 - eq5)) # two-tailed
  
  return(power)
}
