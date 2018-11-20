# SCED

An R package for robust analysis, plotting and meta analysis of data from A-B Single Case Experiment Designs using [exact tests](https://en.wikipedia.org/wiki/Exact_test) and [robust effect sizes](https://www.ncbi.nlm.nih.gov/pubmed/18331151).

## Author

Ian Hussey (ian.hussey@ugent.be)

## License

GPLv3+

## Installation

```R
install.packages("devtools")
library(devtools)
install_github("ianhussey/SCED")
library(SCED)
```

## Main functions 

### simulate_ab_data()

Simulate data that meets the required format of `sced_analysis()` and `sced_plot()` for testing/demonstration (via the simstudy package).

### sced_plot()

Panes represent individual participants with ID numbers included above each. Vertical dotted lines separate experimental conditions A and B (e.g., pre and post intervention). Horizontal dashed lines represent median values in each expeirmental condition. Linear regression lines and error bars are also included to higlight change trends within conditions. 

![plot](./screenshots/plot.png)

### sced_analysis()

Calculates the following statistics:

- An exact *p* value for using a non-parametric permutation test (via the coin package). These are insensitive to distributions and outliers. False positive rates are therefore equal to the alpha value, rather than approximate to it. Assesses whether the values of the data points in condition B are different to condition A.
- Median difference score. Difference score between the median value of condition A and condition B. Median values are employed given their greater robustness. Unstandardized values such as the median difference score can be useful in order to assess clinical significance, and when used in conjunction with non-parametric effect sizes (such as *A*, below), which can suffer from floor/ceiling effects.  
- Effect size *A* ([Ruscio, 2008](https://www.ncbi.nlm.nih.gov/pubmed/18331151)), a non-parametric, probabalistic effect size. Bootstrapping is used to calculated the median value and its 95% confidence intervals. This is also calculated via a permutation method, and is therefore robust/insensitive to outliers and distributions. The original, parametric versions of this test are referred to as the Area Under the Curve (AUC), the Common Language Effect Size (CLES), or the probability of superiority. *A* has an identical range (i.e. a probability between and 1) interpretation (i.e., the probability of a randomly selected time point in condition B being greater than a randomly selected time point in condition A). *A* is therefore also more robust than traditional effect size metrics for SCED such as the % overlap and the % greater than the median.
- Hedges' *g* effect sizes, and its bootstrapped 95% confidence intervals. *g* is a variation of Cohen's *d* that is bias corrected for small sample sizes. It is included here for the sake of familiarity/comparison. However, it is sensitive to the ratio of data points in A vs. B, the normality of the distributions of data in each condition, and the equality of variances of A and B. Bootstrapped CIs serves to mitigate these assumptions somewhat.

### sced_meta_analysis()

Conduct a random effects meta analysis of the robust standardized effect sizes (Ruscio's A values) from `sced_analysis()`. Produces a meta analytic effect size, 95% confidence intervals, 95% credibility intervals (also referred to as prediction intervals), measures of heterogeneity between participants, and a forest plot of the effect sizes and meta analysed effect size. 

## To do

- Change CIs in the meta analysis to using bootstrapping rather than Wald estimation

  - http://www.metafor-project.org/doku.php/tips:bootstrapping_with_ma
  - Possibly also CIs for heterogeneity metrics, see https://rdrr.io/cran/metafor/man/robust.html

- How to establish stability at baseline, either retrospectively or in an optional stopping fashion (i.e., optional condition switching once stability has been reached)?

  - Theil-Sen slope would be more robust than the OLS slopes used in the plots. However, optional-transition designs would suffer from the changing power of the test as *n* grows. All methods I know of are unrobust or have power problems. For the moment, I rely on visual inspection of the OLS linear regression line. 

- Add additional other terminology for *A* from Parker (2009). 

  ​