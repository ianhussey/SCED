# SCED

An R package for robust analysis, plotting and meta analysis of data from A-B Single Case Experiment Designs using [exact tests](https://en.wikipedia.org/wiki/Exact_test), [robust effect sizes](https://www.ncbi.nlm.nih.gov/pubmed/18331151) and meta analysis of these effect sizes.

## Author

Ian Hussey (ian.hussey -at- unibe.ch)

## License

[GPLv3+](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3))

You may use, copy, distribute and modify this software, as long as you (a) track any changes or additions you make to it (e.g., by submitting pull requests) and (b) make your changes or additions available to others.

## Installation

```R
install.packages("devtools")
library(devtools)
install_github("ianhussey/SCED")
library(SCED)
```

## Overview

See the vignette for an examle workflow (vignettes/vignette.Rmd) 

## Main functions 

### simulate_ab_data()

Simulate data that meets the required format of `sced_analysis()` and `sced_plot()` for testing/demonstration (via the simstudy package).

### sced_plot()

Panes represent individual participants with ID numbers included above each. Vertical dotted lines separate experimental conditions A and B (e.g., pre and post intervention). Horizontal dashed lines represent median values in each expeirmental condition. 

![plot](./screenshots/plot.png)

### sced_analysis()

Calculates the following statistics:

- An exact *p* value for using a non-parametric permutation test (via the coin package). These are insensitive to distributions and outliers. False positive rates are therefore equal to the alpha value, rather than approximate to it. Assesses whether the values of the data points in condition B are different to condition A.
- Median difference score. Difference score between the median value of condition A and condition B. Median values are employed given their greater robustness. Unstandardized values such as the median difference score can be useful in order to assess clinical significance, and when used in conjunction with non-parametric effect sizes (such as *A*, below), which can suffer from floor/ceiling effects.  
- Effect size *A* ([Ruscio, 2008](https://www.ncbi.nlm.nih.gov/pubmed/18331151)), a non-parametric, probabalistic effect size. Bootstrapping is used to calculated the median value and its 95% confidence intervals. This is also calculated via a permutation method, and is therefore robust/insensitive to outliers and distributions. The original, parametric versions of this test are referred to as the Area Under the Curve (AUC), the Common Language Effect Size (CLES), or the probability of superiority. *A* has an identical range (i.e. a probability between and 1) interpretation (i.e., the probability of a randomly selected time point in condition B being greater than a randomly selected time point in condition A). *A* is therefore also more robust than traditional effect size metrics for SCED such as the % overlap and the % greater than the median.
- Hedges' *g* effect sizes, and its bootstrapped 95% confidence intervals. *g* is a variation of Cohen's *d* that is bias corrected for small sample sizes. It is included here for the sake of familiarity/comparison. However, it is sensitive to the ratio of data points in A vs. B, the normality of the distributions of data in each condition, and the equality of variances of A and B. Bootstrapped CIs serves to mitigate these assumptions somewhat.

### sced_meta_analysis()

Conduct a random effects meta analysis of the Standardized Mean Differences (Hedge's *g* values) from `sced_analysis()`. Produces a meta analytic effect size, 95% Confidence Intervals, 95% Prediction Intervals (which take between participant heterogeneity into account), measures of heterogeneity between participants. Optionally, you can produce a forest plot from the output of the function. 

### power_meta()

Rough *a priori* power analysis for SCED's between subjects meta analysis based on the equations derived from Valentine et al. (2009) by Quintana ([2017](https://towardsdatascience.com/how-to-calculate-statistical-power-for-your-meta-analysis-e108ee586ae8)). Some approximations are made: e.g., Valentine et al assume a between groups design and the use of Cohen's *d* over the SCED package's Hedge's *g*. The function therefore likely underestimates power by a small degree. Note that power analysis should not be used in isolation to determine sample size: number of participants, timepoints, etc should also take methodological recommendations for SCEDs into account. 

## To do

- Add additional other terminology for *A* from Parker (2009), and relate A to tau.
- Possibly implement tau effect size as it controls for baseline trends. 
- Elaborate the preprint describing the package and methods.

## Changelog

- 0.2
	- Removed meta-analysis of Ruscio's A effect size. There are too many ways for this to go wrong, and too many compromises to be made (e.g., linear probability model has prediction problems, zero-one-inflated Beta regression is overly complex and probably requires a switch to Bayesian, logit transformations have problems with values at 0 or 1 or with no variance, etc). 
	- Removed the adjust_ceiling options from sced_analysis, as this was an attempted solution to the problem of 0 or 1 values of Ruscio's A.

  
