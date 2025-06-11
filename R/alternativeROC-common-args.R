#' Arguments used across the functions of the alternativeROC package.

#' @param roc Object of class pROC.
#' @param se Sensitivity.
#' @param sp Specificity.
#' @param ppv Positive predictive value.
#' @param npv Negative predictive value.
#' @param prevalence Prevalence of the endpoint in the study population.
#' @param boot.n Number of bootstrap replicates. Default: 2000.
#' @param quantiles Quantiles. Default: c(0.5,.025,.975).
#' @param conf.level Width of the confidence interval. Default: 0.95 (i.e., 95\% CI).

#' @name alternativeROC-common-args

#' @return No return value, used for the documentation of
#' the functions of the package.
NULL
