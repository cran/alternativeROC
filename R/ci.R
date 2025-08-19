## ============================================================================
## Author:           Gregoire Thomas
## Created:          2014-10-27
## Last Modified:    2025-06-04
## Copyright (c) 2014-2025 SQU4RE  - http://www.squ4re.com/
## 
## This program is free software: you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the Free 
## Software Foundation, either version 3 of the License, or (at your option) 
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ============================================================================

## =========================================================================
#' Diagnostic performance: Confidence interval for positive predictive value
#' 
#' This function computes a confidence interval for the positive predictive value
#' from a ROC curve, given the prevalence of the positive outcome.
#' 
#' @details
#' This function computes a confidence interval for the positive predictive value
#' from a ROC curve, given the prevalence of the positive outcome.
#' The confidence interval is computed using bootstrap resampling.
#' 
#' @inheritParams alternativeROC-common-args
#' @param ... Not used.
#' 
#' @return A numeric vector of length 3 containing the median, lower bound, and 
#' upper bound of the confidence interval.
#' 
#' @importFrom plyr rdply
#' 
#' @export 
#'
cippv <- function(roc,ppv,prevalence,boot.n=2000,quantiles=c(0.5,.025,.975),...)
  stats::quantile(plyr::rdply(boot.n,stratifiedcippv(roc,ppv,prevalence,replace=TRUE))[,2],quantiles)
## ======
stratifiedcippv <- function (roc,ppv,prevalence,FUN=base::sample,...) {
  controls <- FUN(roc$controls,...)
  cases <- FUN(roc$cases,...)
  perfs <- rocsesp(controls=controls,cases=cases,decreasing=roc$direction=="<")
  p <- perfs$se*prevalence/(perfs$se*prevalence+(1-perfs$sp)*(1-prevalence))
  return(ifelse(all(p<ppv,na.rm=TRUE),0,max(perfs$se[p>ppv],na.rm=TRUE)))
}
## =========================================================================

## =========================================================================
#' Diagnostic performance: Confidence interval for negative predictive value
#' 
#' This function computes a confidence interval for the negative predictive value
#' from a ROC curve, given the prevalence of the negative outcome.
#' 
#' @details
#' This function computes a confidence interval for the negative predictive value
#' from a ROC curve, given the prevalence of the negative outcome.
#' The confidence interval is computed using bootstrap resampling.
#' 
#' @inheritParams alternativeROC-common-args
#' @param ... Not used.
#' 
#' @return A numeric vector of length 3 containing the median, lower bound, and
#' upper bound of the confidence interval.
#' 
#' @importFrom plyr rdply
#' 
#' @export 
#'
cinpv <- function(roc,npv,prevalence,boot.n,quantiles=c(0.5,.025,.975),...)
  stats::quantile(plyr::rdply(boot.n,stratifiedcinpv(roc,npv,prevalence,replace=TRUE))[,2],quantiles)
## ======
stratifiedcinpv <- function (roc,npv,prevalence,FUN=base::sample,...) {
  controls <- FUN(roc$controls,...)
  cases <- FUN(roc$cases, ...)
  perfs <- rocsesp(controls=controls,cases=cases,decreasing=roc$direction=="<")
  n <- perfs$sp*(1-prevalence)/((1-perfs$se)*prevalence+perfs$sp*(1-prevalence))
  return(ifelse(all(n<npv,na.rm=TRUE),0,max(perfs$sp[n>npv],na.rm=TRUE)))
}
## =========================================================================


## =========================================================================
## Diagnostic performance: Format AUC
## 
## Diagnostic performance: Format AUC
## 
## @param x Object of class ci.auc.
## @param digits Precision.
## @param show.method Show method.
## @param ... Not used.
## 
## @details
## This function formats the AUC value and its confidence interval for display.
## @return A character string representing the formatted AUC value and its confidence interval.
## @export
##
ascharacter_ciauc <- function(x, digits = max(3, getOption("digits") - 3),
                                show.method=FALSE,...) {
  signif.ci <- format(x, digits = digits, nsmall = digits)
  ans <- ""
  ans <- paste(ans, "AUC:",signif.ci[2],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), sep = "")
  ans <- paste(ans, " (", attr(x, "conf.level") * 100, "% CI: ", sep = "")
  ans <- paste(ans, signif.ci[1],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), "-",
               signif.ci[3],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), sep = "")
  if(show.method) {
    if (attr(x, "method") == "delong")
      ans <- paste(ans, ", DeLong)", sep = "")
    else
      ans <- paste(ans,", ", attr(x, "boot.n"), " ",
                   ifelse(attr(x, "boot.stratified"), "stratified", "non-stratified"),
                   " bootstrap replicates)", sep = "")
  } else ans <- paste(ans,")",sep="")
  ans
}
## =========================================================================

## =========================================================================
## Diagnostic performance: Format specificity
## 
## Diagnostic performance: Format specificity
## 
## @param x Object of class ci.sp.
## @param digits Precision.
## @param show.method Show method.
## @param ... Not used.
## 
## @details
## This function formats the specificity and its confidence interval for display.
## @return A character string representing the formatted specificity and its confidence interval.
## @export
##
ascharacter_cisp <- function(x, digits = max(3, getOption("digits") - 3), 
                               show.method=FALSE,...) {
  signif.ci <- signif(x, digits = digits)
  ans <- ""
  ans <- paste(ans, "sens.:", signif(attr(x, "sensitivities"), digits = digits), sep="")
  ans <- paste(ans, " spec.:",signif.ci[,2], sep="")
  ans <- paste(ans, " (", attr(x, "conf.level") * 100, "% CI: ", sep = "")
  ans <- paste(ans, signif.ci[,1],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), "-",
               signif.ci[,3],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), sep = "")
  if(show.method) {
    #if (attr(x, "method") == "delong")
    #  ans <- paste(ans, ", DeLong)", sep = "")
    #else
    ans <- paste(ans,", ", attr(x, "boot.n"), " ",
                 ifelse(attr(x, "boot.stratified"), "stratified", "non-stratified"),
                 " bootstrap replicates)", sep = "")
  } else ans <- paste(ans,")",sep="")
  ans
}
## =========================================================================

## =========================================================================
## Diagnostic performance: Format sensitivity
## 
## Diagnostic performance: Format sensitivity
## 
## @param x Object of class ci.se.
## @param digits Precision.
## @param show.method Show method.
## @param ... Not used.
## 
## @details
## This function formats the sensitivity and its confidence interval for display.
## @return A character string representing the formatted sensitivity and its confidence interval.
## @export
##
ascharacter_cise <- function(x, digits = max(3, getOption("digits") - 3), 
                               show.method=FALSE,...) {
  signif.ci <- signif(x, digits = digits)
  ans <- ""
  ans <- paste(ans, "spec.:", signif(attr(x, "specificities"), digits = digits), sep="")
  ans <- paste(ans, " sens.:",signif.ci[,2], sep="")
  ans <- paste(ans, " (", attr(x, "conf.level") * 100, "% CI: ", sep = "")
  ans <- paste(ans, signif.ci[,1],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), "-",
               signif.ci[,3],
               ifelse(attr(attr(x, "auc"), "percent"), "%", ""), sep = "")
  if(show.method) {
    #if (attr(x, "method") == "delong")
    #  ans <- paste(ans, ", DeLong)", sep = "")
    #else
    ans <- paste(ans,", ", attr(x, "boot.n"), " ",
                 ifelse(attr(x, "boot.stratified"), "stratified", "non-stratified"),
                 " bootstrap replicates)", sep = "")
  } else ans <- paste(ans,")",sep="")
  ans
}
## =========================================================================

