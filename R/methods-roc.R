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
#' Diagnostic performance: Specificity from sensitivity, PPV and prevalence
#'
#' Diagnostic performance: Specificity from sensitivity, positive predictive value (PPV) and prevalence of the endpoint
#'
#' @inheritParams alternativeROC-common-args
#' 
#' @return Specificity from sensitivity, PPV and prevalence.
#' 
#' @export 
#' 
ppv.spatse <- function(se,ppv,prevalence) 1-(   se *   prevalence /(1-prevalence)*(1-ppv)/   ppv )
## =========================================================================

## =========================================================================
#' Diagnostic performance: Sensitivity from specificity, NPV and prevalence
#'
#' Diagnostic performance: Sensitivity from specificity, negative predictive value (NPV) and prevalence of the endpoint
#'
#' @inheritParams alternativeROC-common-args
#' 
#' @return Sensitivity from specificity, NPV and prevalence.
#' 
#' @export 
#'
npv.seatsp <- function(sp,npv,prevalence) 1-(   sp *(1-prevalence)/   prevalence *(1-npv)/   npv )
## =========================================================================

## =========================================================================
#' Diagnostic performance: Sensitivity from specificity, PPV and prevalence
#'
#' Diagnostic performance: Sensitivity from specificity, positive predictive value (PPV) and prevalence of the endpoint
#'
#' @inheritParams alternativeROC-common-args
#' 
#' @return Sensitivity from specificity, PPV and prevalence.
#' 
#' @export 
#'
ppv.seatsp <- function(sp,ppv,prevalence)    (1-sp)*(1-prevalence)/   prevalence *   ppv /(1-ppv)
## =========================================================================

## =========================================================================
#' Diagnostic performance: Specificity from sensitivity, NPV and prevalence
#'
#' Diagnostic performance: Specificity from sensitivity, negative predictive value (NPV) and prevalence of the endpoint
#'
#' @inheritParams alternativeROC-common-args
#' 
#' @return Specificity from sensitivity, NPV and prevalence.
#' 
#' @export 
#'
npv.spatse <- function(se,npv,prevalence)    (1-se)*   prevalence /(1-prevalence)*   npv /(1-npv)
## =========================================================================

## =========================================================================
#' Diagnostic performance: Positive predictive values from a ROC curve
#'
#' Diagnostic performance: Positive predictive values (PPV) from a ROC curve
#'
#' @inheritParams alternativeROC-common-args
#' 
#' @return A matrix with the following columns:
#' \itemize{
#' \item{\code{threshold}: The thresholds used to compute the sensitivity and specificity.}
#' \item{\code{sensitivity}: The sensitivities at the threshold.}
#' \item{\code{specificity}: The specificities at the threshold.}
#' \item{\code{ppv}: The positive predictive values at the threshold.}
#' \item{\code{prevalence}: The prevalence of the endpoint in the study population, as provided in the input.}
#' }
#' 
#' @export
#'
ppv <- function(roc, prevalence)
  with(roc,cbind(threshold=c(thresholds),
               sensitivity=sensitivities,
               specificity=specificities,
               ppv=sensitivities * prevalence /
               (sensitivities * prevalence + (1-specificities) * (1-prevalence)),
               prevalence=rep(prevalence,length(sensitivities))))
## =========================================================================

## =========================================================================
#' Diagnostic performance: Negative predictive values from a ROC curve
#'
#' Diagnostic performance: Negative predictive values (NPV) from a ROC curve
#'
#' @inheritParams alternativeROC-common-args
#'
#' @return A matrix with the following columns:
#' \itemize{
#' \item{\code{threshold}: The thresholds used to compute the sensitivity and specificity.}
#' \item{\code{sensitivity}: The sensitivities at the threshold.}
#' \item{\code{specificity}: The specificities at the threshold.}
#' \item{\code{npv}: The negative predictive values at the threshold.}
#' \item{\code{prevalence}: The prevalence of the endpoint in the study population, as provided in the input.}
#' }
#'
#' @export
#'
npv <- function(roc, prevalence)
  with(roc,cbind(threshold=c(thresholds),
               sensitivity=sensitivities,
               specificity=specificities,
               npv=specificities * (1-prevalence) /
               ((1-sensitivities) * prevalence + specificities * (1-prevalence)),
               prevalence=rep(prevalence,length(sensitivities))))
## =========================================================================

## =========================================================================
## A Swiss-knife function, not ready yet.
calc.ppv <- function(se=NULL, sp=NULL, ppv=NULL, prevalence, roc=NULL) {
  if(!is.null(roc)) {
    if(!is.null(ppv)) {
      p <- pvs(roc, prevalence)
      w <- which(p[,"ppv"]>ppv)
      w <- w[1]
      p[w,]#c("sensitivity","specificity")]
    } else if(!is.null(se)) {
      p <- pvs(roc, prevalence)
      w <- which(p[,"sensitivity"]>se)
      w <- w[length(w)]
      p[w,]
    } else stop("invalid input")
  } else if(is.null( se))    (1-sp)*(1-prevalence)/   prevalence *   ppv /(1-ppv)
  else   if(is.null( sp)) 1-(   se *   prevalence /(1-prevalence)*(1-ppv)/   ppv )
  else   if(is.null(ppv)) se*prevalence / (se*prevalence + (1-sp)*(1-prevalence))
  else stop("invalid input")
}
## =========================================================================

## =========================================================================
## A Swiss-knife function, not ready yet.
calc.npv <- function(se=NULL, sp=NULL, npv=NULL, prevalence, roc=NULL) {
  if(!is.null(roc)) {
    p <- npv(roc, prevalence)
    w <- which(p[,"npv"]>npv)
    w <- rev(w)[1]
    p[w,c("sensitivity","specificity")]
  } else if(is.null( se)) 1-(   sp *(1-prevalence)/   prevalence *(1-npv)/   npv )
  else   if(is.null( sp))    (1-se)*   prevalence /(1-prevalence)*   npv /(1-npv)
  else   if(is.null(npv)) sp*(1-prevalence) / ((1-se)*prevalence + sp*(1-prevalence))
  else stop("invalid input")
}
## =========================================================================

## =========================================================================
## Odd ratio, not ready yet
diagnosticOR <- function (sp, se, prevalence)  {
  if(length(sp)!=length(se))
    stop("length of se and sp must match")
  tp <- se * prevalence
  fn <- (1-se) * prevalence
  tn <- sp * (1-prevalence)
  fp <- (1-sp) * (1-prevalence)
  ans <- (tp/fn)/(fp/tn)
  attributes(ans)$stderr.log <- sqrt(1/tp+1/fn+1/fp+1/tn)
  return(ans)
}
## =========================================================================

## =========================================================================
## Relative risk, not ready yet
diagnosticRR <- function (sp, se, prevalence)  {
  if(length(sp)!=length(se)) {
    stop("length of se and sp must match")
  }
  a <- se * prevalence
  c <- (1-se) * prevalence
  d <- sp * (1-prevalence)
  b <- (1-sp) * (1-prevalence)
  ans <- (a/(a+b))/(c/(c+d))
  attributes(ans)$OR <- a*d/b/c
  return(ans)
}
## =========================================================================

