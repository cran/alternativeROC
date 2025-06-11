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
#' Diagnostic performance: Predictive values from a ROC curve
#'
#' Diagnostic performance: Predictive values from a ROC curve
#'
#' @inheritParams alternativeROC-common-args
#' @param thresholds Thresholds of the ROC curve, default is roc$thresholds.
#' @param sensitivities Sensitivity values of the ROC curve, default is roc$sensitivities.
#' @param specificities Specificity values of the ROC curve, default is roc$specificities.
#' 
#' @details This function computes the positive and negative predictive values
#' from a ROC curve, given the prevalence of the positive outcome.
#' 
#' @return A data frame with the following columns:
#' \itemize{
#' \item{\code{threshold}: Thresholds of the ROC curve.}
#' \item{\code{sensitivity}: Sensitivity values of the ROC curve.}
#' \item{\code{specificity}: Specificity values of the ROC curve.}
#' \item{\code{prevalence}: Prevalence of the positive outcome.}
#' \item{\code{ppv}: Positive predictive value.}
#' \item{\code{npv}: Negative predictive value.}
#' }
#' 
#' @export
#'
pvs <- function(roc=NULL, prevalence,
                thresholds=roc$thresholds,
                sensitivities=roc$sensitivities,
                specificities=roc$specificities) 
  cbind(threshold=c(thresholds),
        sensitivity=sensitivities,
        specificity=specificities,
        prevalence=rep(prevalence,length(sensitivities)),
        ppv=sensitivities * prevalence /
          (sensitivities * prevalence + (1-specificities) * (1-prevalence)),
        npv=specificities * (1-prevalence) /
          ((1-sensitivities) * prevalence + specificities * (1-prevalence)))
## =========================================================================
