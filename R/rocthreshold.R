## ============================================================================
## Author:           Gregoire Thomas
## Created:          2023-05-12
## Last Modified:    2025-06-04
## Copyright (c) 2014-2024 SQU4RE  - http://www.squ4re.com/
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

## ============================================================================
#' rocthreshold
#'
#' Compute ROC sensitivity, specificity and threshold for a given cutoff.
#'
#' @inheritParams alternativeROC-common-args
#' @param x either a ROC object or the predictor to build the ROC curve   
#' @param y if x is not a ROC object, the binary outcome of the ROC curve
#' @param cut cutoff value
#' @param input cutoff type, either sensitivity, specificity, threshold, PPV or NPV  
#' @param annotate if TRUE, the full ROC curve is returned as an attribute
#' 
#' @return a vector with sensitivity, specificity, threshold, PPV and NPV at the given cutoff. 
#' 
#' @details
#' This function computes the sensitivity, specificity, threshold, PPV and NPV 
#' at a given cutoff value from a ROC curve.
#' 
#' NPV and PPV are computed only if a prevalence is provided as input.
#' 
#' If the ROC curve is not provided, it will be computed from the predictor and outcome.
#'
#' @export 
#'
rocthreshold <- function(x,y,cut,input,prevalence=NULL,annotate=FALSE) {
  ## ==========
  input <- match.arg(tolower(input),
                   c("sensitivity","specificity","threshold","npv","ppv"))
  if(inherits(x,"roc")) {
    r <- x
  } else {
    if(!is.numeric(x)) stop("x must be numeric or a ROC object")
    if(length(y)!=length(x)) stop("x and y must have the same length")
    r <- pROC::roc(y,x,quiet=TRUE)
  } 
  ## ==========
  stts <- NULL
  if(is.null(prevalence)) {
    stts <- as.data.frame(r[c("sensitivities","specificities","thresholds")])
  } else {
    stts <- as.data.frame(pvs(roc=r,prevalence))
  }
  colnames(stts) <- gsub("ties$","ty",gsub("olds$","old",colnames(stts)))
  stts <- stts[order(stts[[input]]),]
  ## ==========
  ans <- stts[c(utils::tail(which(stts[[input]]<=cut),1),
                utils::head(which(stts[[input]]>=cut),1)),]
  if(nrow(ans)>1) {
    p <- (max(ans[[input]])-cut)/diff(ans[[input]])
    ans <- apply(ans,2,function(x)max(x)-p*abs(diff(x)),
                 simplify=FALSE)
  }
  ans <- as.data.frame(ans)
  ## ==========
  if((nrow(ans)>0)&&(abs(ans[[input]]-cut)/cut>1e-2)) stop("thresholding failed")
  ## ==========
  if(annotate) attr(ans,"roc_curve") <- stts
  ans
}
## =================================================================

