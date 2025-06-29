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

## ============================================================================
#' p[AUC=0.5]
#' 
#' p[AUC=0.5] using DeLong's methods (DeLong et al. 1988).
#'
#' @inheritParams alternativeROC-common-args
#' @param ref Reference AUC. Default 0.5.  
#' 
#' @return p value. 
#'
#' @export 
#'
p.auc <- function(roc,ref=.5) {
  n <- length(roc$controls)
  m <- length(roc$cases)
  if(m<=1||n<=1) return(NA)
  V <- delong(roc)
  SX <- sum((V$X-V$theta)*(V$X-V$theta))/(m-1)
  SY <- sum((V$Y-V$theta)*(V$Y-V$theta))/(n-1)
  S <- SX/m+SY/n
  p <- max(0,min(1,pnorm(ref,mean=V$theta,sd=sqrt(S))))
  p <- min(p,1-p)*2
  return(p)
}
## ============================================================================
