## ============================================================================
## Author:           Gregoire Thomas
## Created:          2025-06-10
## Last Modified:    2025-06-10
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
## Compute threshold, sensitivity and specificity of a ROC curve
## Example: rocsesp(roc$controls,roc$cases,roc$direction=="<")
rocsesp <- function(controls,cases,decreasing) {
  x <- c(controls, cases)
  y <- c(rep(0, length(controls)), rep(1, length(cases)))
  o <- order(x, decreasing = decreasing)
  xs <- x[o]
  ys <- y[o]
  se <- cumsum(ys == 1) / length(cases)
  sp <- 1 - cumsum(ys == 0) / length(controls)
  ## ---------
  w <- c(xs[-1]==xs[-length(xs)],FALSE)
  if(any(c(se[-1]==se[-length(se)]) & c(sp[-1]==sp[-length(sp)])))
    stop("Some thresholds have the same sensitivity and specificity")
  ans <- data.frame(
    se=c(0, se[!w]),
    sp=c(1, sp[!w]))
  if(decreasing) {
    ans$th <- (c(+Inf, xs[!w]) + c(xs[!w], -Inf))/2
  } else {
    ans$th <- (c(-Inf, xs[!w]) + c(xs[!w], +Inf))/2
  }
  return(ans[order(ans$th),])
}
## ============================================================================
