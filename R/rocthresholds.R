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
# #' Compute thresholds of a ROC curve
# #' 
# #' This function computes the thresholds of a ROC curve given a vector of
# #' scores. The thresholds are defined as the midpoints between unique scores.
# #' 
# #' @param x A numeric vector of scores.
# #' @return A numeric vector of thresholds.
# #' @details The function computes the unique scores from the input vector,
# #' sorts them, and then calculates the midpoints between consecutive unique
# #' scores. The first threshold is set to negative infinity and the last to
# #' positive infinity. Thresholds may contain input values if differences 
# #' between consecutive thresholds are small. 
# #' @export
rocthresholds <- function(x) {
  xu <- sort(unique(x))
  ans <- (c(-Inf, xu)/2 + c(xu, +Inf)/2) 
  return(ans)
}
## ============================================================================
