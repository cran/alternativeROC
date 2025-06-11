# pROC: Tools Receiver operating characteristic (ROC curves) with
# (partial) area under the curve, confidence intervals and comparison. 
# Copyright (C) 2010-2014 Xavier Robin, Alexandre Hainard, Natacha Turck,
# Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez
# and Markus Müller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Helper functions for the ROC curves. These functions should not be called directly as they peform very specific tasks and do nearly no argument validity checks. Not documented in RD and not exported.

# return the thresholds to evaluate in the ROC curve, given the 'predictor' values. Returns all unique values of 'predictor' plus 2 extreme values
#
rocutilsthresholds <- function(predictor, direction) {
  unique.candidates <- sort(unique(predictor))
  thresholds1 <- (c(-Inf, unique.candidates) + c(unique.candidates, +Inf))/2
  thresholds2 <- (c(-Inf, unique.candidates)/2 + c(unique.candidates, +Inf)/2)
  thresholds <- ifelse(abs(thresholds1) > 1e100, thresholds2, thresholds1)
  if (any(ties <- thresholds %in% predictor)) {
    # If we get here, some thresholds are identical to the predictor
    # This is caused by near numeric ties that caused the mean to equal
    # one of the candidate
    # We need to make sure we select the right threshold more carefully
    if (direction == '>') {
      # We have:
      # tp <- sum(cases <= threshold)
      # tn <- sum(controls > threshold)
      # We need to make sure the selected threshold
      # Corresponds to the lowest observation of the predictor
      # Identify problematic thresholds
      # rows <- which(ties)
      for (tie.idx in which(ties)) {
        if (thresholds[tie.idx] == unique.candidates[tie.idx - 1]) {
          # We're already good, nothing to do
        }
        else if (thresholds[tie.idx] == unique.candidates[tie.idx]) {
          thresholds[tie.idx] <- unique.candidates[tie.idx - 1]
        }
        else {
          sessionInfo <- sessionInfo()
          ## removed by GT 2025-06-06: save debug info to file
          stop(sprintf("Couldn't fix near ties in thresholds: %s, %s, %s, %s.", thresholds[tie.idx], unique.candidates[tie.idx - 1], unique.candidates[tie.idx], direction))
        }
      }
    }
    else if (direction == '<') {
      # We have:
      # tp <- sum(cases >= threshold)
      # tn <- sum(controls < threshold)
      # We need to make sure the selected threshold
      # Corresponds to the highest observation of the predictor
      # Identify the problematic thresholds:
      # rows <- which(apply(o, 1, any))
      for (tie.idx in which(ties)) {
        if (thresholds[tie.idx] == unique.candidates[tie.idx - 1]) {
          # Easy to fix: should be unique.candidates[tie.idx]
          thresholds[tie.idx] <- unique.candidates[tie.idx]
        } else if (thresholds[tie.idx] == unique.candidates[tie.idx]) {
          # We're already good, nothing to do
        }
        else {
          sessionInfo <- sessionInfo()
          ## removed by GT 2025-06-06: save debug info to file
          stop(sprintf("Couldn't fix near ties in thresholds: %s, %s, %s, %s.", thresholds[tie.idx], unique.candidates[tie.idx - 1], unique.candidates[tie.idx], direction))
        }
      }
    }
  }
  return(thresholds)
}
