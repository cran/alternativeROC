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


# Calls delongPlacementsCpp safely
# Ensures that the theta value calculated is correct
delongPlacements <- function(roc) {
  placements <- delongPlacementsCpp(roc)
  
  # Ensure theta equals auc
  auc <- roc$auc / ifelse(roc$percent, 100, 1)
  if (! isTRUE(all.equal(placements$theta, auc))) {
    sessionInfo <- sessionInfo()
    ## removed by GT 2025-06-06: save debug info to file
    stop(sprintf("pROC: error in calculating DeLong's theta: got %.20f instead of %.20f.", placements$theta, auc))
  }
  
  return(placements)
}
