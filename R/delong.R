delong <- function(roc) {
  ans <- delongPlacementsCpp(roc)
  if(abs(ans$theta-roc$auc)>1e-3) stop(sprintf("internal error [delong.cpp]"))
  ans
}
