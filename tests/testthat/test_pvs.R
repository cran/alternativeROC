test_that("typical workflow",{
  ## ========================================================
  library(alternativeROC)
  library(pROC)
  ## ========================================================
  ppv <- .40;NPV <- .98;prevalence <- .05;n <- 1e2;pow <- 2
  x <- c(stats::rnorm(n),stats::rnorm(n)^pow)
  y <- rep(0:1,each=n)
  roc <- pROC::roc(y,x,quiet=TRUE)
  graphics::boxplot(split(x,y))
  plotROC(roc,prevalence=prevalence,ppv=ppv,npv=NPV)
  
  pf <- as.data.frame(pvs(roc,prevalence=prevalence))
  testthat::expect_false(any(abs(pf$sensitivity-ppv.seatsp(pf$specificity,pf$ppv,prevalence))>1e-3,na.rm=TRUE))
  testthat::expect_false(any(abs(pf$sensitivity-npv.seatsp(pf$specificity,pf$npv,prevalence))>1e-3,na.rm=TRUE)) 
  testthat::expect_false(any(abs(pf$specificity-ppv.spatse(pf$sensitivity,pf$ppv,prevalence))>1e-3,na.rm=TRUE)) 
  testthat::expect_false(any(abs(pf$specificity-npv.spatse(pf$sensitivity,pf$npv,prevalence))>1e-3,na.rm=TRUE)) 
  
  fu <- function(x,...)x
  res <- list(se=alternativeROC:::stratifiedcippv(roc,ppv,prevalence,fu),
              sp=alternativeROC:::stratifiedcinpv(roc,NPV,prevalence,fu))
  ret <- list(se=alternativeROC:::stratifiedcippv(roc,ppv-.01,prevalence,fu),
              sp=alternativeROC:::stratifiedcinpv(roc,NPV-.01,prevalence,fu))
  
  graphics::abline(h=res$se,col="blue",lty="dotted")
  graphics::abline(v=res$sp,col="blue",lty="dotted")
  graphics::lines(pf$specificity,pf$npv,col="blue",lty="dashed")
  graphics::lines(pf$ppv,pf$sensitivity,col="blue",lty="dashed")
  ## ========================================================
})
