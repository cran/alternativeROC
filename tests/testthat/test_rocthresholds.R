test_that("typical workflow",{
  ## ========================================================
  ## Check whether the approximation in rocthresholds has consequences on Sp/Se
  library(alternativeROC)
  library(pROC)
  ## ========================================================
  ppv <- .40;NPV <- .98;prevalence <- .05;n <- 1e2;pow <- 2
  rocthresholdsTest <- function(x) {
    xu <- sort(unique(x))
    ans <- (c(-Inf, xu)/2 + c(xu, +Inf)/2) 
    any(ans%in%x)
  }
  ## ========================================================
  sd <- 1e-12
  mn <- 1
  ch <- 0
  ## ========================================================
  for(seed in 1:200) {
    ## ====
    set.seed(seed)
    x <- c(stats::rnorm(n,mean=mn,sd=sd),stats::rnorm(n,mean=mn,sd=sd)^pow)
    if(seed%%2==0) x <- 2-x
    y <- rep(0:1,each=n)
    x <- x+y*runif(1)*3*sd
    roc <- pROC::roc(y,x,quiet=TRUE)
    ## ====
    ch <- ch + rocthresholdsTest(c(roc$cases,roc$controls))
    ## ====
    thresholds <- alternativeROC:::rocthresholds(c(roc$cases,roc$controls))
    p1 <- roc$fun.sesp(thresholds=thresholds, 
                       controls=roc$controls,
                       cases=roc$cases, 
                       direction=roc$direction)
    p0 <- roc$fun.sesp(thresholds=roc$thresholds, 
                       controls=roc$controls,
                       cases=roc$cases, 
                       direction=roc$direction)
    ## ====
    d <- lapply(names(p1),\(x)abs(p1[[x]]-p0[[x]]))
    testthat::expect_false(any(unlist(d)>1e-6))
    ## ====
  }  
  ## ========================================================
  testthat::expect_false(ch<20)
})
