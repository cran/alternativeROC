test_that("rocperf_fun",{
  ## ========================================================
  ## Test whether the argument fun of rocperf works
  library(alternativeROC)
  library(pROC)
  ## ========================================================
  fu <- function(controls,cases,
                 threshols,
                 sensitivities,
                 specificities,...) {
    ro <- pROC::roc(c(rep(0,length(controls)),
                      rep(1,length(cases))),
                    c(controls,cases),
                    quiet=TRUE)
    c(funauc=ro$auc)
  }
  ## =========================================================
  for(seed in 1:20) {
    set.seed(seed)
    n <- 123
    y <- runif(n)<.5
    x <- round(rnorm(n),2)
    x <- x+y*(2*(seed<10)-1)
    roc <- pROC::roc(y,x,quiet=TRUE)
    ## 
    ans <- rocperf(x,y,fun=fu,boot.n=1e2)
    testthat::expect_true(abs(ans$AUC-ans$funauc)<1e-2)
    ## 
    pe <- alternativeROC:::rocsesp(roc$controls,roc$cases,roc$direction=="<")
    th <- alternativeROC:::rocthresholds(c(roc$cases,roc$controls))
    pr <- data.frame(th=roc$thresholds,
                     se=roc$sensitivities,
                     sp=roc$specificities)
    pr <- pr[order(pr$th),]
    testthat::expect_true(all(abs(pe$th-   th)<1e-6,na.rm=TRUE))
    testthat::expect_true(all(abs(pe$th-pr$th)<1e-6,na.rm=TRUE))
    testthat::expect_true(all(abs(pe$se-pr$se)<1e-6))
    testthat::expect_true(all(abs(pe$sp-pr$sp)<1e-6))
    ## 
  }
  ## ========================================================
})

