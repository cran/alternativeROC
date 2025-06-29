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
    x <- rnorm(n)+y*1
    ans <- rocperf(x,y,fun=fu)
    testthat::expect_true(abs(ans$AUC-ans$funauc)<1e-2)
  }
  ## ========================================================
})

