test_that("rocperf",{
  ## ========================================================
  ## Test whether getting the same results as pROC::ci.coords
  library(alternativeROC)
  library(pROC)
  ## ========================================================
  dp <- FALSE
  if(dp) par(mfrow=c(2,2))
  for(seed in 1:3) {
    cat("*")
    set.seed(seed)
    count.neg <- round(runif(1,10,200))
    count.pos <- round(runif(1,10,200))
    sd.neg <- sd.pos <- 1
    mean.neg <- runif(1)
    mean.pos <- 1
    skew.neg <- runif(1,-10,10)
    skew.pos <- 0
    x <- c(sn::rsn(count.neg,mean.neg,sd.neg,skew.neg),
           sn::rsn(count.pos,mean.pos,sd.pos,skew.pos))
    y <- ordered(c(rep("neg",count.neg),rep("pos",count.pos)),levels=c("neg","pos"))
    cu <- c(.5,.75,.9)
    ro <- roc(y,x,quiet=TRUE)
    pe1 <- rocperf(x,y,sensitivities=cu,specificities=cu)
    pe2 <- list(`Sp@Se`=ci.coords(ro,cu,"sensitivity","specificity",progress="none",best.method="closest.topleft"),
                `Se@Sp`=ci.coords(ro,cu,"specificity","sensitivity",progress="none",best.method="closest.topleft"))
    a <- c()
    for(n in names(pe2)) {
      if(dp) plot(ro,col="white",main=seed)
      v <- pe2[[n]][[attr(pe2[[n]],"ret")]]
      for(i in 1:nrow(v)) {
        fs <- sprintf("%s%.0f%s",n,cu[i]*100,c(".lCI","",".uCI"))
        for(j in 1:3) a[fs[j]] <- v[i,j]
        if(dp) if(n=="Se@Sp") {
          points(cu[i],v[i,2],pch=19,cex=2,col="cyan")
          segments(cu[i],v[i,1],cu[i],v[i,3],lwd=5,col="cyan")
          points(cu[i],pe1[[fs[2]]],pch=19,cex=1,col="red")
          segments(cu[i],pe1[[fs[1]]],cu[i],pe1[[fs[3]]],lwd=1,col="red")
        } else {
          points(v[i,2],cu[i],pch=19,cex=2,col="cyan")
          segments(v[i,1],cu[i],v[i,3],cu[i],lwd=5,col="cyan")
          points(pe1[[fs[2]]],cu[i],pch=19,cex=1,col="red")
          segments(pe1[[fs[1]]],cu[i],pe1[[fs[3]]],cu[i],lwd=1,col="red")
        }
      }
      if(dp) lines(ro,col="#88888888")
    }
    n <- names(a)[!grepl("CI$",names(a))]
    to <- 2/(min(count.neg,count.pos)-1)
    d <- max(abs(unlist(pe1[n])-a[n]))
    testthat::expect_false((d>to)&&(d>1e-2))
  }
  ## ========================================================
})

