## ============================================================================
## Author:           Gregoire Thomas
## Created:          2014-10-27
## Last Modified:    2025-06-04
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

## =========================================================================
#' Diagnostic performance: Plot ROC curve
#'
#' Diagnostic performance: Plot ROC curve
#'
#' @param x Object of class roc.
#' @param annotate Annotate plot.
#' @param col.diagonal Color of the diagonal.
#' @param lty.diagonal Line type of the diagonal.
#' @param lwd.diagonal Line width of the diagonal.
#' @param col Color.
#' @param lwd Line width.
#' @param cex Size of the symbols.
#' @param ppv Positive predictive value cutoff
#' @param npv Negative predictive value cutoff
#' @param prevalence Prevalence of the positive outcome
#' @param col.pvs Color of the predictive value triangles
#' @param col.ci Color of the positive and negative predictive values.
#' @param lwd.ci Line width for the positive and negative predictive values.
#' @param len.ci Length of the end segment for positive and negative predictive values (see arrows).
#' @param boot.n Number of bootstrap replicates for the computation of the confidence interval of the specificity at NPV and of the sensitivity at PPV.
#' @param conf.level Width of the confidence interval of the specificity at NPV and of the sensitivity at PPV.
#' @param ... parameters to be passed to plot.
#' 
#' @return A list with the following elements:
#' \itemize{
#' \item{\code{AUC}: A numeric vector of length 3 containing the median, lower bound, and upper bound of the AUC.}
#' \item{\code{PPV}: The positive predictive value cutoff.}
#' \item{\code{sensitivity@PPV}: A numeric vector of length 3 containing the median, lower bound, and upper bound of the sensitivity at the specified PPV.}
#' \item{\code{NPV}: The negative predictive value cutoff.}
#' \item{\code{specificity@NPV}: A numeric vector of length 3 containing the median, lower bound, and upper bound of the specificity at the specified NPV.}
#' }
#' 
#' @import graphics
#' @importFrom sn rsn
#' @importFrom pROC ci.auc
#' 
#' @export
#' 
#' @examples
#' set.seed(0)
#' count.neg <- count.pos <- 200
#' sd.neg <- sd.pos <- 1
#' mean.neg <- .5
#' mean.pos <- 1
#' skew.neg <- -8
#' skew.pos <- 0
#' x <- c(sn::rsn(count.neg,mean.neg,sd.neg,skew.neg),
#'        sn::rsn(count.pos,mean.pos,sd.pos,skew.pos))
#' y <- ordered(c(rep("neg",count.neg),rep("pos",count.pos)),levels=c("neg","pos"))
#' r1 <- roc(y,x)
#' plotROC(r1,ppv=0.2,prevalence=0.05,boot.n=1e2)
#' skew.neg <- 0
#' skew.pos <- 8
#' x <- c(sn::rsn(count.neg,mean.neg,sd.neg,skew.neg),
#'        sn::rsn(count.pos,mean.pos,sd.pos,skew.pos))
#' y <- ordered(c(rep("neg",count.neg),rep("pos",count.pos)),levels=c("neg","pos"))
#' r2 <- roc(y,x)
#' plotROC(r2,npv=0.995,prevalence=0.05,boot.n=1e2)
#' 
#' 
plotROC <- function (x, annotate = FALSE, col.diagonal = "#00000080",
                     lty.diagonal = "solid",
                     lwd.diagonal = 1, col = "#303030", lwd = 2, cex = 2,
                     ppv=NULL,npv=NULL,prevalence=NULL,
                     col.pvs="#888888",
                     col.ci="#dd0000",lwd.ci=lwd,len.ci=.1,
                     boot.n=1e3,conf.level=.95,
                     ...) {
  ## ======
  quantiles <- (1-conf.level)/2*c(1,0,-1)+c(0,.5,1)
  if(is.na(boot.n)||(boot.n<2)) boot.n <- NULL
  ## ======
  ans <- list()
  v <- sort(ci(x,conf.level=0.95))[c(2,1,3)]
  names(v) <- c("50%","2.5%","97.5%")
  ans$AUC <- v
  ## ======
  pvs <- pvs(x,prevalence)
  cutpoint <- NULL
  ws <- c()
  if(!is.null(ppv)) {
    w <- which(pvs[,"ppv"]>ppv-.001)
    ws <- c(ws,w[1])
  } 
  if(!is.null(npv)) {
    w <- which(pvs[,"npv"]>npv-.001)
    if(length(w)) ws <- c(ws,rev(w)[1])
  }
  if(length(ws)) cutpoint <- as.data.frame(pvs)[ws,]
  ## ======
  graphics::plot(1:0, 0:1, xlim = 1:0, xlab = "specificity", ylab = "sensitivity",
                 type = "l", col = col.diagonal, lty = lty.diagonal, lwd = lwd.diagonal,
                 ...)
  if(!is.null(ppv)) {
    s <- ppv.spatse(0:1,ppv,prevalence)
    graphics::polygon(s[c(1,1,2)],c(1,0,1),col=paste(col.pvs,"44",sep=""),border=NA)
    graphics::lines(s,0:1,col=col.pvs,lwd=1,lty="solid")
  }
  if(!is.null(npv)) {
    s <- npv.seatsp(0:1,npv,prevalence)
    graphics::polygon(c(1,0,1),s[c(1,1,2)],col=paste(col.pvs,"44",sep=""),border=NA)
    graphics::lines(0:1,s,col=col.pvs,lwd=1,lty="solid")
  }
  olend <- graphics::par()$lend
  on.exit(graphics::par(lend = olend))
  graphics::par(lend = 0)
  graphics::segments(0:5/5, rep(0, 6), 0:5/5, rep(1, 6), col = "grey",
                     lty = "dotted", lwd = lwd.diagonal)
  graphics::segments(rep(0, 6), 0:5/5, rep(1, 6), 0:5/5, col = "grey",
                     lty = "dotted", lwd = lwd.diagonal)
  graphics::par(lend = olend)
  if (!is.na(x[1])) {
    graphics::lines(x, col = col, lwd = lwd)
    if (annotate) {
      if ((length(x$cases)>3) && (length(x$controls)>3))
        graphics::mtext(side = 1, paste("", ascharacter_ciauc(pROC::ci.auc(x),
                                                         digits = 2), ""), adj = 1, line = -1.2, cex = 0.6,
                        las = 0)
      else graphics::mtext(side = 1, paste("AUC:", format(x$auc,
                                                          digits = 2, nsmall = 2), "(95% CI: N/A) ", sep = ""),
                           adj = 1, line = -1.2, cex = 0.6, las = 0)
    }
  }
  if(!is.null(cutpoint)&&is.null(boot.n)) graphics::points(cutpoint$specificity,cutpoint$sensitivity,
                                                           cex=cex,col=col,lwd=lwd)
  if(!is.null(boot.n)&&!is.null(ppv)) {
    ci <- cippv(x,ppv=ppv,prevalence=prevalence,boot.n=boot.n,quantiles=quantiles)
    ans$PPV <- ppv ; ans$`sensitivity@PPV` <- ci
    e <- ppv.spatse(ci,ppv,prevalence)
    graphics::arrows(e[1:2],ci[1:2],e[2:3],ci[2:3],col=col.ci,lwd=lwd.ci,angle=90,code=3,length=len.ci)
  }
  if(!is.null(boot.n)&&!is.null(npv)) {
    ci <- cinpv(x,npv=npv,prevalence=prevalence,boot.n=boot.n,quantiles=quantiles)
    ans$NPV <- npv ; ans$`specificity@NPV` <- ci
    e <- npv.seatsp(ci,npv,prevalence)
    graphics::arrows(ci[1:2],e[1:2],ci[2:3],e[2:3],col=col.ci,lwd=lwd.ci,angle=90,code=3,length=len.ci)
  }
  invisible(ans)
}
## =========================================================================

