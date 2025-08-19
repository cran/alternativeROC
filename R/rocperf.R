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

## ============================================================================
#' ROC curve performances
#' 
#' Range of statistics associated with a ROC curve with confidence interval 
#' where applicable. This function is faster than the alternatives provided by
#' the package pROC. 
#'
#' @inheritParams alternativeROC-common-args
#' @param x Numeric vector containing the predicted value for each observation. 
#' @param y Factor, numeric, logical or character vector encoding the response. 
#' @param sensitivities Vector of sensitivity thresholds. Default NULL. 
#' @param specificities Vector of specificity thresholds. Default NULL.  
#' @param fun Function to compute additional statistics. Default NULL.
#' @param seed Random seed for bootstrapping. Default 1.  
#' @param boot.n Number of bootstrap samples. Default 2e3.
#' @param median If TRUE, return median bootstrap sensitivities and specificities, otherwise return observed values, otherwise the observe value is provided. Default FALSE.
#' @param attr Return bootstrap results and ROC curve as attributes. Default FALSE.
#' @param parallel Parallelise bootstrap. Default FALSE.
#' @param simplify If TRUE, return only median for results of the function \code{fun} having one value across bootstraps. Default TRUE.
#' @param ... Additional arguments passed to \code{fun} if not NULL.
#' 
#' @details
#' This function computes the area under the ROC curve (AUC) and its confidence
#' interval, the Mann-Whitney U test p-value, and the p-value for the null
#' hypothesis that the AUC is equal to 0.5 (DeLong et al. 1988).
#' 
#' The function also computes the sensitivity at specified specificities and
#' the specificity at specified sensitivities, with confidence intervals and
#' interquartile ranges if bootstrapping is performed.
#' 
#' The function uses the \code{pROC} package to compute the ROC curve and
#' confidence intervals, and it can handle parallel processing for bootstrapping.
#' 
#' The function returns a data frame with the computed statistics, including: 
#' \itemize{
#' \item Number of control and case patients 
#' \item Mann Whitney U test p-value 
#' \item AUC and its confidence intervals 
#' \item Sensitivity at specified specificities and their confidence intervals 
#' \item Specificity at specified sensitivities and their confidence intervals 
#' }
#' 
#' The function \code{fun} must take the following arguments:
#' \itemize{
#' \item \code{controls}: vector of control values
#' \item \code{cases}: vector of case values
#' \item \code{thresholds}: vector of thresholds used for the ROC curve
#' \item \code{sensitivities}: vector of sensitivities
#' \item \code{specificities}: vector of specificities
#' \item \code{...}: additional arguments
#' }
#' and return a named vector of values.
#' 
#' @examplesIf interactive() 
#' fu <- function(controls,cases,thr,se,sp,...) {
#'   r <- pROC::roc(c(rep(0,length(controls)),
#'                    rep(1,length(cases))),
#'                  c(controls,cases),
#'                  quiet=TRUE)
#'   c(AUC_fun=r$auc) 
#' }
#' set.seed(1)
#' n <- 123
#' y <- runif(n)<.5
#' x <- rnorm(n)+y*1
#' ans <- rocperf(x,y,fun=fu)
#' ans <- rocperf(x,y,fun=fu,
#'                senitivities=c(.5,.75,.9),
#'                specificities=c(.5,.75,.9))
#'                
#' @return A data frame with the following columns:
#' \itemize{
#' \item{\code{n.control}: Number of control patients}
#' \item{\code{n.case}: Number of case patients}
#' \item{\code{MannWhitney.pvalue}: Mann Whitney U test p-value}
#' \item{\code{AUC.pvalue}: p-value for the null hypothesis that AUC=0.5}
#' \item{\code{AUC}: Area under the ROC curve (point estimate)}
#' \item{\code{AUC.lCI}: Lower limit of 95\% confidence interval for AUC}
#' \item{\code{AUC.uCI}: Upper limit of 95\% confidence interval for AUC}
#' \item{\code{AUC.lQuart}: Lower limit of 50\% confidence interval for AUC}
#' \item{\code{AUC.uQuart}: Upper limit of 50\% confidence interval for AUC}
#' \item{\code{Se@SpX}: Sensitivity at X\% specificity}
#' \item{\code{Se@SpX.lCI}: Lower limit of 95\% confidence interval for sensitivity at X\% specificity}
#' \item{\code{Se@SpX.uCI}: Upper limit of 95\% confidence interval for sensitivity at X\% specificity}
#' \item{\code{Se@SpX.lQuart}: Lower limit of 95\% confidence interval for sensitivity at X\% specificity}
#' \item{\code{Se@SpX.uQuart}: Upper limit of 95\% confidence interval for sensitivity at X\% specificity}
#' \item{\code{Sp@SeX}: Specificity at X\% sensitivity}
#' \item{\code{Sp@SeX.lCI}: Lower limit of 95\% confidence interval for specificity at X\% sensitivity}
#' \item{\code{Sp@SeX.uCI}: Upper limit of 95\% confidence interval for specificity at X\% sensitivity}
#' \item{\code{Sp@SeX.lQuart}: Lower limit of 50\% confidence interval for specificity at X\% sensitivity}
#' \item{\code{Sp@SeX.uQuart}: Upper limit of 50\% confidence interval for specificity at X\% sensitivity}
#' \item{Additional columns for statistics computed by the function \code{fun} if provided}
#' }
#' 
#' @import stats
#' @importFrom Hmisc label
#' @importFrom pROC roc ci
#' 
#' @return \code{data.frame} with one row with computed statistics in columns. 
#'
#' @export 
#'
rocperf <- function(x,y,
                    sensitivities=NULL,
                    specificities=NULL,
                    conf.level=.95,
                    #prevalence=NULL,
                    fun=NULL,
                    seed=1,boot.n=2e3,
                    median=FALSE,
                    attr=FALSE,
                    parallel=FALSE,
                    simplify=TRUE,
                    ...) {
  ## ==========
  lbz <- list(
    `n.control`="number of control patients"
    ,`n.case`="number of case patients"
    ,`MannWhitney.pvalue`="Mann Whitney U test (p Value)"
    ,`AUC.pvalue`="p [ AUC=0.5 ] (DeLong et al. 1988)"
    ,`AUC`="area under the receiver operating curve (point estimate)"
    ,`AUC.lCI`="area under the receiver operating curve, lower limit of 95% confidence interval"
    ,`AUC.uCI`="area under the receiver operating curve, upper limit of 95% confidence interval"
    ,`AUC.lQuart`="area under the receiver operating curve, lower limit of 50% confidence interval"
    ,`AUC.uQuart`="area under the receiver operating curve, upper limit of 50% confidence interval"
  )
  ## ==========
  for(v in specificities) {
    lbz[[sprintf("Se@Sp%g",v*100)]] <- sprintf("sensitivity at %g%% specificity",v*100)
    lbz[[sprintf("Se@Sp%g.lCI",v*100)]] <- sprintf("sensitivity at %g%% specificity, lower limit of %f%% confidence interval",v*100,conf.level*100)
    lbz[[sprintf("Se@Sp%g.uCI",v*100)]] <- sprintf("sensitivity at %g%% specificity, higher limit of %f%% confidence interval",v*100,conf.level*100)
    lbz[[sprintf("Se@Sp%g.lQuart",v*100)]] <- sprintf("sensitivity at %g%% specificity, lower limit of 50%% interval",v*100)
    lbz[[sprintf("Se@Sp%g.uQuart",v*100)]] <- sprintf("sensitivity at %g%% specificity, higher limit of 50%% confidence interval",v*100)
  }
  for(v in sensitivities) {
    lbz[[sprintf("Sp@Se%g",v*100)]] <- sprintf("specificity at %g%% sensitivity",v*100)
    lbz[[sprintf("Sp@Se%g.lCI",v*100)]] <- sprintf("specificity at %g%% sensitivity, lower limit of %f%% confidence interval",v*100,conf.level*100)
    lbz[[sprintf("Sp@Se%g.uCI",v*100)]] <- sprintf("specificity at %g%% sensitivity, higher limit of %f%% confidence interval",v*100,conf.level*100)
    lbz[[sprintf("Sp@Se%g.lQuart",v*100)]] <- sprintf("specificity at %g%% sensitivity, lower limit of 50%% interval",v*100)
    lbz[[sprintf("Sp@Se%g.uQuart",v*100)]] <- sprintf("specificity at %g%% sensitivity, higher limit of 50%% confidence interval",v*100)
  }
  ## ==========
  wa <- !is.na(x*unclass(y))
  ## ==========
  if(all(y%in%c("case","control"),na.rm = TRUE)) {
    y <- factor(y,levels = c("control","case"))
  } else if(all(y%in%c("yes","no"),na.rm = TRUE)) {
    y <- factor(y,levels = c("no","yes"))
  } else if(!is.factor(y)) {
    y <- factor(y)
  }
  if(length(levels(y))>2) stop("outcome must be binary")
  yu <- unclass(y) == 2
  ## ==========
  ro <- bm <- NA
  ans <- data.frame(n.control=sum(y=="control",na.rm=TRUE)
                   ,n.case   =sum(y=="case",na.rm=TRUE)
  )
  errz <- NULL
  ## ==========
  if(all(table(y[wa])>3)) {
    ## =====
    owa <- options()$warn ; options(warn=0) ; on.exit(options(warn = owa))
    ro <- pROC::roc(y,x,quiet=TRUE)
    au <- sort(pROC::ci(ro,conf.level=conf.level))
    aq <- sort(pROC::ci(ro,conf.level=.5))
    fq <- NULL
    td <- p.auc(ro)
    tm <- wilcox.test(x[wa&!yu],x[wa&yu])$p.value
    ## =====
    ans <- data.frame(n.control=sum(!yu,na.rm=TRUE)
                      ,n.case   =sum( yu,na.rm=TRUE)
                      ,MannWhitney.pvalue=tm
                      ,AUC.pvalue=td
                      ,AUC=au[2],AUC.lCI=au[1],AUC.uCI=au[3]
                      ,AUC.lQuart=aq[1],AUC.uQuart=aq[3]
    )
    ## =====
    if(!is.null(sensitivities)||!is.null(specificities)||!is.null(fun)) {
      if(parallel) {
        stop("parallelisation not implemented")
      } else {
        n <- c("check")
        if(!is.null(sensitivities)) n <- c(n,paste0("Sp@Se",sensitivities*100))
        if(!is.null(specificities)) n <- c(n,paste0("Se@Sp",specificities*100))
        bm <- matrix(NA,
                     nrow=boot.n,
                     ncol=length(n))
        colnames(bm) <- n
        for(iboot in 1:boot.n) {
          ## ===
          if(!median&&(iboot==1)) {
            wco <- wca <- TRUE
          } else {
            set.seed(seed*1e6+iboot)
            wco <- sample(1:length(ro$controls),replace=TRUE)
            wca <- sample(1:length(ro$cases),replace=TRUE)
          }
          controls <- ro$controls[wco]
          cases <- ro$cases[wca]
          ## ===
          bm[iboot,"check"] <- sum(wco^.5)-sum(wca^.5)
          perfs <- rocsesp(controls=controls,cases=cases,decreasing=ro$direction=="<")
          ## ===
          for(cutoff in sensitivities) {
            if(perfs$se[1]>.5) w <- unique(c(min(which(perfs$se <= cutoff)), max(which(perfs$se >= cutoff)))) # decrease
            else w <- unique(c(min(which(perfs$se >= cutoff)), max(which(perfs$se <= cutoff)))) # increase
            if(length(w)==1) {
              bm[iboot,paste("Sp@Se",cutoff*100,sep="")] <- max(c(0,perfs$sp[w]),na.rm=TRUE)
            } else {
              pp <- ifelse(perfs$se[w[2]]==perfs$se[w[1]], 1, (cutoff-perfs$se[w[1]]) / (perfs$se[w[2]]-perfs$se[w[1]]))
              bm[iboot,paste("Sp@Se",cutoff*100,sep="")] <- max(c(0,perfs$sp[w[1]]+pp*diff(perfs$sp[w])),na.rm=TRUE)
            }
          }
          ## ===
          for(cutoff in specificities) {
            if(perfs$sp[1]>.5) w <- unique(c(min(which(perfs$sp <= cutoff)), max(which(perfs$sp >= cutoff)))) # decrease
            else w <- unique(c(min(which(perfs$sp >= cutoff)), max(which(perfs$sp <= cutoff)))) # increase 
            if(length(w)==1) {
              bm[iboot,paste("Se@Sp",cutoff*100,sep="")] <- max(c(0,perfs$se[w]),na.rm=TRUE)
            } else {
              pp <- ifelse(perfs$sp[w[2]]==perfs$sp[w[1]], .5, (cutoff-perfs$sp[w[1]]) / (perfs$sp[w[2]]-perfs$sp[w[1]]))
              bm[iboot,paste("Se@Sp",cutoff*100,sep="")] <- max(c(0,perfs$se[w[1]]+pp*diff(perfs$se[w])),na.rm=TRUE)
            }
          }
          ## ===
          if(!is.null(fun)) {
            vfun <- unlist(fun(controls,cases,perfs$th,perfs$se,perfs$sp,...))
            if(!is.numeric(vfun)) {
              stop("fun must return a named numeric vector")
            }
            if(length(e<-setdiff(names(vfun),colnames(bm)))>0) {
              b <- matrix(NA,nrow=boot.n,ncol=length(e),
                          dimnames=list(rownames(bm),e))
              bm <- cbind(bm,b)
            }
            bm[iboot,names(vfun)] <- vfun
            ## =
            if(!is.null(attr(vfun,"errors")))
              errz <- c(errz,attr(vfun,"errors"))
          }
          ## ===
        }
      }
      for(f in setdiff(colnames(bm),c("auc","check"))) {
        if(any(is.na(bm[,f]))) {
          ans[[f]] <- ifelse(median,NA,bm[1,f])
        } else {
          ans[[f]] <- ifelse(median,median(bm[,f]),bm[1,f])
          if(!simplify||(diff(range(bm[,f]))/ifelse(bm[1,f]==0,1,bm[1,f])>1e-6)) {
            ans[[paste0(f,".lCI")]] <- quantile(bm[,f],  .5*(1-conf.level))
            ans[[paste0(f,".uCI")]] <- quantile(bm[,f],1-.5*(1-conf.level))
            ans[[paste0(f,".lQuart")]] <- quantile(bm[,f],.25)
            ans[[paste0(f,".uQuart")]] <- quantile(bm[,f],.75)          
          }        
        }
      }
    }
    ## =====
  }
  attr(ans,"errors") <- errz
  ## ==========
  for(f in setdiff(names(lbz),names(ans))) ans[[f]] <- NA
  for(f in names(lbz)) Hmisc::label(ans[[f]]) <- lbz[[f]]
  if(attr) {
    attr(ans,"bootstrap") <- bm
    attr(ans,"roc") <- ro
  }
  ## ==========
  ans
}
## ============================================================================

