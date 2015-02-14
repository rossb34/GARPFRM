

# # Backtesting VaR (backTestVaR)
# # 
# # Description of backTestVaR. The function should handle UV and MLM.
# # 
# # @param R returns
# # @param CI confidence level
# # @export
# backTestVaR <- function(R, CI = 0.95) {
#   if (ncol(R)>1){stop("One Asset at a time")}
#   normalVaR = as.numeric(VaR(R, p=CI, method="gaussian")) 
#   historicalVaR = as.numeric(VaR(R, p=CI, method="historical")) 
#   modifiedVaR = as.numeric(VaR(R, p=CI, method="modified"))
#   result = c(normalVaR, historicalVaR, modifiedVaR)
#   names(result) = c("Normal", "HS", "Modified")  
#   
#   return(result)
# }
# 
# # Count backtesting VaR
# # 
# # Description of countBacktesting VaR
# # 
# # @param backTestVaR object created by \code{\link{backTestVaR}}
# # @param initialWindow
# # @param CI
# # @param temp 
# # @export
# countViolations <- function(object, temp, initialWindow, CI){
#   UseMethod("countViolations")
# }
# 
# # @method countViolations xts
# # @S3method countViolations xts
# countViolations.xts <- function(object, temp, initialWindow=10, CI=0.95){
#   violations = matrix(0, 3, 5)
#   testWindow = nrow(temp) -initialWindow
#   rownames(violations) = c("Normal", "HS", "Modified")
#   colnames(violations) = c("En1", "n1", "1-CI", "Percent", "VaR")
#   violations[, "En1"] = (1-CI)*initialWindow
#   violations[, "1-CI"] = 1 - CI
#   
#   for(i in colnames(object)) {
#     violationVaR = temp[index(object), ] < object[, i]
#     violations[i, "n1"] = sum(violationVaR, na.rm= TRUE)
#     violations[i, "Percent"] = sum(violationVaR, na.rm=TRUE)/testWindow
#     violations[i, "VaR"] = violations[i, "n1"]/violations[i, "En1"]
#   }
#   return(violations)
# }

# The backTestVaR and countViolations.xts functions are more or less copied from
# http://faculty.washington.edu/ezivot/econ589/econ589backtestingRiskModels.r
# We either need to give credit here or write our own code and structure the
# way we want for plotting and output.

#' Backtest Value-at-Risk (VaR)
#' 
#' Backtesting Value-at-Risk estimate over a moving window.
#' 
#' @details
#' The size of the moving window is set with the \code{window} argument. For 
#' example, if the window size is 100, periods 1:100 are used to estimate the
#' VaR level for period 101.
#' 
#' @param R xts or zoo object of asset returns
#' @param window size of the moving window in the rolling VaR estimate.
#' @param p confidence level for the VaR estimate.
#' @param method method for the VaR calculation. Valid choices are "modified", "guassian", "historical", and "kernel"
#' @param bootstrap TRUE/FALSE use the bootstrap estimate for the VaR calculation, (default FALSE).
#' @param replications number of bootstrap replications.
#' @param bootParallel TRUE/FALSE run the bootstrap in parallel, (default FALSE).
#' @author Ross Bennett
#' @seealso \code{\link[PerformanceAnalytics]{VaR}}, \code{\link{bootVaR}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1]
#' backtest <- backtestVaR(R, window=100, p=0.95, method=c("gaussian", "historical", "modified"))
#' backtest
#' 
#' head(getVaREstimates(backtest))
#' head(getVaRViolations(backtest))
#' @export
backtestVaR <- function(R, window=100, p=0.95, method="historical", bootstrap=FALSE, replications=1000, bootParallel=FALSE){
  if(!is.xts(R)) stop("R must be an xts or zoo object")
  if(ncol(R) > 1) {
    warning("VaR backtest only supported for univariate series. Using R[,1]")
    R <- R[,1]
  }
  p <- p[1]
  # number of observations
  n <- nrow(R)
  # vector to store the VaR estimates
  # est <- vector("numeric", (n-window+1))
  est <- matrix(0, nrow=(n-window+1), ncol=length(method))
  if(bootstrap){
    for(j in 1:length(method)){
      for(i in window:n){
        tmpR <- R[(i-window+1):i,]
        # compute VaR estimate
        est[(i-window+1), j] <- bootVaR(R=tmpR, 
                                        ...=pairlist(p=p, method=method[j], portfolio_method="single"), 
                                        replications=replications, parallel=bootParallel)[1,]
      }
    }
  } else {
    for(j in 1:length(method)){
      for(i in window:n){
        tmpR <- R[(i-window+1):i,]
        # compute VaR estimate
        est[(i-window+1), j] <- VaR(R=tmpR, p=p, method=method[j], portfolio_method="single")
      }
    }
  }
  # convert to xts and lag by k=1 for 1-step ahead VaR forecast
  est <- na.omit(lag(xts(est, index(R)[seq.int(from=window, to=n, by=1L)]), k=1))
  colnames(est) <- paste(method, " VaR (", (1-p)*100, "%)" )
  
  # subset the actual returns to the same period as the VaR forecast estimates
  backtestR <- R[seq.int(from=(window+1), to=n, by=1L)]
  violation <- matrix(0, nrow=nrow(est), ncol=ncol(est))
  colnames(violation) <- colnames(est)
  for(i in 1:ncol(est)){
    violation[,i] <- backtestR < est[,i]
  }
  violation <- xts(violation, index(est))
  
  # put the VaR estimate and violation into a list
  dataVaR <- list(estimate=est, violation=violation)
  
  # put the model parameters into a list
  parameters <- list(p=p, window=window)
  
  # structure and return
  structure(list(VaR=dataVaR, R=R, parameters=parameters), class="backtestVaR")
}

#' GARCH Model VaR Backtest
#' 
#' Function for rolling estimate of GARCH model and VaR backtest
#' 
#' @param garch uvGARCH object create via \code{\link{uvGARCH}}
#' @param p confidence level for the VaR estimate.
#' @param nAhead number of steps ahead to forecast. (nAhead = 1 only supported)
#' @param refitEvery number of periods the mode is refit
#' @param window size of the moving window in the rolling VaR estimate.
#' @author Ross Bennett
#' @seealso \code{\link[rugarch]{ugarchroll}}
#' @export
backtestVaR.GARCH <- function(garch, p=c(0.95, 0.99), nAhead=1, refitEvery=25, window=100){
  # GARCH model VaR Backtesting
  # http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
  # extract R from the fit object
  R <- xts(garch$fit@model$modeldata$data, garch$fit@model$modeldata$index)
  # call ugarchroll
  modelRoll <- ugarchroll(spec=getSpec(garch), data=R, n.ahead=nAhead, 
                          refit.every=refitEvery, refit.window="moving", 
                          window.size=window, VaR.alpha=(1-p))
  estimatedVaR <- modelRoll@forecast$VaR
  
  dates <- as.Date(rownames(estimatedVaR))
  # GARCH VaR estimates
  idx <- grep(pattern="alpha", x=colnames(estimatedVaR))
  est <- xts(estimatedVaR[, idx], dates)
  colnames(est) <- paste("GARCH VaR", colnames(estimatedVaR)[idx])
  
  # Realized returns
  backtestR <- xts(estimatedVaR[, "realized"], dates)
  
  # matrix of violations
  violation <- matrix(0, nrow=nrow(est), ncol=ncol(est))
  colnames(violation) <- colnames(est)
  for(i in 1:ncol(est)){
    violation[,i] <- backtestR < est[,i]
  }
  violation <- xts(violation, index(est))
  
  # put the VaR estimate and violation into a list
  dataVaR <- list(estimate=est, violation=violation)
  
  # put the model parameters into a list
  parameters <- list(p=p, window=window)
  structure(list(VaR=dataVaR, R=R, parameters=parameters), 
            class=c("backtestVaR", "uGARCHroll"))
}


#' VaR Estimates
#' Extract VaR Estimates from a VaR Backtest
#' 
#' @param object an object created by \code{\link{backtestVaR}}.
#' @param \dots not currently used
#' @return xts object of unconditional VaR estimates
#' @author Ross Bennett
#' @seealso \code{\link{backtestVaR}}
#' @export
getVaREstimates <- function(object, ...){
  if(!inherits(object, "backtestVaR")) stop("object must be of class 'backtestVaR'")
  object$VaR$estimate
}

#' VaR Violations
#' Extract VaR Violations from a VaR Backtest
#' 
#' @param object an object created by \code{\link{backtestVaR}}.
#' @param \dots not currently used
#' #' @return xts object of VaR violations
#' @author Ross Bennett
#' @seealso \code{\link{backtestVaR}}
#' @export
getVaRViolations <- function(object, ...){
  if(!inherits(object, "backtestVaR")) stop("object must be of class 'backtestVaR'")
  object$VaR$violation
}

#' @method print backtestVaR
#' @S3method print backtestVaR
print.backtestVaR <- function(x, ...){
  cat("Value-at-Risk Backtest\n\n")
  
  cat("Returns Data:\n")
  print(colnames(x$R))
  cat("\n")
  
  cat("1 - p tail quantile:\n")
  print(1 - x$parameters$p)
  cat("\n")
  
  cat("Number of Violations:\n")
  nViolations <- colSums(x$VaR$violation)
  print(nViolations)
  cat("\n")
  
  cat("Violations (%):\n")
  print(nViolations / nrow(x$VaR$violation) * 100)
  cat("\n")
  
  #cat("VaR Estimate Data Summary:\n")
  #print(head(x$VaR$estimate))
  #print(tail(x$VaR$estimate))
  #cat("\n")
}

#' Plotting for VaR Backtest
#' 
#' Plotting method for VaR Backtest
#' 
#' @param x backtestVaR object created with \code{\link{backtestVaR}}.
#' @param y not used.
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param pch plotting 'character' for the violation points, same as in 
#' \code{\link{plot}}. If NULL, violation points will not be plotted.
#' @param main the main title
#' @param ylim limits for the y-axis, same as in \code{\link{plot}}.
#' @param colorset colorset for plotting the VaR forecasts. The length of 
#' colorset should be equal to the number of VaR methods.
#' @param legendLoc legend location. If NULL, no legend is plotted.
#' @param legendCex numerical value giving the amount by which the legend.
#' text should be magnified relative to the default. 
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1]
#' backtest <- backtestVaR(R, window=100, p=0.95, method=c("gaussian", "historical", "modified"))
#' plot(backtest, pch=18, legendLoc="topright")
#' @method plot backtestVaR
#' @S3method plot backtestVaR
plot.backtestVaR <- function(x, y, ..., pch=NULL, main="VaR Backtest", ylim=NULL, colorset=NULL, legendLoc=NULL, legendCex=0.8){
  if(!inherits(x, "backtestVaR")) stop("x must be of class 'backtestVaR'")
  
  # get the VaR estimates
  tmpVaR <- x$VaR$estimate
  tmpViolation <- x$VaR$violation
  
  # get the actual retun data
  R <- x$R
  
  # set an appropriate ylim
  ranges <- c(range(tmpVaR), range(R))
  if(is.null(ylim)) ylim <- c(min(ranges), max(ranges))
  
  # set the colorset
  if(is.null(colorset)) colorset <- seq.int(from=2, to=(ncol(tmpVaR)+1), by=1)
  
  # plot the returns and VaR estimates
  plot(R, ...=..., main=main, type="n", ylim=ylim)
  lines(R)
  for(i in 1:ncol(tmpVaR)){
    lines(tmpVaR[, i], col=colorset[i])
    if(!is.null(pch)) points(tmpVaR[,i][which(tmpViolation[,i] == 1)], col=colorset[i], pch=pch)
  }
  
  # add the legend to the plot
  if(!is.null(legendLoc)){
    legendNames <- c("observed returns", colnames(tmpVaR))
    legend(legendLoc, legend=legendNames, col=c(1, colorset), 
    lty=rep(1, ncol(tmpVaR)+1), cex=legendCex, bty="n")
  }
}

