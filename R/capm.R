# CAPM Function 
# Description for CAPM
# @param r risk-free rate
# @param mkrt market return
# @return the function returns tstat upon default & pvalue when specified
# @export
# capm.tstats = function(r,mkrt,type = FALSE) {
#   # Fiting CAPM and retrieve alpha specific tstats or pvalues
#   capm.fit = lm(r~mkrt)    
#   # Extract summary info
#   capm.summary = summary(capm.fit) 
#   if(is.null(type) | type=="pvalue"){
#     # Retrieve p-value if specified
#     p.value = coef(capm.summary)[1,4]  
#     p.value
#   }else{
#     # Otherwise retrieve t-stat if specified or on default
#     t.stat = coef(capm.summary)[1,3]  
#     t.stat
#   }
# }

#' Capital Asset Pricing Model
#' 
#' CAPM describes the relationship between risk and (expected) return
#' 
#' Retrieves alphas, betas, as well as pvalue and tstats. 
#' The CAPM is used to determine a theoretically appropriate rate of return
#' of the non-diversifiable risk of an asset.
#' 
#' @param R asset returns
#' @param Rmkt market returns
#' @examples
#' data(crsp.short)
#' 
#' head(largecap.ts)
#' 
#' Rf <- largecap.ts[, "t90"]
#' R <- largecap.ts[, "CAT"] - Rf
#' MKT <- largecap.ts[, "market"] - Rf
#'
#' # Fit the CAPM model
#' tmp <- CAPM(R=R, Rmkt=MKT)
#' @author Thomas Fillebeen
#' @export
CAPM <- function(R, Rmkt){  
  capm_fit <- lm(R ~ Rmkt)
  capm_fit$x_data <- Rmkt
  capm_fit$y_data <- R
  
  if(ncol(R) > 1){
    #  Multiple Linear Model CAPM
    class(capm_fit) <- c("capm_mlm", "mlm", "lm")
  } else if(ncol(R) == 1){
    #  Univariate CAPM
    class(capm_fit) <- c("capm_uv", "lm")
  }
  return(capm_fit)
}

#' CAPM alphas
#' 
#' Extract the computed alphas (intercept) from the fitted CAPM object.
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @author Thomas Fillebeen
#' @export
getAlphas <- function(object){
  UseMethod("getAlphas")
}

#' @method getAlphas capm_uv
#' @S3method getAlphas capm_uv
getAlphas.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  temp = getStatistics(object)[,1]
  return(temp[1])
}

#' @method getAlphas capm_mlm
#' @S3method getAlphas capm_mlm
getAlphas.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  tmp_sm = tmp_sm[seq(1,nrow(tmp_sm),2),1]
  return(tmp_sm)
}

#' CAPM betas
#' 
#' Extract the computed alpha (intercept) from the CAPM object.
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @author Thomas Fillebeen
#' @export
getBetas <- function(object){
  UseMethod("getBetas")
}

#' @method getBetas capm_uv
#' @S3method getBetas capm_uv
getBetas.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  temp = getStatistics(object)[,1]
  return(temp[2])
}

#' @method getBetas capm_mlm
#' @S3method getBetas capm_mlm
getBetas.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  tmp_sm = tmp_sm[seq(2,nrow(tmp_sm),2),1]
  return(tmp_sm)
}

#' CAPM statistics
#' 
#' Extract the standard error, t-values, and p-values from the CAPM object.
#' 
#' The t-statistic and corresponding two-sided p-value are calculated differently
#' for the alpha and beta coefficients.
#' \itemize{
#'   \item{alpha}{ the t-statistic and corresponding p-value are calculated to
#'   test if alpha is significantly different from 0.
#'   \deqn{
#'     H0: \alpha = 0
#'   }
#'   }
#'   \item{beta}{ the t-statistic and corresponding p-value are calculated to
#'   test if beta is significantly different from 1.
#'   \deqn{
#'     H0: \beta = 1
#'   }
#'   }
#' }
#' 
#' @param object a capm object created by \code{\link{CAPM}}.
#' @author Thomas Fillebeen
#' @export
getStatistics <- function(object){
  UseMethod("getStatistics")
}

#' @method getStatistics capm_uv
#' @S3method getStatistics capm_uv
getStatistics.capm_uv <- function(object){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  tmp_sm <- summary.lm(object)
  # Gets t-value, and p-value of model
  result = coef(tmp_sm)[,c(1:4)]
  # recalculate the tstat for the beta to test if beta is significantly different from 1
  # (beta - beta0) / se
  tstat = (result[2,1] - 1 )/result[2,2]
  # Two sided t-test
  pvalue= (2*(1 - pt(abs(tstat),df=nrow(object$x_data)-2)))
  result[2,3:4] = cbind(tstat, pvalue)
  rownames(result) = cbind(c(paste("alpha.", colnames(object$y_data))),c(paste("beta. ", colnames(object$y_data))))
  return(result)
}

#' @method getStatistics capm_mlm
#' @S3method getStatistics capm_mlm
getStatistics.capm_mlm <- function(object){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  # Gets t-value, and p-value of model
  # Multi-Beta CAPM
  x <- coef(summary(object))
  tmp_sm <- do.call(rbind, x)
  holder = matrix(0,nrow=1,ncol=ncol(coef(object))*2)
  n=1
  for (i in 1:ncol(coef(object))){
    holder[,n:(i*2)] = cbind(c(paste("alpha.",colnames(coef(object))[i])) ,c(paste("beta. ",colnames(coef(object))[i])))
    n = i*2 +1
  }
  rownames(tmp_sm) <- c(holder)
  # recalculate the tstat for the beta to test if beta is significantly different from 1
  # (beta - beta0) / se
  tstat = (tmp_sm[seq(2,nrow(tmp_sm),2),1] - 1 )/tmp_sm[seq(2,nrow(tmp_sm),2),2]
  #' Two sided t-test
  pvalue = (2*(1 - pt(abs(tstat),df=nrow(object$x_data)-2)))
  tmp_sm[seq(2,nrow(tmp_sm),2),3:4] = cbind(tstat,pvalue)
  return(tmp_sm)
}

#' Plotting method for CAPM
#' 
#' Plot a fitted CAPM object
#' 
#' @param x a capm object created by \code{\link{CAPM}}.
#' @param y not used
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param main a main title for the plot
#' @author Thomas Fillebeen
#' @method plot capm_uv
#' @S3method plot capm_uv
plot.capm_uv <- function(x, y, ..., main="CAPM"){
  xlab <- colnames(x$x_data)
  ylab <- colnames(x$y_data)
  plot(x=coredata(x$x_data), y=(x$y_data), ...=..., xlab=xlab, ylab=ylab, main=main)
  abline(x)
  abline(h=0,v=0,lty=3)
  alpha = coef(summary(x))[1,1]
  a_tstat = coef(summary(x))[1,2]
  beta = coef(summary(x))[2,1]
  b_tstat = coef(summary(x))[2,2]
  legend("topleft", legend=c(paste("alpha =", round(alpha,digits=2),"(", round(a_tstat,digits=2),")"),
                             paste("beta =", round(beta,digits=2),"(", round(b_tstat,digits=2),")")), cex=.8, bty="n")
  
}

#' Plotting method for CAPM
#' 
#' Plot a fitted CAPM object
#' 
#' @param x a capm object created by \code{\link{CAPM}}.
#' @param y not used
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param main a main title for the plot
#' @author Thomas Fillebeen
#' @method plot capm_mlm
#' @S3method plot capm_mlm
plot.capm_mlm <- function(x, y, ..., main="CAPM"){
  if(ncol(x$y_data) > 4) warning("Only first 4 assets will be graphically displayed")
  par(mfrow=c(2,min(round(ncol(coef(x))/2,2))))
  Rmkt = x$x_data
  nbPlot = min(ncol(coef(x)),4)
  for (i in 1:nbPlot){
    tmp = CAPM(x$y_data[,i], Rmkt)
    plot(tmp, ...=..., main=main)
  }
  par(mfrow=c(1,1))
}

#' CAPM SML
#' 
#' Security Market Line (SML) of the CAPM.
#' The SML is a represesentation of the CAPM. It illustrates the expected rate 
#' of return of an individual security as a function of systematic, 
#' non-diversified risk (known as beta).
#' 
#' @param object a capm object created by \code{\link{CAPM}}.
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param main a main title for the plot.
#' @author Thomas Fillebeen
#' @export
chartSML <- function(object, ..., main="Estimated SML"){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  #' Plot expected return versus beta
  mu.hat = colMeans(object$y_data, na.rm=TRUE)
  betas = getBetas(object)
  sml.fit = lm(mu.hat~betas)
  # Plot Fitted SML
  plot(betas,mu.hat,main=main, ...=...)
  abline(sml.fit)
  #legend("topleft",1, "Estimated SML",1)                  
}


#' CAPM Hypothesis Test
#' 
#' Test the CAPM coefficients for significance.
#' 
#' @details
#' This function tests the significance of the coefficients (alpha and beta)
#' estimated by the CAPM.
#' 
#' #' The t-statistic and corresponding two-sided p-value are calculated differently
#' for the alpha and beta coefficients
#' \itemize{
#'   \item{alpha}{ the t-statistic and corresponding p-value are calculated to
#'   test if alpha is significantly different from 0.
#'   \deqn{
#'     H0: \alpha = 0
#'   }
#'   }
#'   \item{beta}{ the t-statistic and corresponding p-value are calculated to
#'   test if beta is significantly different from 1.
#'   \deqn{
#'     H0: \beta = 1
#'   }
#'   }
#' }
#' 
#' If the p-value is less than the coefficients confidence level, the null 
#' hypothesis is rejected meaning that the coefficients is significant. If 
#' the p-value is greater than the specified confidence level, the null 
#' hypothesis cannot be rejected.
#' 
#' @param object a capm object created by \code{\link{CAPM}}
#' @param significanceLevel confidence level
#' @return TRUE if the null hypothesis is rejected (i.e. the estimated coefficient is significant)
#' FALSE if the null hypothesis cannot be rejected (i.e. the estimated coefficient is not significant)
#' @seealso \code{\link{getStatistics}}
#' @author Thomas Fillebeen
#' @export
hypTest <- function(object,significanceLevel){
  UseMethod("hypTest")
}

#' @method hypTest capm_uv
#' @S3method hypTest capm_uv
hypTest.capm_uv <- function(object, significanceLevel = 0.05){
  if(!inherits(object, "capm_uv")) stop("object must be of class capm_uv")
  tmp_sm = getStatistics(object)
  # test for alpha p-value < significanceLevel
  tmp_A = tmp_sm[1,4] < significanceLevel
  # test for beta p-value < significanceLevel
  tmp_B = tmp_sm[2,4] < significanceLevel
  result = list(alpha = tmp_A, beta = tmp_B)
  return(result)
}

#' @method hypTest capm_mlm
#' @S3method hypTest capm_mlm
hypTest.capm_mlm <- function(object, significanceLevel = 0.05){
  if(!inherits(object, "capm_mlm")) stop("object must be of class capm_mlm")
  tmp_sm = getStatistics(object)
  # test for alpha p-value < significanceLevel
  tmp_A = tmp_sm[seq(1,nrow(tmp_sm),2),4] < significanceLevel
  # test for beta p-value < significanceLevel
  tmp_B = tmp_sm[seq(2,nrow(tmp_sm),2),4] < significanceLevel
  result = list(alpha = tmp_A, beta = tmp_B)  
  return(result)
}
