# 
# #' Exponential Weighted Moving Average (EWMA)
# #' 
# #' Use an exponentially weighted moving average to estimate the covariance or 
# #' correlation of asset returns.
# #' 
# #' @param R xts object asset returns
# #' @param lambda smoothing parameter, must be greater than 0 or less than 1
# #' @param initialWindow initial window of observations used in estimating the 
# #' initial conditions
# #' @param TRUE/FALSE to return a correlation matrix. Default cor = FALSE.
# #' @examples
# #' data(crsp.short)
# #' # Use the first 5 assets in largecap.ts for the returns data
# #' R <- largecap.ts[, 1:5]
# #' # Estimate the covariance matrix via EWMA
# #' covEst <- EWMA(R, 0.94, 15)
# #' @export
# EWMA <- function(R, lambda=0.94, initialWindow=10, cor=FALSE){
#   # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
#   if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
#   if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
#   
#   # Separate data into a initializing window and a testing window
#   initialR = R[1:initialWindow,]
#   testR = R[(initialWindow+1):nrow(R),]
#   
#   # Initialization of covariance matrix
#   lagCov = cov(initialR)
#   covTmp = vector("list", nrow(testR))
#   for(i in 1:nrow(testR)){
#     # Extract R for the ith time step
#     tmpR = testR[i,]
#     covTmp[[i]] = lambda * (t(tmpR)%*%tmpR) + (1 - lambda) * lagCov
#     # Update lagCov to be covTmp from the current period
#     lagCov <- covTmp[[i]]
#   }
#   # Final estimated EWMA of covariance
#   estEWMA <- covTmp
#   # Properly assign list key to date
#   names(estEWMA) <- index(testR)
#   
#   # Add R as a separate element to object to return
#   out <- list(EWMA=estEWMA, R=R)
#   
#   # Check correlation option
#   if(cor & ncol(R) > 1) {
#     out$EWMA <- lapply(out$EWMA, cov2cor)
#     class(out) <- c("corEWMA")
#   } else if(cor & (ncol(R) == 1)) {
#     stop("EWMA correlation is only to be estimated for two or more assets")
#   }
#   
#   # Check for covariance or variance
#   if((cor == FALSE) & (ncol(R) > 1)) { 
#     class(out) <- c("covEWMA")
#   } else if ((cor == FALSE) & (ncol(R) == 1)){
#     class(out) <- c("varEWMA")
#   }
#   class(out) <- c("EWMA", class(out))
#   return(out)
# }

#' EWMA Covariance
#' 
#' Extract the covariance between two assets from an EWMA object
#' 
#' @param EWMA an EWMA object created by \code{EWMA}
#' @param assets character vector or numeric vector. If 
#' \code{assets} is of length 1, then the variance will be returned. 
#' The assets can be specified by name or index.
#' @examples
#' data(crsp.short)
#' # Use the first 5 assets in largecap.ts for the returns data
#' R <- largecap.ts[, 1:5]
#' # Estimate the covariance matrix via EWMA
#' covEst <- EWMA(R, 0.94, 15)
#' # get the covariance between AMAT and CAT
#' covAMATCAT <- getCov(covEst, assets=c("AMAT", "CAT"))
#' cov13 <- getCov(covEst, assets=c(1, 3))
#' all.equal(covAMATCAT, cov13)
#' @export
getCov <- function(EWMA, assets){
  UseMethod("getCov")
}

#' @method getCov covEWMA
#' @S3method getCov covEWMA
getCov.covEWMA <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "covEWMA")) stop("object must be of class covEWMA")
  
  if(length(assets) == 1) assets[2] <- assets[1]
  
  cnames <- colnames(EWMA$EWMA[[1]])
  
  # Check if asset is a character
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], cnames)
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  
  out = xts(unlist(lapply(EWMA$EWMA, function(X) X[idx1, idx2])), as.Date(names(EWMA$EWMA)))
  colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  
  return(out)
}

#' @method getCov varEWMA
#' @S3method getCov varEWMA
getCov.varEWMA <- function(EWMA, assets=1){
  if(!inherits(EWMA, "varEWMA")) stop("EWMA must be of class varEWMA")
  
  cnames <- colnames(EWMA$EWMA[[1]])
  
  # Check if asset is a character
  if(is.character(assets[1])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset not in EWMA object")
  } else {
    idx1 = assets[1]
  }
  out = xts(unlist(lapply(EWMA$EWMA, function(x) x[idx1])), as.Date(names(EWMA$EWMA)))
  colnames(out) = cnames[idx1]
  return(out)
}

#' EWMA Correlation
#' 
#' Extract the correlation of two assets from an \code{EWMA} object
#' 
#' @param object an EWMA object created by \code{EWMA}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @examples
#' data(crsp.short)
#' # Use the first 5 assets in largecap.ts for the returns data
#' R <- largecap.ts[, 1:5]
#' # Estimate the correlation matrix via EWMA
#' corEst <- EWMA(R, 0.94, 15, TRUE)
#' # get the correlation between AMAT and CAT
#' corAMATCAT <- getCov(corEst, assets=c("AMAT", "CAT"))
#' cor13 <- getCov(corEst, assets=c(1, 3))
#' all.equal(corAMATCAT, cor13)
#' @export
getCor <- function(EWMA, assets){
  UseMethod("getCor")
}

#' @method getCor corEWMA
#' @S3method getCor corEWMA
getCor.corEWMA <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "corEWMA")) stop("EWMA must be of class corEWMA")
  
  cnames <- colnames(EWMA$EWMA[[1]])
  
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], cnames)
    if(length(idx1) == 0) stop("name for asset1 not in EWMA object")
    idx2 = grep(assets[2], cnames)
    if(length(idx2) == 0) stop("name for asset2 not in EWMA object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  
  out = xts(unlist(lapply(EWMA$EWMA, function(x) x[idx1, idx2])), as.Date(names(EWMA$EWMA)))
  colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  
  return(out)
}

# EWMA plotting for covar
#' @method plot covEWMA
#' @S3method plot covEWMA
plot.covEWMA <- function(object, ..., assets=c(1, 2), xlab="", ylab="Covariance", main="EWMA Estimate"){
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(object$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for assets[1] not in object")
    idx2 = grep(assets[2], colnames(object$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for assets[2] not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  tmp = getCov(object, assets)
  plot(x=time(as.zoo(tmp)), y=tmp, ...=..., type="l", xlab=xlab, ylab=ylab, main=main)
  grid()
  abline(h=var(object$R)[idx1,idx2], lwd=2, col="red")
}


# EWMA plotting for var
#' @method plot varEWMA
#' @S3method plot varEWMA
plot.varEWMA <- function(object, ..., assets=c(1,2), xlab="", ylab="Covariance", main="EWMA Estimate"){
  tmp = getCov(object, assets[1])
  plot(x=time(as.zoo(tmp)),y=tmp, ...=..., type="l", xlab=xlab, ylab=ylab, main=main)
  grid()
  abline(h=var(object$R), lwd=2, col="red")
}


# EWMA plotting for correlation
#' @method plot corEWMA
#' @S3method plot corEWMA
plot.corEWMA <- function(object, ..., assets=c(1,2), xlab="", ylab="Covariance", main="EWMA Estimate"){
  # Check if asset is a character 
  if(is.character(assets[1]) & is.character(assets[2])){
    idx1 = grep(assets[1], colnames(object$EWMA[[1]]))
    if(length(idx1) == 0) stop("name for asset1 not in object")
    idx2 = grep(assets[2], colnames(object$EWMA[[1]]))
    if(length(idx2) == 0) stop("name for asset2 not in object")
  } else {
    # Then dimensions are enough to find covar
    idx1 = assets[1]
    idx2 = assets[2]
  }
  tmp = getCor(object, assets)
  plot(x=time(as.zoo(tmp)), y=tmp, ...=..., type="l", xlab=xlab, ylab=ylab, main=main)
  grid()
  abline(h=cor(object$R)[idx1,idx2], lwd=2, col="red")
}

#' @method print EWMA
#' @S3method print EWMA
print.EWMA <- function(x, ...){
  n <- length(x$EWMA)
  cat("Final Period EWMA Estimate: ")
  cat(names(x$EWMA)[n], "\n")
  print(x$EWMA[[n]])
}
