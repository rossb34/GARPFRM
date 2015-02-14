
#' EWMA Model
#' 
#' EWMA model to estimate volatility, covariance, and correlation
#' 
#' If lambda=NULL, the lambda value can be estimated for univariate estimates 
#' of volatility,  covariance, and correlation by minimizing the mean 
#' squared error between the estimated value and realized value. 
#' 
#' @param R xts object of asset returns
#' @param lambda smoothing parameter, must be greater than 0 or less than 1. If
#' NULL, lambda will be estimated by minimizing the mean squared error
#' between the estimated value and the realized value.
#' @param initialWindow initial window of observations used in estimating the 
#' initial conditions
#' @param n number of periods used to calculate realized volatility, covariance, or correlation.
#' @param type estimate volatility, covariance, or correlation.
#' @return an EWMA object with the following elements
#' \itemize{
#'   \item \code{estimate} EWMA model estimated statistic
#'   \item \code{model} list with model parameters
#'   \item \code{data} list with original returns data and realized statistic if applicable
#' }
#' @examples
#' # data and parameters for EWMA estimate
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' mvR <- largecap_weekly[,1:4]
#' lambda <- 0.94
#' initialWindow <- 150
#' 
#' # volatility estimate of univariate data
#' lambda <- estimateLambdaVol(R[,1], initialWindow, n=10)
#' vol1 <- EWMA(R[,1], lambda=NULL, initialWindow, n=10, "volatility")
#' vol1a <- EWMA(R[,1], lambda, initialWindow, n=10, "volatility")
#' all.equal(vol1$estimate, vol1a$estimate)
#' vol1
#' 
#' # covariance estimate of bivariate data
#' lambda <- estimateLambdaCov(R, initialWindow, n=10)
#' cov1 <- EWMA(R, lambda=NULL, initialWindow, n=10, "covariance")
#' cov1a <- EWMA(R, lambda, initialWindow, n=10, "covariance")
#' all.equal(cov1$estimate, cov1a$estimate)
#' cov1
#' 
#' # correlation estimate of bivariate data
#' lambda <- estimateLambdaCor(R, initialWindow, n=10)
#' cor1 <- EWMA(R, lambda=NULL, initialWindow, n=10, "correlation")
#' cor1a <- EWMA(R, lambda, initialWindow, n=10, "correlation")
#' all.equal(cor1$estimate, cor1a$estimate)
#' cor1
#' 
#' # Multivariate EWMA estimate of covariance
#' lambda <- 0.94
#' cov_mv <- EWMA(mvR, lambda, initialWindow, type="covariance")
#' cov_mv
#' 
#' # Multivariate EWMA estimate of correlation
#' cor_mv <- EWMA(mvR, lambda, initialWindow, type="correlation")
#' cor_mv
#' @author Ross Bennett and Thomas Fillebeen
#' @export
EWMA <- function(R, lambda=0.94, initialWindow=10, n=10, type=c("volatility", "covariance", "correlation")){
  if(!is.xts(R)) stop("R must be an xts object")
  
  type <- match.arg(type)
  
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if(!is.null(lambda)){
    if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  }
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  
  # check if R is univariate, bivariate, or multivariate
  if(ncol(R) == 1){
    # univariate
    data <- "uv"
  } else if(ncol(R) == 2){
    # bivariate
    data <- "bv"
  } else {
    # multivariate
    data <- "mv"
  }
  
  realized <- NULL
  
  # univariate volatility estimate
  if((data == "uv") & (type == "volatility")){
    if(is.null(lambda)){
      lambda <- estimateLambdaVol(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAvol(R=R, lambda=lambda, initialWindow=initialWindow)
    realized <- realizedVol(R=R, n=n)
    class <- "uvEWMAvol"
  }
  
  # recursive EWMA call for multivariate data set
  # note that we are returning here
  if(((data == "mv") | (data == "bv")) & (type == "volatility")){
    out <- vector("list", ncol(R))
    for(i in 1:length(out)){
      out[[i]] <- EWMA(R=R[,i], lambda=lambda, initialWindow=initialWindow, n=n, type=type)
    }
    class(out) <- c("mvEWMAvol", "EWMA")
    return(out)
  }
  
  # bivariate covariance estimate
  if((data == "bv") & (type == "covariance")){
    if(is.null(lambda)){
      lambda <- estimateLambdaCov(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAcov(R=R, lambda=lambda, initialWindow=initialWindow)
    realized <- realizedCov(R=R, n=n)
    class <- "uvEWMAcov"
  }
  
  # bivariate correlation estimate
  if((data == "bv") & (type == "correlation")){
    if(is.null(lambda)){
      lambda <- estimateLambdaCor(R=R, initialWindow=initialWindow, n=n)
    }
    est <- uvEWMAcor(R=R, lambda=lambda, initialWindow=initialWindow)
    realized <- realizedCor(R=R, n=n)
    class <- "uvEWMAcor"
  }
  
  # multivariate covariance estimate
  if((data == "mv") & (type == "covariance")){
    if(is.null(lambda)){
      lambda <- 0.94
      warning("lambda must be specified for multivariate data")
    }
    est <- mvEWMAcov(R=R, lambda=lambda, initialWindow=initialWindow)
    class <- "mvEWMAcov"
  }
  
  # multivariate correlation estimate
  if((data == "mv") & (type == "correlation")){
    if(is.null(lambda)){
      lambda <- 0.94
      warning("lambda must be specified for multivariate data")
    }
    est <- mvEWMAcor(R=R, lambda=lambda, initialWindow=initialWindow)
    class <- "mvEWMAcor"
  }
  
  # put the parameters in a list
  parameters <- list(lambda=lambda, 
                     initialWindow=initialWindow,
                     type=type)
  
  # put the raw data and realized values in a list
  data <- list(R=R, realized_values=realized)
  
  # structure and return
  out <- structure(list(estimate=est,
                        model=parameters,
                        data=data), 
                   class=c(class, "EWMA"))
  return(out)
}


# univariate EWMA volatility estimate
uvEWMAvol <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA volatility estimates of univariate returns
  
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 1 column of data for univariate EWMA estimate
  if(ncol(R) > 1){
    warning("Only using first column of data")
    R <- R[,1]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = R[(initialWindow+1):nrow(R),]
  
  # Compute initial variance estimate
  oldVar <- var(initialR)
  oldR <- mean(initialR)
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update variance estimate
  for(i in 1:nrow(testR)){
    tmp[i] <- lambda * oldVar + (1 - lambda) * oldR^2
    oldVar <- tmp[i]
    oldR <- testR[i]
  }
  
  # Pad with leading NA and compute sqrt of variance vector
  out <- xts(c(rep(NA, initialWindow), sqrt(tmp)), index(R))
  colnames(out) <- colnames(R)
  return(out)
}

# univariate EWMA covariance estimate
uvEWMAcov <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA covariance estimates
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 2 columnw of data for bivariate EWMA estimate
  if(ncol(R) > 2){
    warning("Only using first two columns of data")
    R <- R[,1:2]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = coredata(R[(initialWindow+1):nrow(R),])
  
  # Compute initial estimate
  covOld <- cov(initialR[,1], initialR[,2])
  oldR1 <- mean(initialR[,1])
  oldR2 <- mean(initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update covariance estimate
  for(i in 1:nrow(testR)){ 
    tmp[i] <- covOld + (1 - lambda) * (oldR1 * oldR2  - covOld)
    covOld <- tmp[i]
    oldR1 <- testR[i, 1]
    oldR2 <- testR[i, 2]
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

# univariate EWMA correlation estimate
uvEWMAcor <- function(R, lambda=0.94, initialWindow=10){
  # function to compute EWMA correlation estimates of a bivariate dataset
  # Check for lambda between 0 and 1 & initialWindow must be greater than ncol(R)
  if ((lambda > 1) || (lambda < 0)) stop("lambda must be greater than 0 or less than 1")
  if (initialWindow > nrow(R)) stop("Initial window must be less than the number of observations")
  
  # R must be an xts object and have only 2 columns of data for bivariate EWMA estimate
  if(ncol(R) > 2){
    warning("Only using first two columns of data")
    R <- R[,1:2]
  }
  
  # Separate data into a initializing window and a testing window
  initialR = R[1:initialWindow,]
  testR = coredata(R[(initialWindow+1):nrow(R),])
  
  # Compute initial estimates
  covOld <- cov(initialR[,1], initialR[,2])
  varOld1 <- var(initialR[,1])
  varOld2 <- var(initialR[,2])
  oldR1 <- mean(initialR[,1])
  oldR2 <- mean(initialR[,2])
  
  # Initialize output vector
  tmp <- vector("numeric", nrow(testR))
  
  # Loop through and recursively update estimates
  for(i in 1:nrow(testR)){
    # Compute the covariance EWMA estimate
    tmpCov <- covOld + (1 - lambda) * (oldR1 * oldR2 - covOld)
    # Compute the variance EWMA estimate for each asset
    tmpVar1 <- lambda * varOld1 + (1 - lambda) * oldR1^2
    tmpVar2 <- lambda * varOld2 + (1 - lambda) * oldR2^2
    
    # Compute correlation with the covariance and volatility of each asset
    tmp[i] <- tmpCov / (sqrt(tmpVar1) * sqrt(tmpVar2))
    
    # Now update the old values
    covOld <- tmpCov
    varOld1 <- tmpVar1
    varOld2 <- tmpVar2
    oldR1 <- testR[i, 1]
    oldR2 <- testR[i, 2]
  }
  
  # Pad with leading NA
  out <- xts(c(rep(NA, initialWindow), tmp), index(R))
  colnames(out) <- paste(colnames(R), collapse=".")
  return(out)
}

# multivariate EWMA covariance estimate
# mvEWMAcov <- function(R, lambda, initialWindow){
#   # Separate data into a initializing window and a testing window
#   initialR = R[1:initialWindow,]
#   testR = R[(initialWindow+1):nrow(R),]
#   
#   # Initialize starting values
#   lagCov = cov(initialR)
#   oldR = as.numeric(colMeans(initialR))
#   
#   est = vector("list", nrow(testR))
#   for(i in 1:nrow(testR)){
#     est[[i]] = lambda * (oldR %*% t(oldR)) + (1 - lambda) * lagCov
#     # Update values from the current period
#     lagCov = est[[i]]
#     oldR = as.numeric(testR[i,])
#   }
#   # Properly assign list key to date
#   names(est) <- index(testR)
#   return(est)
# }

# multivariate EWMA covariance estimate
mvEWMAcov <- function(R, lambda, initialWindow){
  # This computes the covariance matrix for the final time period
  cnames <- colnames(R)
  n <- ncol(R)
  mat <- matrix(0, n, n)
  rownames(mat) <- cnames
  colnames(mat) <- cnames
  for(i in 1:n){
    for(j in i:n){
      # EWMA covariance estimate
      tmpCov <- uvEWMAcov(cbind(R[,i], R[,j]), lambda, initialWindow)
      mat[i,j] <- as.numeric(last(tmpCov))
      if(i != j){
        mat[j,i] <- as.numeric(last(tmpCov))
      }
    }
  }
  return(mat)
}

# multivariate EWMA correlation estimate
# mvEWMAcor <- function(R, lambda, initialWindow){
#   # Separate data into a initializing window and a testing window
#   initialR = R[1:initialWindow,]
#   testR = R[(initialWindow+1):nrow(R),]
#   
#   # Initialize starting values
#   lagCov = cov(initialR)
#   oldR = as.numeric(colMeans(initialR))
#   
#   est = vector("list", nrow(testR))
#   for(i in 1:nrow(testR)){
#     # est[[i]] = lambda * (oldR %*% t(oldR)) + (1 - lambda) * lagCov
#     tmp = lambda * (oldR %*% t(oldR)) + (1 - lambda) * lagCov
#     # Update values from the current period
#     # lagCov = est[[i]]
#     est[[i]] <- cov2cor(tmp)
#     lagCov <- tmp
#     oldR = as.numeric(testR[i,])
#   }
#   # Properly assign list key to date
#   names(est) <- index(testR)
#   return(est)
# }

# multivariate EWMA correlation estimate
mvEWMAcor <- function(R, lambda, initialWindow){
  cov2cor(mvEWMAcov(R, lambda, initialWindow))
}

#' Realized Volatility
#' 
#' Calculate realized volatility
#' 
#' Realized volatility is defined as the standard deviation of using the 
#' previous \code{n} periods.
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' @return xts object of realized volatility
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1]
#' # Calculate realized volatility
#' realizedVolatility <- realizedVol(R[,1], 10)
#' head(realizedVolatility)
#' @author Ross Bennett
#' @export
realizedVol <- function(R, n){
  n <- as.integer(n)[1]
  lag(rollSD(R=R[,1], width=n))
}

#' Realized Covariance
#' 
#' Calculate realized covariance
#' 
#' Realized covariance is calculated using the previous \code{n} periods.
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' @return xts object of realized covariance
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' # Calculate realized covariance
#' realizedCovariance <- realizedCov(R, 10)
#' head(realizedCovariance)
#' @author Ross Bennett
#' @export
realizedCov <- function(R, n){
  n <- as.integer(n)[1]
  lag(rollCov(R=R, width=n))
}

#' Realized Correlation
#' 
#' Calculate realized correlation
#' 
#' Realized correlation is calculated using the previous \code{n} periods.
#' 
#' @param R xts object of asset returns
#' @param n number of periods used to calculate realized volatility
#' @return xts object of realized correlation
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' # Calculate realized correlation
#' realizedCorrelation <- realizedCor(R, 10)
#' head(realizedCorrelation)
#' @author Ross Bennett
#' @export
realizedCor <- function(R, n){
  n <- as.integer(n)[1]
  lag(rollCor(R=R, width=n))
}

# objective function for calculating lambda
# objective is the mean squared error between estimated volatility and 
# realized volatility
objLambdaVol <- function(lambda, R, initialWindow, n){
  R <- R[,1]
  realized <- realizedVol(R, n)
  est <- uvEWMAvol(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- mean((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

# objective function for calculating lambda
# objective is the mean squared error between estimated covariance and 
# realized covariance
objLambdaCov <- function(lambda, R, initialWindow, n){
  realized <- realizedCov(R, n)
  est <- uvEWMAcov(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- mean((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

# objective function for calculating lambda
# objective is the mean squared error between estimated correlation and 
# realized correlation
objLambdaCor <- function(lambda, R, initialWindow, n){
  realized <- realizedCor(R, n)
  est <- uvEWMAcor(R, lambda, initialWindow)
  tmpDat <- na.omit(cbind(est, realized))
  obj <- mean((tmpDat[,1] - tmpDat[,2])^2)
  return(obj)
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA volatility estimate
#' 
#' The optimal value for lambda is calcualted by minimizing the mean squared
#' error between the estimated volatility and realized volatility.
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized volatility
#' 
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1]
#' initialWindow <- 150
#' lambda <- estimateLambdaVol(R, initialWindow, n=10)
#' @author Ross Bennett
#' @export
estimateLambdaVol <- function(R, initialWindow=10, n=10){
  if(!is.xts(R)) stop("R must be an xts object")
  if(ncol(R) > 1) warning("lambda to estimate volatility only supported for univariate data (i.e. ncol(R) == 1)")
  
  opt <- optimize(objLambdaVol, interval=c(0,1), R=R[,1], 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA covariance estimate
#' 
#' The optimal value for lambda is calcualted by minimizing the mean squared
#' error between the estimated covariance and realized covariance.
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized covariance
#' 
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' initialWindow <- 150
#' lambda <- estimateLambdaCov(R, initialWindow, n=10)
#' @author Ross Bennett
#' @export
estimateLambdaCov <- function(R, initialWindow=10, n=10){
  if(!is.xts(R)) stop("R must be an xts object")
  if(ncol(R) > 2) warning("lambda to estimate covariance only supported for bivariate data (i.e. ncol(R) == 2)")
  
  opt <- optimize(objLambdaCov, interval=c(0,1), R=R[,1:2], 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' Estimated Lambda
#' 
#' Estimate lambda for EWMA correlation estimate
#' 
#' The optimal value for lambda is calcualted by minimizing the mean squared
#' error between the estimated correlation and realized correlation.
#' 
#' @param R xts object of asset returns
#' @param initialWindow initial window of observations used in estimating the 
#' initial
#' @param n number of periods used to calculate realized correlation
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' initialWindow <- 150
#' lambda <- estimateLambdaCor(R, initialWindow, n=10)
#' @author Ross Bennett
#' @export
estimateLambdaCor <- function(R, initialWindow=10, n=10){
  if(!is.xts(R)) stop("R must be an xts object")
  if(ncol(R) > 2) warning("lambda to estimate covariance only supported for bivariate data (i.e. ncol(R) == 2)")
  
  opt <- optimize(objLambdaCor, interval=c(0,1), R=R[,1:2], 
                  initialWindow=initialWindow, n=n, 
                  tol=.Machine$double.eps)
  lambda <- opt$minimum
  lambda
}

#' @method print EWMA
#' @S3method print EWMA
print.EWMA <- function(x, ...){
  cat("EWMA Estimate\n\n")
  
  cat("Parameters\n")
  cat("lambda: ", x$model$lambda, "\n", sep="") 
  cat("initialWindow: ", x$model$initialWindow, "\n", sep="")
  cat("type: ", x$model$type, "\n\n", sep="")
  
  cat("Final Period EWMA Estimate: \n")
  if(inherits(x, "mvEWMAcov") | inherits(x, "mvEWMAcor")){
    print(x$estimate)
  } else {
    print(last(x$estimate))
  }
}

#' @method print mvEWMAvol
#' @S3method print mvEWMAvol
print.mvEWMAvol <- function(x, ...){
  for(i in 1:length(x)){
    print(x[[i]], ...=...)
    cat("\n*****\n")
  }
}

# extract the covariance between two assets from an mvEWMAcov object

#' EWMA Covariance
#' 
#' Extract the covariance of two assets from an \code{mvEWMAcov} object
#' 
#' @param EWMA an EWMA object created by \code{\link{EWMA}}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @examples
#' data(crsp_weekly)
#' mvR <- largecap_weekly[,1:4]
#' lambda <- 0.94
#' initialWindow <- 150
#' cov_mv <- EWMA(mvR, lambda, initialWindow, type="covariance")
#' # Extract the estimated covariance between ORCL and MSFT
#' tail(getCov(cov_mv, assets=c("ORCL", "MSFT")))
#' @author Ross Bennett and Thomas Fillebeen
#' @export
getCov <- function(EWMA, assets){
  UseMethod("getCov")
}

#' @method getCov mvEWMAcov
#' @S3method getCov mvEWMAcov
getCov.mvEWMAcov <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "mvEWMAcov")) stop("object must be of class mvEWMAcov")
  
  if(length(assets) == 1) assets[2] <- assets[1]
  
  # cnames <- colnames(EWMA$estimate[[1]])
  cnames <- colnames(EWMA$estimate)
  R <- EWMA$data$R
  lambda <- EWMA$model$lambda
  initialWindow <- EWMA$model$initialWindow
  
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
  #out = xts(unlist(lapply(EWMA$estimate, function(X) X[idx1, idx2])), as.Date(names(EWMA$estimate)))
  #colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  # estimate the EWMA cov between the assets specified
  out <- uvEWMAcov(R=R[,c(idx1,idx2)], lambda=lambda, initialWindow=initialWindow)
  return(out)
}


# # extract the variance from an mvEWMAcov object
# # EWMA Variance
# # 
# # Extract the Variance of an asset from an \code{mvEWMAcov} object
# # 
# # @param object an EWMA object created by \code{EWMA}
# # @param assets character vector or numeric vector. The assets can be 
# # specified by name or index.
# # @export
# getVar <- function(EWMA, assets){
#   UseMethod("getVar")
# }
# 
# # @method getCov mvEWMAcov
# # @S3method getCov mvEWMAcov
# getVar.mvEWMAcov <- function(EWMA, assets=1){
#   if(!inherits(EWMA, "mvEWMAcov")) stop("EWMA must be of class mvEWMAcov")
#   
#   cnames <- colnames(EWMA$estimate[[1]])
#   
#   # Check if asset is a character
#   if(is.character(assets[1])){
#     idx1 = grep(assets[1], cnames)
#     if(length(idx1) == 0) stop("name for asset not in EWMA object")
#   } else {
#     idx1 = assets[1]
#   }
#   out = xts(unlist(lapply(EWMA$estimate, function(x) x[idx1])), as.Date(names(EWMA$estimate)))
#   colnames(out) = cnames[idx1]
#   return(out)
# }

# extract the correlation between two assets from an mvEWMAcor object
#' EWMA Correlation
#' 
#' Extract the correlation of two assets from an \code{mvEWMAcor} object
#' 
#' @param EWMA an EWMA object created by \code{\link{EWMA}}
#' @param assets character vector or numeric vector. The assets can be 
#' specified by name or index.
#' @examples
#' data(crsp_weekly)
#' mvR <- largecap_weekly[,1:4]
#' lambda <- 0.94
#' initialWindow <- 150
#' cor_mv <- EWMA(mvR, lambda, initialWindow, type="correlation")
#' # Extract the estimated correlation between ORCL and MSFT
#' tail(getCor(cor_mv, assets=c("ORCL", "MSFT")))
#' @author Ross Bennett and Thomas Fillebeen
#' @export
getCor <- function(EWMA, assets){
  UseMethod("getCor")
}

#' @method getCor mvEWMAcor
#' @S3method getCor mvEWMAcor
getCor.mvEWMAcor <- function(EWMA, assets=c(1,2)){
  if(!inherits(EWMA, "mvEWMAcor")) stop("object must be of class mvEWMAcor")
  
  if(length(assets) == 1) assets[2] <- assets[1]
  
  # cnames <- colnames(EWMA$estimate[[1]])
  cnames <- colnames(EWMA$estimate)
  R <- EWMA$data$R
  lambda <- EWMA$model$lambda
  initialWindow <- EWMA$model$initialWindow
  
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
  #out = xts(unlist(lapply(EWMA$estimate, function(X) X[idx1, idx2])), as.Date(names(EWMA$estimate)))
  #colnames(out) = paste(cnames[idx1], cnames[idx2], sep=".")
  # estimate the EWMA cov between the assets specified
  out <- uvEWMAcor(R=R[,c(idx1,idx2)], lambda=lambda, initialWindow=initialWindow)
  return(out)
}

#' Plot EWMA Model Estimates
#' 
#' Plot method for EWMA objects.
#' 
#' @param x an EWMA object created via \code{\link{EWMA}}.
#' @param y not used.
#' @param \dots passthrough parameters to \code{plot.xts}.
#' @param assets character vector or numeric vector of assets to extract from 
#' the covariance or correlation matrix. The assets can be specified by name or 
#' index. This argument is only usd for multivariate EWMA estimates of 
#' a covariance or correlation matrix.
#' @param legendLoc location of legend. If NULL, the legend will be omitted 
#' from the plot.
#' @param main main title for the plot.
#' @param legendCex numerical value giving the amount by which the legend.
#' @examples
#' # data and parameters for EWMA estimate
#' data(crsp_weekly)
#' R <- largecap_weekly[, 1:2]
#' mvR <- largecap_weekly[,1:4]
#' lambda <- 0.94
#' initialWindow <- 150
#' 
#' # volatility estimate of univariate data
#' vol1 <- EWMA(R[,1], lambda, initialWindow, type="volatility")
#' plot(vol1)
#' 
#' # covariance estimate of bivariate data
#' cov1 <- EWMA(R, lambda, initialWindow, type="covariance")
#' plot(cov1)
#' 
#' # correlation estimate of bivariate data
#' cor1 <- EWMA(R, lambda, initialWindow, type="correlation")
#' plot(cor1)
#' 
#' # Multivariate EWMA estimate of covariance
#' cov_mv <- EWMA(mvR, lambda, initialWindow, type="covariance")
#' # These two are equivalent
#' plot(cov_mv, assets=c("ORCL", "MSFT"))
#' plot(cov_mv, assets=c(1, 2))
#' 
#' # Multivariate EWMA estimate of correlation
#' cor_mv <- EWMA(mvR, lambda, initialWindow, type="correlation")
#' # These two are equivalent
#' plot(cor_mv, assets=c("ORCL", "EMC"))
#' plot(cor_mv, assets=c(1, 4))
#' @author Ross Bennett
#' @method plot EWMA
#' @S3method plot EWMA
plot.EWMA <- function(x, y=NULL, ..., assets=c(1,2), legendLoc=NULL, main="EWMA Estimate", legendCex=0.8){
  type <- x$model$type
  if(inherits(x, "uvEWMAvol") | inherits(x, "uvEWMAcov") | inherits(x, "uvEWMAcor")){
    # all uvEWMA* objects have same format
    estValues <- x$estimate
  } else if(inherits(x, "mvEWMAcov") | inherits(x, "mvEWMAcor")){
    # the covariance of two assets must be extracted
    if(inherits(x, "mvEWMAcov")){
      estValues <- getCov(x, assets)
    }
    # the correlation of two assets must be extracted
    if(inherits(x, "mvEWMAcor")){
      estValues <- getCor(x, assets)
    }
  } else if(inherits(x, "mvEWMAvol")){
    estValues <- getEstimate(x)
  }
  if(inherits(x, "mvEWMAvol")){
    cnames <- colnames(estValues)
    ylim <- range(na.omit(estValues))
    plot.xts(estValues[,1], ...=..., ylim=ylim, type="n", ylab="volatility", main=main)
    for(i in 1:ncol(estValues)){
      lines(estValues[,i], col=i)
    }
    if(!is.null(legendLoc)){
      legend(legendLoc, legend=cnames, 
             lty=rep(1, ncol(estValues)), col=1:ncol(estValues), bty="n", cex=legendCex)
    }
  } else {
    # plot the values
    plot.xts(x=estValues, ...=..., type="l", ylab=type, main=main)
    if(!is.null(legendLoc)){
      legend(legendLoc, legend=colnames(estValues), 
             lty=1, col="black", bty="n", cex=legendCex)
    }
  }
}

#' Get Estimated Values
#' 
#' Get the estimated values from the model
#' 
#' @param model fitted model (currently only EWMA)
#' @param \dots passthrough parameters (not currently used)
#' @return model estimate
#' @author Ross Bennett
#' @export
getEstimate <- function(model, ...){
  UseMethod("getEstimate")
}

#' @method getEstimate EWMA
#' @S3method getEstimate EWMA
getEstimate.EWMA <- function(model, ...){
  model$estimate
}

#' @method getEstimate mvEWMAvol
#' @S3method getEstimate mvEWMAvol
getEstimate.mvEWMAvol <- function(model, ...){
  do.call(cbind, lapply(model, FUN=function(x) x$estimate))
}
