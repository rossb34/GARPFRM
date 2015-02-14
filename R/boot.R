
#' Bootstrap
#' 
#' Bootstrap a function
#' 
#' @details
#' \code{R} is the data passed to \code{FUN}. \code{FUN} must have \code{x} or
#' \code{R} as arguments for the data. For example, see the functions linked to
#' in the 'See Also' section. Care must be taken when using \code{bootFUN} on
#' multivariate data. This function is designed to only accept univariate 
#' (i.e. ncol(R) = 1) data, however is made to work with bivariate data for
#' \code{bootCor} and \code{bootCov}. For multivariate data, a wrapper function
#' should be written to apply the bootstrap function to each column of data.
#' 
#' @param R xts object or matrix of data passed to \code{FUN}.
#' @param FUN the function to be applied.
#' @param \dots optional arguments to \code{FUN}.
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @seealso \code{\link{bootMean}}, \code{\link{bootSD}}, \code{\link{bootStdDev}},
#' \code{\link{bootSimpleVolatility}}, \code{\link{bootCor}}, \code{\link{bootCov}},
#' \code{\link{bootVaR}}, \code{\link{bootES}}
#' @export
bootFUN <- function(R, FUN="mean", ..., replications=1000){
  parallel=FALSE
  # R should be a univariate xts object
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  fun <- match.fun(FUN)
  if(!is.function(fun)) stop("FUN could not be matched")
  
  if(is.function(fun)){
    .formals <- formals(fun)
    # add the dots
    .formals <- modify.args(formals=.formals, ...=..., dots=TRUE)
    if("portfolio_method" %in% names(.formals)){ 
      .formals$portfolio_method <- "single"
    }
    .formals$... <- NULL
  }
  # print(.formals)
  
  replications <- as.integer(replications)
  n <- nrow(R)
  
  if(parallel){
    stopifnot("package:foreach" %in% search() || require("foreach",quietly = TRUE))
    stopifnot("package:iterators" %in% search() || require("iterators",quietly = TRUE))
    out <- foreach(i=icount(replications), .inorder=FALSE, .combine=c, .errorhandling='remove', .packages="GARPFRM") %dopar% {
      #tmpR <- R[sample.int(n, replace=TRUE),]
      # match the resampled data to R or x in .formals
      if("R" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, R=R[sample.int(n, replace=TRUE),], dots=TRUE)
      } else if("x" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, x=R[sample.int(n, replace=TRUE),], dots=TRUE)
      }
      do.call(fun, .formals)
    }
  } else {
    out <- vector("numeric", replications)
    for(i in 1:replications){
      # sampled data
      #tmpR <- R[sample.int(n, replace=TRUE),]
      # match the resampled data to R or x in .formals
      if("R" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, R=R[sample.int(n, replace=TRUE),], dots=TRUE)
      } else if("x" %in% names(.formals)){ 
        .formals <- modify.args(formals=.formals, arglist=NULL, x=R[sample.int(n, replace=TRUE),], dots=TRUE)
      }
      # call the function
      tmp <- try(do.call(fun, .formals), silent=TRUE)
      
      # if try-error, stop the function call, else insert the resampled statistic
      # to the output vector
      if(inherits(tmp, "try-error")){
        stop("FUN could not be evaluated")
      } else {
        out[i] <- tmp
      }
    }
  }
  # compute the expected value of the statistic on resampled data
  estimate <- mean(out)
  # compute the standard error of the bootstrap estimate
  std.err <- sd(out)
  matrix(c(estimate, std.err), nrow=2, ncol=1, dimnames=list(c(FUN, "std.err")))
}

.bootMean <- function(R, ..., replications=1000){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="mean", ...=..., replications=replications)
}

#' Bootstrap Mean
#' 
#' Bootstrap the mean of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[base]{mean}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootMean(R[,1])
#' bootMean(R)
#' @seealso \code{\link[base]{mean}}
#' @export
bootMean <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootMean(R=R, ...=..., replications=replications)
  } else {
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootMean(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootMean(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  #out <- matrix(tmp, nrow=1, ncol=ncol(R))
  rownames(out) <- c("mean", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

.bootSD <- function(R, ..., replications=1000){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="sd", ...=..., replications=replications)
}

#' Bootstrap Standard Deviation
#' 
#' Bootstrap the standard deviation of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{sd}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootSD(R[,1])
#' bootSD(R)
#' @seealso \code{\link[stats]{sd}}
#' @export
bootSD <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootSD(R=R, ...=..., replications=replications)
  } else {
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootSD(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootSD(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  rownames(out) <- c("sd", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

.bootStdDev <- function(R, ..., replications=1000){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="StdDev", ...=..., replications=replications)
}

#' Bootstrap StdDev
#' 
#' Bootstrap the StdDev of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{StdDev}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootStdDev(R[,1])
#' bootStdDev(R)
#' @seealso \code{\link[PerformanceAnalytics]{StdDev}}
#' @export
bootStdDev <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootStdDev(R=R, ...=..., replications=replications)
  } else {
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootStdDev(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootStdDev(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  rownames(out) <- c("StdDev", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

.bootSimpleVolatility <- function(R, ..., replications=1000){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="simpleVolatility", ...=..., replications=replications)
}

#' Bootstrap Simple Volatility
#' 
#' Bootstrap the simple volatility of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link{simpleVolatility}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootSimpleVolatility(R[,1])
#' bootSimpleVolatility(R)
#' @seealso \code{\link{simpleVolatility}}
#' @export
bootSimpleVolatility <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootSimpleVolatility(R=R, ...=..., replications=replications)
  } else {
    tmp <- vector("numeric", ncol(R))
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootSimpleVolatility(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootSimpleVolatility(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  rownames(out) <- c("SimpleVolatility", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

tmpCor <- function(R, ...){
  # R should be a bivariate xts object
  cor(x=R[,1], y=R[,2], ...)
}

.bootCor <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a bivariate xts object
  bootFUN(R=R[,1:2], FUN="tmpCor", ...=..., replications=replications)
}

#' Bootstrap Correlation
#' 
#' Bootstrap the correlation of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{cor}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootCor(R[,1:2])
#' bootCor(R[,1:2], method="kendall")
#' bootCor(R)
#' @seealso \code{\link[stats]{cor}}
#' @export
bootCor <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) < 2) stop("R must have 2 or more columns of asset returns")
  cnames <- colnames(R)
  if(ncol(R) == 2){
    out <- .bootCor(R=R, ...=..., replications=replications)
    out_names <- paste(cnames[1], cnames[2], sep=".")
    num_col <- 1
  } else {
    out_names <- vector("numeric", choose(ncol(R), 2))
    num_col <- length(out_names)
    k <- 1
    for(i in 1:(ncol(R)-1)){
      for(j in (i+1):ncol(R)){
        if(k == 1){
          out <- .bootCor(R=cbind(R[,i], R[,j]), ...=..., replications=replications)
        } else {
          out <- cbind(out, .bootCor(R=cbind(R[,i], R[,j]), ...=..., replications=replications))
        }
        out_names[k] <- paste(cnames[i], cnames[j], sep=".")
        k <- k + 1
      }
    }
  }
  rownames(out) <- c("cor", "std.err")
  colnames(out) <- out_names
  return(out)
}

tmpCov <- function(R, ...){
  # R should be a bivariate xts object
  cov(x=R[,1], y=R[,2], ...)
}

.bootCov <- function(R, ..., replications=1000){
  # R should be a bivariate xts object
  bootFUN(R=R[,1:2], FUN="tmpCov", ...=..., replications=replications)
}

#' Bootstrap Covariance
#' 
#' Bootstrap the covariance of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[stats]{cov}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootCov(R[,1:2])
#' bootCov(R)
#' @seealso \code{\link[stats]{cov}}
#' @export
bootCov <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  cnames <- colnames(R)
  if(ncol(R) == 2){
    out <- .bootCov(R=R, ...=..., replications=replications)
    out_names <- paste(cnames[1], cnames[2], sep=".")
    num_col <- 1
  } else {
    out_names <- vector("numeric", choose(ncol(R), 2))
    num_col <- length(out_names)
    k <- 1
    for(i in 1:(ncol(R)-1)){
      for(j in (i+1):ncol(R)){
        if(k == 1){
          out <- .bootCov(R=cbind(R[,i], R[,j]), ...=..., replications=replications)
        } else {
          out <- cbind(out, .bootCov(R=cbind(R[,i], R[,j]), ...=..., replications=replications))
        }
        out_names[k] <- paste(cnames[i], cnames[j], sep=".")
        k <- k + 1
      }
    }
  }
  rownames(out) <- c("cov", "std.err")
  colnames(out) <- out_names
  return(out)
}

.bootVaR <- function(R, ..., replications=1000, parallel=FALSE){
  # R should be a univariate xts object
  bootFUN(R=R[,1], FUN="VaR", ...=..., replications=replications)
}

#' Bootstrap Value at Risk
#' 
#' Bootstrap the Value at Risk (VaR) of an xts object or matrix  of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{VaR}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootVaR(R[,1], p=0.9, method="historical")
#' bootVaR(R[,1], p=0.9, method="gaussian")
#' bootVaR(R, p=0.9, method="historical", invert=FALSE)
#' @seealso \code{\link[PerformanceAnalytics]{VaR}}
#' @export
bootVaR <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootVaR(R=R, ...=..., replications=replications)
  } else {
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootVaR(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootVaR(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  rownames(out) <- c("VaR", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

.bootES <- function(R, ..., replications=1000){
  # R should be a univariate xts object
  bootFUN(R=R, FUN="ES", ...=..., replications=replications)
}

#' Bootstrap Expected Shortfall
#' 
#' Bootstrap the Expected Shortfall (ES) of an xts object or matrix of asset returns
#' 
#' @param R xts object or matrix of asset returns
#' @param \dots passthrough parameters to \code{\link[PerformanceAnalytics]{ES}}
#' @param replications number of bootstrap replications.
#' @author Ross Bennett
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' bootVaR(R[,1], p=0.9, method="historical")
#' bootVaR(R[,1], p=0.9, method="gaussian")
#' bootVaR(R, p=0.9, method="historical", invert=FALSE)
#' @seealso \code{\link[PerformanceAnalytics]{ES}}
#' @export
bootES <- function(R, ..., replications=1000){
  if(!is.matrix(R) | !is.xts(R)) stop("R must be an xts or matrix")
  
  if(ncol(R) == 1){
    out <- .bootES(R=R, ...=..., replications=replications)
  } else {
    for(i in 1:ncol(R)){
      if(i == 1){
        out <- .bootES(R=R[,i], ...=..., replications=replications)
      } else {
        out <- cbind(out, .bootES(R=R[,i], ...=..., replications=replications))
      }
    }
  }
  rownames(out) <- c("ES", "std.err")
  colnames(out) <- colnames(R)
  return(out)
}

