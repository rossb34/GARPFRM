# source code for efficient frontier

#' Portfolio Return for a Portfolio of Two Assets
#' 
#' Calculate the portfolio return for a portfolio of two assets
#' 
#' This is a specialized function to calculate the return of a portfolio of two
#' assets following the equations presented in 
#' Chapter 3: Delineating Efficient Portfolios.
#' 
#' @param R1 expected return for asset 1
#' @param R2 expected return for asset 2
#' @param X1 fraction of portfolio invested in asset 1
#' @return portfolio expected return
#' @export
portReturnTwoAsset <- function(R1, R2, X1){
  out <- X1 * R1 + (1 - X1) * R2
  return(out)
}

#' Portfolio Standard Deviation for a Portfolio of Two Assets
#' 
#' Calculate the standard deviation for a portfolio of two assets
#' 
#' This is a specialized function to calculate the standard deviation of a 
#' portfolio of two assets following the equations presented in 
#' Chapter 3: Delineating Efficient Portfolios.
#' 
#' @param R1 expected return for asset 1
#' @param R2 expected return for asset 2
#' @param X1 fraction of portfolio invested in asset 1
#' @param sigma1 standard deviation for asset 1
#' @param sigma2 standard deviation for asset 2
#' @param rho correlation coefficient between asset 1 and asset 2
#' @return portfolio expected return
#' @export
portSDTwoAsset <- function(R1, R2, X1, sigma1, sigma2, rho){
  out <- sqrt(X1^2 * sigma1^2 + (1 - X1)^2 * sigma2^2 + 
                2 * X1 * (1 - X1) * rho * sigma1 * sigma2)
  return(out)
}

#' Efficient Frontier for Portfolio of Two Assets
#' 
#' Create an efficient frontier for a portfolio of two risky assets
#' 
#' This is a specialized function to generate points along the efficient
#' frontier for a portfolio of two assets following the equations presented in 
#' Chapter 3: Delineating Efficient Portfolios.
#' 
#' @param R1 expected return for asset 1
#' @param R2 expected return for asset 2
#' @param sigma1 standard deviation for asset 1
#' @param sigma2 standard deviation for asset 2
#' @param rho correlation coefficient between asset 1 and asset 2
#' @param nPortfolios number of portfolios to generate along efficient frontier
#' @param rf risk free rate
#' @param allowShorting TRUE/FALSE to allow short sales
#' @param weights vector of weights for fraction of asset 1 used to generate
#' the efficient frontier. If the \code{weights} argument is specified, the
#' \code{nPortfolios} and \code{allowShorting} arguments are ignored.
#' @export
efficientFrontierTwoAsset <- function(R1, R2, sigma1, sigma2, rho, nPortfolios=25, rf=0, allowShorting=FALSE, weights=NULL){
  # fraction of portfolio invested in asset 1
  if(is.null(weights)){
    if(allowShorting){
      X1 <- seq(from=-1, to=2, length.out=nPortfolios)
    } else {
      X1 <- seq(from=0, to=1, length.out=nPortfolios)
    }
  } else {
    X1 <- weights
  }
  X2 <- 1 - X1

  # Calculate the portfolio return
  R_P <- portReturnTwoAsset(R1, R2, X1)
  
  # Calculate the portfolio standard deviation
  sigma_P <- portSDTwoAsset(R1, R2, X1, sigma1, sigma2, rho)
  
  # Combine the return, sd, and weights in a matrix for the efficient frontier
  ef <- cbind(R_P, sigma_P, X1, X2)
  colnames(ef) <- c("portfolio_return", "portfolio_sd", "X1", "X2")
  
  ret <- c(R1, R2)
  names(ret) <- c("Asset 1", "Asset 2")
  
  stdev <- c(sigma1, sigma2)
  names(stdev) <- c("Asset 1", "Asset 2")
  
  parameters <- list()
  parameters$returns <- ret
  parameters$standard_deviation <- stdev
  parameters$rho <- rho
  parameters$risk_free_rate <- rf
  
  return(structure(list(parameters=parameters, 
                        efficient_frontier=ef), 
                   class="efTwoAsset"))
}

#' Minimum Variance Portfolio
#' 
#' Extract the minimum variance portfolio from an efficient frontier
#' 
#' Note that the values for the minimum variance portfolio are an approximation 
#' and depend on the number of portfolios used to create the efficient frontier.
#' 
#' @param ef efficient frontier object created via \code{\link{efficientFrontierTwoAsset}}
#' @seealso \code{\link{efficientFrontierTwoAsset}}
#' @export
minVarPortfolio <- function(ef){
  if(!inherits(ef, "efTwoAsset")) stop("ef must be of class 'efTwoAsset'")
  ef <- ef$efficient_frontier
  ef[which.min(ef[,2]),]
}

#' Tangent Portfolio
#' 
#' Extract the tangent portfolio from an efficient frontier. The tangent 
#' portfolio is the portfolio which maximizes risk adjusted return.
#' 
#' Note that the values for the tangent portfolios are an approximation and 
#' depend on the number of portfolios used to create the efficient frontier.
#' 
#' @param ef efficient frontier object created via \code{\link{efficientFrontierTwoAsset}}
#' @seealso \code{\link{efficientFrontierTwoAsset}}
#' @export
tangentPortfolio <- function(ef){
  if(!inherits(ef, "efTwoAsset")) stop("ef must be of class 'efTwoAsset'")
  rf <- ef$parameters$risk_free_rate
  ef <- ef$efficient_frontier
  tangent <- (ef[,1] - rf) / ef[,2]
  ef[which.max(tangent),]
}

#' Efficient Frontier Plot
#' 
#' Plot the efficient frontier in return - standard deviation space
#' 
#' @param x object of class \code{efTwoAsset} created via \code{\link{efficientFrontierTwoAsset}}
#' @param y not used
#' @param \dots passthrough parameters
#' @param chartAssets TRUE/FALSE to include the assets in the plot
#' @param main a main title for the plot
#' @param xlim set the x-axis limit, same as in \code{plot}
#' @param ylim set the x-axis limit, same as in \code{plot}
#' @param type what type of plot should be drawn, same as in \code{plot}
#' @param pchAssets plotting character of the assets
#' @param cexAssets numerical value giving the amount by which the asset points
#' and labels should be magnified relative to the default.
#' @method plot efTwoAsset
#' @S3method plot efTwoAsset
#' @export
plot.efTwoAsset <- function(x, y, ..., chartAssets=TRUE, main="Efficient Frontier", xlim=NULL, ylim=NULL, type="l", pchAssets=19, cexAssets=0.8){
  
  # extract some of the parameters
  returns <- x$parameters$returns
  sigmas <- x$parameters$standard_deviation
  
  # Extract the efficient frontier matrix
  ef <- x$efficient_frontier
  R_P <- ef[,1]
  sigma_P <- ef[,2]
  
  # Extract the minimum variance portfolio
  minvar <- minVarPortfolio(x)
  
  # Extract the tangent portfolio
  tangent <- tangentPortfolio(x)
  if(is.null(xlim)){
    xlim <- c(0, max(sigma_P) * 1.15)
  }
  
  if(is.null(ylim)){
    ylim <- c(0, max(R_P) * 1.15)
  }
  
  plot(sigma_P, R_P, ...=..., main=main, 
       xlim=xlim, ylim=ylim, type=type,
       xlab="Portfolio Standard Deviation", 
       ylab="Portfolio Expected Return", 
       cex.lab=0.8)
  
  # Add the minimum variance portfolio to the plot
  points(x=minvar[2], y=minvar[1], pch=15)
  text(x=minvar[2], y=minvar[1], labels="MV", cex=0.8, pos=2)
  
  # Add the tangent portfolio to the plot
  points(x=tangent[2], y=tangent[1], pch=17)
  text(x=tangent[2], y=tangent[1], labels="T", cex=0.8, pos=3)
  
  # Add the tangent line
  rf <- x$parameters$risk_free_rate
  slope <- try((tangent[1] - rf) / tangent[2], silent=TRUE)
  if(!is.infinite(slope)) abline(a=rf, b=slope)
  
  if(chartAssets){
    points(c(sigmas[1], sigmas[2]), c(returns[1], returns[2]), pch=pchAssets)
    text(c(sigmas[1], sigmas[2]), c(returns[1], returns[2]), cex=cexAssets,
         labels=c("Asset 1", "Asset 2"), pos=4)
  }
  
  legend("topleft", legend=c(paste("correlation = ", x$parameters$rho),
                             paste("risk free rate = ", x$parameters$risk_free_rate),
                             "MV: Minimum Variance Portfolio",
                             "T: Tangent Portfolio"), 
         pch=c(NA, NA, 15, 17), bty="n", cex=0.75)
}

#' Efficient Frontier
#' 
#' Generate portfolios along an efficient frontier.
#' 
#' This is a wrapper function for code in PortfolioAnalytics to initialize a 
#' portfolio and create an efficint frontier in mean - standard deviation space.
#' Box constraints and group constraints are supported for constrained
#' optimization to generate portfolios along the efficient frontier.  
#' 
#' @param R xts object of asset returns
#' @param nPortfolios number of portfolios to generate along efficient frontier
#' @param minBox box constraint minimum
#' @param maxBox box constraint maximum
#' @param groupList list specifying asset groups
#' @param groupMin group constraint minimum
#' @param groupMax group constraint maximum
#' @export
efficientFrontier <- function(R, nPortfolios=25, minBox=0, maxBox=1, groupList=NULL, groupMin=NULL, groupMax=NULL){
  stopifnot("package:PortfolioAnalytics" %in% search() || require("PortfolioAnalytics", quietly = TRUE))
  
  # Initialize the portfolio and create an efficient frontier
  funds <- colnames(R)
  init <- portfolio.spec(assets=funds)
  # initial constraints
  init <- add.constraint(portfolio=init, type="weight_sum", min_sum=1, max_sum=1)
  init <- add.constraint(portfolio=init, type="box", min=minBox, max=maxBox)
  if(!is.null(groupList) & !is.null(groupMin) & !is.null(groupMax)){
    init <- add.constraint(portfolio=init, type="group",
                           groups=groupList,
                           group_min=groupMin,
                           group_max=groupMax)
  }
  init <- add.objective(portfolio=init, type="return", name="mean")
  init <- add.objective(portfolio=init, type="risk", name="StdDev")
  create.EfficientFrontier(R=R, portfolio=init, type="mean-StdDev", 
                           n.portfolios=nPortfolios, match.col="StdDev")
}

#' Efficient Frontier Plot
#' 
#' Plot the efficient frontier in return - standard deviation space
#' 
#' @param x object of class \code{efficient.frontier} created via \code{\link{efficientFrontier}}
#' @param y not used
#' @param \dots passthrough parameters
#' @param rf risk free rate
#' @param chartAssets TRUE/FALSE to include the assets in the plot
#' @param labelAssets TRUE/FALSE to include the labels of the assets in the plot
#' @param main a main title for the plot
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the x-axis limit, same as in \code{\link{plot}}
#' @param type what type of plot should be drawn, same as in \code{\link{plot}}
#' @param pchAssets plotting character of the assets
#' @param cexAssets numerical value giving the amount by which the asset points
#' and labels should be magnified relative to the default.
#' @seealso \code{\link{efficientFrontier}}
#' @method plot efficient.frontier
#' @S3method plot efficient.frontier
plot.efficient.frontier <- function(x, y, ..., rf=0, chartAssets=TRUE, 
                                    labelAssets=TRUE,
                                    main="Efficient Frontier", 
                                    xlim=NULL, ylim=NULL, type="l",
                                    pchAssets=19, cexAssets=0.8){
  chart.EfficientFrontier(object=x, ...=..., match.col="StdDev", 
                          xlim=xlim, ylim=ylim, type=type, cex.axis=0.8, 
                          main=main, rf=rf, tangent.line=TRUE, 
                          chart.assets=chartAssets, 
                          labels.assets=labelAssets,
                          pch.assets=pchAssets, 
                          cex.assets=cexAssets)
}
