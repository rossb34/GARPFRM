########## Hedge Section-Convexity and Duration##########

# macaulay duration
bondDuration.MC <- function(bond, discountCurve, percentChangeYield = 0){
  # Get data from the bond and discount curve
  nDC = length(discountCurve)
  m = bond$m
  couponRate = bond$couponRate
  face = bond$face
  time = bond$time
  # Calculate the ytm
  ytm = bondYTM(bond=bond, discountCurve=discountCurve) + percentChangeYield
  # Convert to continuously compounded rate
  y_c = m * log(1 + ytm / m)
  # Get the cashflows of coupon amounts and face value
  couponAmount = face * couponRate / m
  cashflows = rep(couponAmount, nDC)
  cashflows[nDC] = couponAmount + face
  # Calculate the price based on the continuously compounded rate
  price = sum(cashflows * exp(-y_c * time))
  # Calculate the duration
  duration = sum(-time * cashflows * exp(-y_c * time)) / -price
  return(duration)
}

# modified duration
bondDuration.Mod <- function(bond, discountCurve, percentChangeYield = 0){
  #Get the macaulay duration using bondDuration.MC function
  duration = bondDuration.MC(bond, discountCurve, percentChangeYield)  
  #Calculating yield to maturity using bondYTM function
  ytm = bondYTM(bond,discountCurve)
  mduration = duration/(1+ytm/bond$m)
  return(mduration)
}

#' Calculate the duration of a bond
#' 
#' Estimate the macaulay or modified duration of a fixed rate coupon bond 
#' given the discount curve and bond data. The duration is calculated
#' using the continuously compounded yield
#' 
#' @param bond a \code{bond} object created with \code{\link{bondSpec}}
#' @param discountCurve vector of discount rates
#' @param percentChangeYield optional elasticity measure 
#' @param type specify modified or macaulay duration
#' @return duration of the bond
#' @examples
#' time = seq(from=0.5, to=2, by=0.5)
#' DF = rbind(0.968,0.9407242,0.9031545,0.8739803)
#' bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
#' mcDuration = bondDuration(bond,DF, type="macaulay")
#' modDuration = bondDuration(bond,DF, type="modified")
#' @author Thomas Fillebeen and Jaiganesh Prabhakaran
#' @export
bondDuration <- function(bond, discountCurve, percentChangeYield = 0, type=c("modified", "macaulay")){
  type <- match.arg(type)
  switch(type,
         modified = {
           out <- bondDuration.Mod(bond, discountCurve, percentChangeYield)
         },
         macaulay = {
           out <- bondDuration.MC(bond, discountCurve, percentChangeYield)
         }
  )
  return(out)
}

#' Calculate the convexity of a fixed rate coupon bond
#' 
#' This function estimates the convexity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param bond a \code{bond} object in discountFactorArbitrage
#' @param discountCurve vector of discount rates
#' @return convexity of the bond
#' @examples
#' time = seq(from=0.5, to=2, by=0.5)
#' DF = rbind(0.968,0.9407242,0.9031545,0.8739803)
#' bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
#' convexity = bondConvexity(bond,DF)
#' @author Thomas Fillebeen
#' @export
bondConvexity <- function(bond, discountCurve){
  # Get data from the bond and discount curve
  nDC = length(discountCurve)
  m = bond$m
  couponRate = bond$couponRate
  face = bond$face
  time = bond$time
  # Get the cashflows of coupon amounts and face value
  couponAmount = face * couponRate / m
  cashflows = rep(couponAmount, nDC)
  cashflows[nDC] = couponAmount + face
  # The price is the sum of the discounted cashflows
  price = sum(discountCurve * cashflows)
  weights = (discountCurve * cashflows) / price
  convexity = sum(weights * time^2)
  return(convexity)
}

#' Calculate the yield to maturity of a bond
#' 
#' This function calculates the yield to maturity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param bond a \code{bond} object
#' @param discountCurve vector of discount rates
#' @return yield to maturity of the bond
#' @examples
#' time = seq(from=0.5, to=2, by=0.5)
#' DF = rbind(0.968,0.9407242,0.9031545,0.8739803)
#' bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
#' bondYTM(bond,DF)
#' @author Thomas Fillebeen
#' @export
bondYTM <- function(bond, discountCurve){
  # First step is to calculate the price based on the discount curve
  price <- bondPrice(bond=bond, discountCurve=discountCurve)
  
  # Get the data from the bond object
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  # Use optimize to solve for the yield to maturity
  tmp <- optimize(ytmSolve, interval=c(-1,1), couponRate=couponRate, m=m, nPayments=length(time), face=face, targetPrice=price, tol=.Machine$double.eps)
  ytm <- tmp$minimum
  return(ytm)
}

#' Solve for the yield to maturity of a bond
#' 
#' This function solves for the yield to maturity of a fixed rate coupon bond 
#' given the discount curve and bond data.
#' 
#' @param ytm yield to maturity
#' @param couponRate coupon rate
#' @param m compounding frequency
#' @param nPayments is the number of payments
#' @param face is the face value
#' @param targetPrice is the price of the bond
#' @return Absolute value of difference between the price and the present value
#' @author Thomas Fillebeen
#' @export
ytmSolve <- function(ytm, couponRate, m, nPayments, face, targetPrice){
  C <- face * couponRate / m
  tmpPrice <- 0
  for(i in 1:nPayments){
    tmpPrice <- tmpPrice + C / ((1 + (ytm / m))^i)
  }
  tmpPrice <- tmpPrice + face / (1 + ytm / m)^nPayments
  return(abs(tmpPrice - targetPrice))
}


############ Hedge section-Empirical###############

#' Estimate the delta hedge of for a bond
#' 
#' This function estimates the delta for hedging a particular bond 
#' given bond data
#' 
#' @param regressand a \code{bond} object in discountFactorArbitrage
#' @param regressor the right hand side
#' @return delta of the hedge
#' @examples
#' # Load Data for historcal analysis tools
#' data(crsp.short)
#' data = largecap.ts[,2:6]
#' head(data)
#' # Empirical application: Linear hedge estimation 
#' # OLS Level-on-Level regression 
#' deltas = linearHedge(data[,1],data[,2:5])
#' # Insert the normalized hedged contract versus hedgeable contract value
#' deltas = c(1,deltas)
#' # In sample illustration: random, mean reverting spreads
# ' hedgedInstruments = data%*%deltas
#' @author Thomas Fillebeen
#' @export
linearHedge <- function(regressand, regressor){
    deltas = matrix(0,nrow=1,ncol= ncol(regressor))
    reg = lm(regressand ~ regressor)
    deltas = -coef(reg)[seq(2,ncol(data),1)]
  return(deltas)
}

#' Estimate PCA loadings and create a PCA object
#' 
#' This function estimates the delta for hedging a particular bond 
#' given bond data
#' 
#' @param data time series data
#' @param nfactors number of components to extract
#' @param rotate "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" are possible rotations/transformations of the solution.
#' @return pca object loadings
#' @author Thomas Fillebeen
#' @export
PCA <- function(data, nfactors, rotate = "none"){
  stopifnot("package:psych" %in% search() || require("psych", quietly = TRUE))
  
  pca = principal(data, nfactors, rotate="none")
  class(pca) <- c("PCA","psych", "principal")
  return(pca)
}

#' Retrieve PCA loadings
#' 
#' @param object is a pca object created by \code{\link{PCA}}
#' @author Thomas Fillebeen
#' @export 
getLoadings <- function(object){
loadings = object$loadings
  return(loadings)
}

#' Retrieve PCA weights
#' 
#' @param object is a pca object created by \code{\link{PCA}}
#' @author Thomas Fillebeen
#' @export 
getWeights <- function(object){
  weights = object$weight
  return(weights)
}

#' Plotting method for PCA
#' 
#' Plot a fitted PCA object
#' 
#' @param x a PCA object created by \code{\link{PCA}}
#' @param y not used
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param main a main title for the plot
#' @param separate if TRUE plot of same, and if FALSE plot separately
#' @author Thomas Fillebeen
#' @method plot PCA
#' @S3method plot PCA
plot.PCA <- function(x, y, ..., main="Beta from PCA regression",separate=TRUE){
 if(ncol(x$loading)> 3) warning("Only first 3 loadings will be graphically displayed")
  # Plot the first three factors
 if (ncol(x$loading) >= 3){
   if(!separate){
   plot(x$loading[,1], type="l", main = main, 
        xlab="Maturity/Items", ylab="Loadings", ...=...)
   lines(x$loading[,2], col="blue",lty=2)
   lines(x$loading[,3], col="red",lty=2)
   legend("topleft",legend=c("PCA1","PCA2","PCA3"),bty="n",lty=c(1,2,2),col=c("black","blue","red"), cex=0.8)
   }else{
     plot.zoo(x$loading[,1:3], type="l", main = main, 
          xlab="Maturity/Items", ...=...)
   }
 }else if(ncol(x$loading) == 2){
   if(!separate){
   plot(x$loading[,1], type="l", main = main, 
        xlab="Maturity/Items", ylab="Loadings", ...=...)
   lines(x$loading[,2], col="blue",lty=2)
   legend("topleft",legend=c("PCA1","PCA2"),bty="n",lty=c(1,2),col=c("black","blue"), cex=0.8)
   }else{
     plot.zoo(x$loading[,1:2], type="l", main = main, 
              xlab="Maturity/Items", ...=...)
   }
 }else{
   plot(x$loading[,1], type="l", main = main, 
        xlab="Maturity/Items", ylab="Loadings", ...=...)
   legend("topleft",legend=c("PCA1"),bty="n",lty=c(1),col=c("black"), cex=0.8)
 }
}
