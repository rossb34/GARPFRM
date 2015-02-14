# Ch 6 Prices, Discount Factors, and Arbitrage (Law of one Price)
# The Cash Flows from Fixed-Rate Government Coupon Bonds, Discount Faors, Law of One Price
# Arbitrage opportunity: trade that generates profits without any chance of losing money.
# If there is a deviation from the law of one price, there exists an arbitrage opportunity.
# In order to estimate the discount factor for a particular term gives the value today, 
# or the present alue of one unit of currency to be received at the end of that term.(Pg.129)

#' Constructor for bond specification
#' 
#' Create a bond specification.
#' 
#' @param time vector of sequence of coupon payments in years
#' @param face face value of bond
#' @param m compounding frequency
#' @param couponRate rate the coupon pays
#' @return a \code{bond} object with the bond data used for pricing
#' @examples
#' time = seq(from=0.5, to=2, by=0.5)
#' bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
#' @author Thomas Fillebeen
#' @export
bondSpec = function(time=seq(from=0.5,to=2,by=0.5), face=100, m=2, couponRate=0.01){
  if(!all(diff(time) == (1/m))) stop("misspecification of sequence of time and compounding frequency")
  bond = list()
  bond$m = m
  bond$couponRate = couponRate
  bond$face = face
  bond$time = time
  class(bond) = c("bond.spec", "bond")
  return(bond)
}

#' To determine if user is specifying bond parameters correctly
#' 
#' @param object a bond specification object created by \code{\link{bondSpec}}
#' @author Thomas Fillebeen
#' @export
is.bond = function(object){
  inherits(object, "bond.spec")
}

#' Estimate price of bond
#' 
#' This function calculates the price of a fixed rate coupon bond given the 
#' discount curve and bond data. First it converts the discountCurve into CF
#' @param bond a \code{bondSpec} object
#' @param discountCurve vector of discount rates
#' @return price of the bond
#' @examples
#' time = seq(from=0.5, to=2, by=0.5)
#' bond = bondSpec(time, face=100, m=2, couponRate = 0.0475)
#' DF = rbind(0.968,0.9407242,0.9031545,0.8739803)
#' price = bondPrice(bond,DF)
#' @author Thomas Fillebeen
#' @export
bondPrice = function(bond, discountCurve){
  if(!is.bond(bond)) stop("bond must be an object of class 'bond'")
  # Number of periods in discount curve
  nDC <- length(discountCurve)
  m <- bond$m
  couponRate <- bond$couponRate
  face <- bond$face
  time <- bond$time
  
  couponAmount <- face * couponRate / m
  cashflows <- rep(couponAmount, nDC)
  cashflows[nDC] <- couponAmount + face

  price <- sum(cashflows * discountCurve)
  return(price)
}

#' Estimate discountFactor
#' 
#' This function calculates the discountFactor (DF) given price 
#' and cashFlows.
#' @param price of a bond
#' @param cashFlow of a bond
#' @return discount factors
#' @examples
#' cashFlow = rbind(c(100+(1+1/4)/2,0,0),c((4 +7/8)/2,100+(4+7/8)/2,0),c((4+1/2)/2,(4+1/2)/2,100+(4+1/2)/2))
#' # Created Price of the bond
#' price = matrix(c(100.550, 104.513, 105.856), ncol=1)
#' DF = discountFactor(price, cashFlow)
#' @author Thomas Fillebeen
#' @export
discountFactor = function(price, cashFlow){
  DF = solve(cashFlow) %*% price
  return(DF)
}

#' bondFullPrice
#' 
#' Estimate price of bond w/ acrrued interest
#' The present value of a bond's cash flows should be equated or 
#' compared with its full price, with the amount a purchaser actually 
#' pays to purchase those cash flows. The flat price is denoted by p, accrued 
#' interest is AI, the present value of the cash flows by PV, and the 
#' full price by P: 
#' P=p+AI=PV
#' 
#' This function calculates the price of a fixed rate coupon bond given coupon rate, yield, 
#' compoundPd, cashFlowPd, face value, previous coupon date, next coupon date.
#' @param bond is a bondSpec object created by \code{\link{bondSpec}}
#' @param yield is the yield on the bond
#' @param cashFlowPd cash flow period
#' @param t0 previous coupon date
#' @param t1 next coupon period
#' @param currentDate current date
#' @examples
#' t0 = as.Date("2013-08-15")
#' t1 = as.Date("2014-02-15")
#' tn = as.Date("2013-10-04")
#' currentDate = tn
#' bond = bondSpec(face=100, m=2, couponRate = 0.0475)
#' y1 = 0.00961
#' bondFullPrice(bond, y1, 8, t0, t1, tn)$clean
#' bondFullPrice(bond, y1, 8, t0, t1, tn)$dirty
#' bondFullPrice(bond, y1, 8, t0, t1, tn)$accruedInterest
#' @return price of the bond: clean, dirty and accrued interest
#' @author Thomas Fillebeen
#' @export
bondFullPrice = function(bond, yield, cashFlowPd, t0, t1, currentDate){
  compoundPd = bond$m
  face = bond$face
  couponRate = bond$couponRate
  # Apply a general dayCount (weekend included)
  d1 = as.numeric(t1-currentDate)
  d2 = as.numeric(t1-t0)
  # Initialize
  tmp = 0 
  
  #Will go through the loop only if the number of cashFlow periods are at least 2
  if (cashFlowPd > 1){
    for(k in 1:(cashFlowPd-1)){
      tmp = tmp + ((couponRate / compoundPd * face) / ((1 + yield/compoundPd)^k))
    }
  }
  # Calculate dirty price based on partial periods formula
  dirtyP = (1 / ((1 + yield / compoundPd)^(d1/d2))) * (couponRate / compoundPd * face + tmp + face / ((1 + yield/compoundPd)^(cashFlowPd-1)))
  # Calculate accruedInterest
  aiDays = as.numeric(currentDate-t0)
  couponDays = as.numeric(t1-t0)
  ai = couponRate / compoundPd * face * aiDays / couponDays
  cleanP = dirtyP - ai
  return(list(dirty=dirtyP, clean=cleanP, accruedInterest=ai))
}

#' Estimate continuously conpounding rate to be used in term structure
#' 
#' This function calculates the continuously compounding rate given an initial dataset 
#' with specific format, date of reference coumpounding frequency, and face value
#' @param dat is a dataset with cusip, issueDate, MaturityDate, Name, Coupon, Bid/Ask
#' @param initialDate is the date when the estimation should be conducted: date of reference
#' @param m compounding frequency
#' @param face face value
#' @return continuously compounding rates
#' @author Thomas Fillebeen
#' @export
compoundingRate = function(dat, initialDate=as.Date("1995-05-15"), m, face=100){
  # Convert the dates to a date class
  dat[, "IssueDate"] = as.Date(dat[, "IssueDate"], format="%m/%d/%Y")
  dat[, "MaturityDate"] = as.Date(dat[, "MaturityDate"], format="%m/%d/%Y")
  # Convert the coupon column to a numeric
  # Vector of prices
  price = (dat[, "Bid"] + dat[, "Ask"]) / 2
  
  # Generate cash flow dates for each bond
  bondData = list()
  for(i in 1:nrow(dat)){
    maturityDate = dat[i, "MaturityDate"]
    # Intialize a new list for every price, coupon, coupon date.
    bondData[[i]] = list()
    # Store price and the number of the coupon
    bondData[[i]]$price = price[i]
    bondData[[i]]$coupon = dat[i, "Coupon"]
    # Remove initialDate
    tmpSeq <- seq(from=initialDate, to=maturityDate, by="3 months")
    bondData[[i]]$couponDates = tmpSeq[-1]
    tmpDates = bondData[[i]]$couponDates
    tmpCoupons = vector("numeric", length(tmpDates))
    for(j in 1:length(tmpDates)){
      tmpCoupons[j] = bondData[[i]]$coupon / m * face
      if(j == length(tmpDates)){
        tmpCoupons[j] = face + bondData[[i]]$coupon / m * face
      }
    }
    bondData[[i]]$cashFlow = tmpCoupons
  }
  # Create a matrix of cash flows
  CF = matrix(0, length(price), length(price))
  # Populate the CF matrix
  for(i in 1:nrow(CF)){
    tmp = bondData[[i]]$cashFlow
    index = 1:length(tmp)
    CF[i, index] = tmp
  }
  # Utilize the discountFactor function
  DF = discountFactor(price,CF)

  dates = bondData[[nrow(dat)]]$couponDates
  years = vector("numeric", length(dates))
  for(i in 1:length(years)){
    years[i] = (as.numeric(strftime(dates[i], "%Y")) + as.numeric(strftime(dates[i], "%m"))/12) - (as.numeric(strftime(initialDate, "%Y")) + as.numeric(strftime(initialDate, "%m"))/12)
  }
  # Calculate continuously compounded rates from discount factors
  ccRate = vector("numeric", length(years))
  for(i in 1:length(ccRate)){
    ccRate[i] = - log(DF[i]) / years[i]
  }
  rate = list()
  rate$years = years
  rate$ccRate = ccRate 
  return(rate)
}

#' Estimate spot and forward rates
#' 
#' This function calculates the forward or forward rates given an discount factors 
#' and time increments
#' @param time increments of time when discount factors are estimated
#' @param DF discount factor for during time increments
#' @examples 
#' DF = c(0.996489, 0.991306, 0.984484, 0.975616, 0.964519)
#' time = seq(from=0.5, to=2.5, by=0.5)
#' rates = spotForwardRates(time,DF)
#' rates
#' @author Thomas Fillebeen
#' @export
spotForwardRates = function(time, DF){
  if(length(time) != length(DF)) stop("both time and DF parameter need to be of the same length")
  spotRates = matrix(0,length(time),1)
  for(i in 1:(length(time))){
    spotRates[i] = (2-2*DF[i]^(1/(2*time[i]))) / DF[i]^(1/(2*time[i]))
  }
  
  forwardRates = matrix(0,length(time),1)
  forwardRates[1] = spotRates[1]
  for(j in 1:(length(time)-1)){
    forwardRates[j+1] = (DF[j]/DF[j+1] - 1) *2
  }
  rates = cbind(spotRates, forwardRates)
  colnames(rates)= cbind("Spot","Forward")
  return(rates)
}

### Modelling a Zero-Coupon Bond (ZCB)
#' There are three main types of yield curve shapes: normal, inverted and flat (or humped)
#' 
#' Estimate Vasicek zero-coupon bond to be used in term structure
#' 
#' This function calculates the Vasicek Price given an initial data calibration 
#' The function is a subfunction for yieldCurveVasicek
#' @param r initial short rate
#' @param k speed of reversion parameter
#' @param theta long-term reversion yield
#' @param sigma randomness parameter. Modelled after Brownan Motion
#' @param maturity maturity of the bond
#' @return zero coupon bond price estimated from Vasicek model
#' @author Thomas Fillebeen
#' @export
vasicekPrice = function(r, k, theta, sigma, maturity){
    mean = (1/k)*(1 - exp(-maturity*k)) 
    variance = (theta - sigma^2/(2*k^2))*(maturity - mean) + (sigma^2)/(4*k)*mean^2
    price = exp(-variance - mean*r)
    return(price)
  }

#' Estimate Vasicek zero-coupon yield
#' 
#' This function calculates the Vasicek yield given an initial data calibration
#' 
#' @param r initial short rate
#' @param k speed of reversion parameter
#' @param theta long-term reversion yield
#' @param sigma randomness parameter. Modelled after Brownian Motion
#' @param maturity maturity of the bond
#' @return yield curve estimate from Vasicek model
#' @author Thomas Fillebeen
#' @export
yieldCurveVasicek = function(r, k, theta, sigma, maturity){
  n = length(r)
  yield = matrix(0, maturity, n)
  for(i in 1:n){
    for(t in 1:maturity){
      yield[t,i] = -log(vasicekPrice(r[i], k, theta, sigma, t))/t
    }
  }
  return(yield)
}