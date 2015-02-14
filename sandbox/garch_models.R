
# examples from rugarch package

# http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/

# The rugarch package aims to provide for a comprehensive set of methods for 
# modelling uni-variate GARCH processes, including fitting, filtering, 
# forecasting, simulation as well as diagnostic tools including plots and 
# various tests.

library(rugarch)
library(rmgarch)

#' GARCH forecast
#' 
#' Forecasting from a GARCH model
#' 
#' @param object 
#' @param nAhead number of periods ahead to forecast
#' @param nRoll number of rolling forecasts. The rolling forecast specified by 
#' \code{n.roll} depends on the out of sample data available in the fitted 
#' GARCH model.
#' @return a \code{uGARCHforecast} object
forecast <- function(object, nAhead=10, nRoll=0){
  out <- ugarchforecast(fitORspec=object$fit, n.ahead=nAhead, n.roll=nRoll)
  return(out)
}

data(sp500ret)

model <- foo(R=sp500ret)

# Model specification
args(ugarchspec)
?ugarchspec

# Model fitting
args(ugarchfit)
?ugarchfit

# Forecasting
# there are 2 methods
# A rolling method, whereby consecutive 1-ahead forecasts are created based 
# on the out.sample option set in the fitting routine

# an unconditional method for n > 1 ahead forecasts

args(ugarchboot)
?ugarchboot

# simulation
args(ugarchsim)
?ugarchsim

# rolling estimation
args(ugarchroll)
?ugarchroll

# multivariate GARCH specification models
# Copula-GARCH
# DCC-GARCH
# GO-GARCH

# Copula-GARCH specification and fit
?cgarchspec
?cgarchfit

# DCC-GARCH specification and fit
?dccspec
?dccfit

#GO-GARCH specification and fit
?gogarchspec
?gogarchfit

# Also filter, forecast, roll, and sim methods
