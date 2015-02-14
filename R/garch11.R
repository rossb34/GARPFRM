
# Because our function is a wrapper around functions in the rugarch and
# rmgarch packages, we need to give proper credit (i.e. citation("rugarch")
# and citation("rmgarch"))

# rugarch: univariate garch models
# rmgarch: multivariate garch models

# we need to support GARCH models for both univariate and multivariate data

## The GARP text does not have any discussion on multivariate GARCH models.
## I think we should omit this for phase 1 and maybe reconsider in phase 2
## or beyond.

# GARCH Models
# 
# This function is a basic wrapper of functions in the rugarch and rmgarch
# packages to specify and fit GARCH models. The rugarch and rmgarch packages
# provide functions to specify and fit a rich set of GARCH models. 
# The purpose of this function is to specify and fit a GARCH model while 
# abstracting away some complexities.
# 
# The rugarch package implements univariate garch models and the
# rmgarch package implements multivariate garch models. Univariate or 
# multivariate data is automatically detected and the appropriate GARCH model
# will be specified and fit.
# 
# For complete functionality of GARCH models, it is recommended to 
# directly use functions in the rugarch and rmgarch packages.
# 
# @param R xts object of asset returns
# @param model “sGARCH”, “fGARCH”, “eGARCH”, “gjrGARCH”, “apARCH” and “iGARCH” and “csGARCH”
# @param distribution.model. Valid choices are “norm” for the normal distibution, “snorm” for the skew-normal distribution, “std” for the student-t, “sstd” for the skew-student, “ged” for the generalized error distribution, “sged” for the skew-generalized error distribution, “nig” for the normal inverse gaussian distribution, “ghyp” for the Generalized Hyperbolic, and “jsu” for Johnson's SU distribution. 
# @export
# By default we use UV N~GARCH(1,1) and Bollerslev for each series
# garch11 <- function(R, model = "sGARCH", distribution.model = "norm"){
#   # if univariate data, load the rugarch package
#   # if multivariate data, load the rmgarch package
#   
#   garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
#                             variance.model = list(garchOrder = c(1,1), model = model), 
#                             distribution.model)
#   
#   # DCC specification: GARCH(1,1) for conditional cor
#   nbColumns = ncol(R)
#   dcc.garch11.spec = dccspec(uspec = multispec( replicate(nbColumns, garch11.spec) ), 
#                              dccOrder = c(1,1), distribution = "mvnorm")
#   dcc.garch11.spec
#   
#   dcc.fit = dccfit(dcc.garch11.spec, data = R)
#   class(dcc.fit)
#   slotNames(dcc.fit)
#   names(dcc.fit@mfit)
#   names(dcc.fit@model)
#   return(dcc.fit)
# }

# Forecast GARCH(1,1)
# 
# Description of forecast GARCH(1,1)
# 
# @param garch11 object created by \code{\link{garch11}}
# @param window is the forecast window (default is set to window = 100)
# @export
# fcstGarch11 <- function(object, window){
#   UseMethod("fcstGarch11")
# }

# @method fcstGarch11 Dccfit
# @S3method fcstGarch11 DCCfit
# fcstGarch11.DCCfit <- function(object, window = 100){
#   #if ((window > nrow(object))) {stop("Window is too large to forecast")}
#   result = dccforecast(object, n.ahead=window)
#   class(result)
#   slotNames(result)
#   class(result@mforecast)
#   names(result@mforecast)
#   return(result)
# }


#' Univariate GARCH Model
#' 
#' Specify and fit a univariate GARCH model
#' 
#' @details 
#' This function is a basic wrapper of functions in the rugarch package
#' to specify and fit GARCH models. The rugarch package
#' provides functions to specify and fit a rich set of GARCH models. 
#' The purpose of this function is to specify and fit a GARCH model while 
#' abstracting away some complexities.
#' 
#' @param R xts object of asset returns.
#' @param model GARCH Model to specify and fit. Valid GARCH models are
#' "sGARCH", "fGARCH", "eGARCH", "gjrGARCH", "apARCH", "iGARCH", and "csGARCH".
#' @param garchOrder the ARCH(q) and GARCH(p) orders.
#' @param armaOrder the autoregressive and moving average orders.
#' @param distribution conditional density to use for the innovations. Valid 
#' distributions are "norm" for the normal distibution, "snorm" for the 
#' skew-normal distribution, "std" for the student-t, 
#' "sstd for the skew-student, "ged" for the generalized error distribution, 
#' "sged" for the skew-generalized error distribution, 
#' "nig" for the normal inverse gaussian distribution, 
#' "ghyp" for the Generalized Hyperbolic, and "jsu" for Johnson's SU distribution.
#' @param fixedParams named list of parameters to keep fixed.
#' @param solver the solver to use to fit the GARCH model. Valid solvers are
#' "nlminb", "solnp", "lbfgs", "gosolnp", "nloptr", or "hybrid". 
#' @param outSample number of periods of data used to fit the model. 
#' \code{nrow(R) - outSample} number of periods to keep as out of sample data
#' points.
#' @param fitControl named list of arguments for the fitting routine
#' @param solverControl named list of arguments for the solver
#' @return a list of length two containing GARCH specification and GARCH fit objects
#' @author Ross Bennett
#' @seealso \code{\link[rugarch]{ugarchspec}}, \code{\link[rugarch]{ugarchfit}}
#' @export
uvGARCH <- function(R, model="sGARCH", 
                    garchOrder=c(1, 1), 
                    armaOrder=c(1,1), 
                    distribution="norm",
                    fixedParams=NULL,
                    solver="hybrid",
                    outSample=0,
                    fitControl=NULL,
                    solverControl=NULL){
  # Function to specify and fit a univariate GARCH model
  
  stopifnot("package:rugarch" %in% search() || require("rugarch", quietly = TRUE))
  
  if(is.null(fixedParams)){
    fixedParams <- list()
  }
  
  # Specify the GARCH model
  # uGARCHspec object
  spec <- ugarchspec(variance.model=list(model=model, garchOrder=garchOrder),
                     mean.model=list(armaOrder=armaOrder),
                     distribution.model=distribution,
                     fixed.pars=fixedParams)
  
  # Fit the GARCH model
  # uGARCHfit object
  
  if(is.null(fitControl)){
    fitControl <- list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all')
  }
  
  if(is.null(solverControl)){
    solverControl <- list()
  }
  
  fit <- ugarchfit(spec=spec, data=R, out.sample=outSample, solver=solver, 
                   fit.control=fitControl, solver.control=solverControl)
  
  # structure and return the univariate GARCH model specification and fit
  return(structure(list(spec=spec, fit=fit),
                   class="uvGARCH"))
}

#' Get GARCH Model Specification
#' 
#' Function to extract the GARCH model specification object 
#' 
#' @param garch a GARCH model specification and fit created with \code{uvGARCH}
#' @return an object of class uGARCHspec
#' @export
getSpec <- function(garch){
  UseMethod("getSpec")
}

#' @method getSpec uvGARCH
#' @S3method getSpec uvGARCH
getSpec.uvGARCH <- function(garch){
  garch$spec
}

#' Get Fitted GARCH Model
#' 
#' Function to extract the fitted GARCH model object 
#' 
#' @param garch a GARCH model specification and fit created with \code{uvGARCH}
#' @return an object of class uGARCHfit
#' @export
getFit <- function(garch){
  UseMethod("getFit")
}

#' @method getFit uvGARCH
#' @S3method getFit uvGARCH
getFit.uvGARCH <- function(garch){
  garch$fit
}

#' Plot GARCH Model
#' 
#' Plots for fitted GARCH Models
#' 
#' @param x uvGARCH object create via \code{uvGARCH}
#' @param y not used
#' @param \dots additional parameters passed to plot method for uGARCHfit objects
#' @param which plot selection
plot.uvGARCH <- function(x, y, ..., which){
  plot(getFit(x), which=which, ...=...)
}
