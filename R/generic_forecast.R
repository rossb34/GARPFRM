
#' Model Forecasting
#' 
#' Generic method for forecasting from EWMA and GARCH models
#' @param model a model object
#' @param nAhead number of periods ahead to forecast
#' @param \dots any other passthrough parameters
#' @return forecasted value from model
#' @author Ross Bennett
#' @export
forecast <- function(model, nAhead, ...){
  UseMethod("forecast")
}

#' Forecast Univariate GARCH Models
#' 
#' Forecasting for GARCH models fit via \code{\link{uvGARCH}}
#' 
#' @note For rolling forecasts specified with the \code{nRoll} argument, the
#' GARCH model must be fit with \code{outSample} argument greater than or 
#' equal to \code{nRoll}.
#' 
#' @param model GARCH model fit via \code{\link{uvGARCH}}
#' @param nAhead number of steps ahead to forecast
#' @param \dots additional parameters passed to \code{ugarchforecast}
#' @param nRoll number of rolling forecasts
#' @param externalForecasts named list of external regressors in the mean and/or
#' variance equations
#' @return a uGARCHforecast object with the GARCH forecast data
#' @method forecast uvGARCH
#' @S3method forecast uvGARCH
forecast.uvGARCH <- function(model, nAhead=10, ..., nRoll=0, externalForecasts=NULL){
  
  if(is.null(externalForecasts)){
    externalForecasts <- list(mregfor = NULL, vregfor = NULL)
  }
  out <- ugarchforecast(model$fit, n.ahead=nAhead, n.roll=nRoll, 
                        external.forecasts=externalForecasts, ...=...)
  return(out)
}

#' Forecast Univariate EWMA Volatility Model
#' 
#' Forecasting for EWMA Volatility models fit via \code{\link{EWMA}}
#' 
#' @param model EWMA model fit via \code{\link{EWMA}}
#' @param nAhead number of steps ahead to forecast. (nAhead = 1 only supported)
#' @param \dots additional passthrough parameters
#' @return one period ahead EWMA volatility forecast
#' @method forecast uvEWMAvol
#' @S3method forecast uvEWMAvol
forecast.uvEWMAvol <- function(model, nAhead=1, ...){
  # 1 step ahead EWMA forecast
  lambda <- model$model$lambda
  sigma_hat <- last(model$estimate)
  r <- last(model$data$R)
  T0 <- last(index(r))
  tmp <- as.numeric(sqrt(lambda * sigma_hat^2 + (1 - lambda) * r^2))
  
  df <- data.frame(tmp, row.names="T+1")
  colnames(df) <- model$model$type
  df
}

# forecast for
# uvEWMAcov
# uvEWMAcor
# mvEWMAcov
# mvEWMAcor
