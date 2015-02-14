# TODO: We should implement this in C or C++ later assuming we get funding 
# and we use monte carlo for option pricing

# Monte Carlo Function
# generateMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
#   dt <- Time / steps
#   S <- vector("numeric", steps+1)
#   eps <- rnorm(steps)
#   S[1] <- starting_value
#   for(i in 2:length(S)){
#     dS <- mu * dt + sigma * eps(i-1) * sqrt(dt)
#     S[i] <- dS + S[i-1]
#   }
#   return(S)
# }

# Monte Carlo using ln(S) rather than S
# more accurate
generateLogMC <- function(mu, sigma, Time=1, steps=52, starting_value=100){
  dt <- Time / steps
  musigdt <- (mu - 0.5 * sigma^2) * dt
  sigdt <- sigma * sqrt(dt)
  S <- vector("numeric", steps+1)
  eps <- rnorm(steps)
  S[1] <- starting_value
  for(i in 2:length(S)){
    S[i] <- S[i-1] * exp(musigdt + sigdt * eps[i-1])
  }
  return(S)
}

#' Monte Carlo Price Path Simulation
#' 
#' Run \code{N} monte carlo simulations to generate asset price paths following
#' a geometric brownian motion process with constrant drift rate and constant 
#' volatility.
#' 
#' The Geometric Brownian Motion process to describe small movements in prices
#' is given by
#' \deqn{
#'   d S_t = \mu S_t dt + \sigma dz_t
#' }
#' 
#' ln S is simulated rather than simulating S directly such that
#' \deqn{
#'   S_t = S_{t-1} exp((\mu - 0.5 \sigma^2) dt + \sigma \sqrt{dt} \epsilon)
#' }
#' 
#' where:
#' \itemize{
#'   \item S_t is the asset price at time t
#'   \item S_{t-1} is the asset price at time t-1
#'   \item mu is the constant drift rate
#'   \item sigma is the constant volatility rate
#'   \item epsilon is a standard normal random variable
#' }
#' 
#' @note This function returns an m x N matrix of simulated price paths where
#' m is the number of steps + 1 and N is the number of simulations. This can be 
#' very memory and computatitonally intensive with a large number of steps 
#' and/or a large number of  simulations. More efficient methods in terms of 
#' speed and memory should be used, for example, to price options.
#' 
#' @param mu annualized expected return
#' @param sigma annualized standard deviation
#' @param N number of simulations
#' @param time length of simulation (in years)
#' @param steps number of time steps
#' @param starting_value asset price starting value
#' @return matrix of simulated price paths where each column represents a price path
#' @examples
#' library(GARPFRM)
#' 
#' mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)
#' @export
monteCarlo <- function(mu, sigma, N=100, time=1, steps=52, starting_value=100){
  mc_mat <- matrix(0, steps+1, N)
  for(i in 1:N){
    mc_mat[,i] <- generateLogMC(mu, sigma, time, steps, starting_value)
  }
  class(mc_mat) <- "MonteCarlo"
  return(mc_mat)
}

#' @method plot MonteCarlo
#' @S3method plot MonteCarlo
plot.MonteCarlo <-function(x, y, ..., main="Monte Carlo Simulation", xlab="Time Index", ylab="Price"){
  plot(x[,1], type="n", ylim=range(x), main=main, xlab=xlab, ylab=ylab, ...)
  for(i in 1:ncol(x)){
    lines(x[,i])
  }
}

#' Ending Prices of Monte Carlo Simulation
#' 
#' Get the ending prices, i.e. terminal values, of a monte carlo simulation
#' @param mc monte carlo object created with \code{monteCarlo}
#' @return vector ending prices
#' @examples
#' library(GARPFRM)
#' 
#' mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)
#' ep <- endingPrices(mc)
#' @export
endingPrices <- function(mc){
  if(!inherits(mc, "MonteCarlo")) stop("mc must be of class 'MonteCarlo'")
  return(mc[nrow(mc),])
}

#' Plot Ending Prices 
#' 
#' Plot the kernel density estimate and histogram of the ending prices
#' from a Monte Carlo simulation.
#' @param mc monte carlo object created with \code{monteCarlo}.
#' @param \dots additional arguments passed to \code{hist}.
#' @param main a main title for the plot.
#' @param xlab x-axis label, same as in \code{\link{plot}}.
#' @param ylab y-axis label, same as in \code{\link{plot}}.
#' @examples
#' library(GARPFRM)
#' 
#' mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)
#' plotEndingPrices(mc)
#' @export
plotEndingPrices <- function(mc, ..., main="Ending Prices", xlab="Price", ylab="Density"){
  if(!inherits(mc, "MonteCarlo")) stop("mc must be of class 'MonteCarlo'")
  ending_prices <- endingPrices(mc)
  dens_ep <- density(ending_prices)
  hist(ending_prices, freq=FALSE, ylim=range(dens_ep$y), ...=..., main=main, xlab=xlab, ylab=ylab)
  lines(dens_ep)
  invisible(list(ending_prices=ending_prices, density=dens_ep))
}
