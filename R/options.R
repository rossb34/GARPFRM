
##### Option Specification #####
#' Option Specification
#' 
#' Specify parameters of an option
#' 
#' @param style style of the option, e.g. european, american, etc.
#' @param type type of the option; "call" or "put"
#' @param S0 underlying asset price
#' @param K strike price
#' @param maturity the life of the option, measured in years
#' @param r risk free rate
#' @param volatility volatility of the underlying asset price
#' @param q continuous dividend yield rate for options on stocks or stock 
#' indices paying a dividend. Also the foreign risk free rate for options on 
#' currencies
#' @return an object of class "option" with the parameters that specify the option
#' @author Ross Bennett
#' @examples
#' am.call <- optionSpec(style="american", type="call")
#' euro.call <- optionSpec(style="european", type="call", S0=30, K=30, maturity=1, r=0.05, volatility=0.25, q=0)
#' @export
optionSpec <- function(style=c("european", "american"), 
                       type=c("call", "put"), 
                       S0=100, 
                       K=100,
                       maturity=1,
                       r=0.05, 
                       volatility=0.2, 
                       q=0){
  style <- match.arg(style)
  type <- match.arg(type)
  
  # Put into a list and return
  out <- list()
  out$style <- tolower(style)
  out$type <- tolower(type)
  out$S0 <- S0
  out$K <- K
  out$maturity <- maturity
  out$r <- r
  out$volatility <- volatility
  out$q <- q
  class(out) <- "option"
  return(out)
}

is.option <- function(x){
  inherits(x, "option")
}

##### Value #####

#' Option Value
#' 
#' Estimate the value of an option
#' 
#' @param option an \code{option} object created with \code{\link{optionSpec}}
#' @param method the method used to value the option
#' @param N number of steps in binomial tree
#' @param verbose TRUE/FALSE default FALSE. TRUE prints the node information 
#' of the binomial tree
#' @param \dots any other passthrough parameters
#' @return the estimated value of the option
#' @author Ross Bennett
#' @examples
#' am.call <- optionSpec(style="american", type="call")
#' am.call.val <- optionValue(am.call, N=4)
#' euro.call <- optionSpec(style="european", type="call", S0=30, K=30, maturity=1, r=0.05, volatility=0.25, q=0)
#' euro.call.val.bs <- optionValue(euro.call, method="Black-Scholes")
#' @export
optionValue <- function(option, method=c("Binomial", "Black-Scholes"), N=20, verbose=FALSE, ...){
  if(!is.option(option)) stop("option must be of class 'option'")
  
  style <- option$style
  method <- tolower(method[1])
  
  if(style == "american"){
    if(method == "binomial" || method == "lattice"){
      out <- americanBinomial(option, N, verbose)
    } else {
      print(paste(method, " is not supported for an american option"))
      out <- NULL
    }
  } # american
  
  if(style == "european"){
    bs_methods <- c("black-scholes", "black-scholes-merton", "bs", "bsm")
    if(method == "binomial" || method == "lattice"){
      out <- europeanBinomial(option, N, verbose)
    } else if(method %in% bs_methods){
      out <- europeanBS(option)
    } else {
      print(paste(method, " is not supported for an american option"))
      out <- NULL
    }
  } # european
  return(out)
}

##### Binomial Tree #####

# Binomial tree to price a european option
europeanBinomial2 <- function(option, N){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "european") stop("must be a european option")
  
  # N: number of time steps
  # type: call or put
  # S0: initial asset value
  # K: strike price
  # r: continuously compounded yearly risk-free rate
  # vol: annualized standard deviation of log return
  # q: continuous dividend yield
  # ttm: time to maturity (in years), i.e. the life of the option
  
  # Extract the parameters of the option
  type <- option$type
  S0 <- option$S0
  K <- option$K
  r <- option$r
  vol <- option$volatility
  q <- option$q
  ttm <- option$maturity
  
  # 1 for call, -1 for put
  if(type == "call"){
    mult <- 1
  } else if(type == "put") {
    mult <- -1
  } else {
    mult <- 0
  }
  
  # Time step (delta t)
  dt <- ttm / N
  
  # Size of up move
  u <- exp(vol * sqrt(dt))
  
  # Size of down move
  d <- exp(-vol * sqrt(dt))
  
  # Risk neutral probability of an uptick
  p <- (exp((r - q) * dt) - d)/(u - d)
  
  # Vectorized version of binomial tree for european option
  A <- choose(N, 0:N) * p^(0:N) * ((1 - p)^(N - (0:N))) * pmax(mult * ((u^(0:N)) * (d^(N - (0:N))) * S0 - K), 0)
  A <- exp(-r * ttm) * sum(A)
  return(A)
}

# Binomial tree to price a european option
europeanBinomial <- function(option, N, verbose=FALSE){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "european") stop("must be a european option")
  
  # N: number of time steps
  # type: call or put
  # S0: initial asset value
  # K: strike price
  # r: continuously compounded yearly risk-free rate
  # vol: annualized standard deviation of log return
  # q: continuous dividend yield
  # ttm: time to maturity (in years), i.e. the life of the option
  
  # Extract the parameters of the option
  type <- option$type
  S0 <- option$S0
  K <- option$K
  r <- option$r
  vol <- option$volatility
  q <- option$q
  ttm <- option$maturity
  
  # 1 for call, -1 for put
  if(type == "call"){
    mult <- 1
  } else if(type == "put") {
    mult <- -1
  } else {
    mult <- 0
  }
  
  # List to store option values
  # These are used at the end to compute greeks
  # option_value <- vector("list", 4)
  
  # Time step (delta t)
  dt <- ttm / N
  
  # Size of up move
  u <- exp(vol * sqrt(dt))
  
  # Size of down move
  d <- exp(-vol * sqrt(dt))
  
  # Risk neutral probability of an uptick
  p <- (exp((r - q) * dt) - d)/(u - d)
  
  # Discount factor
  df <- exp(-r * dt)
  
  # At the terminal node, there are N+1 asset values
  V <- pmax(0, mult * (S0 * (u^(0:N)) * (d^(N - (0:N))) - K))
  # if(N == 4) option_value[[4]] <- V
  
  if(verbose){
    cat("Time step: ", N, "\n", sep="")
    cat("Prices:\n")
    print(S0 * (u^(0:N)) * (d^(N - (0:N))))
    cat("Option Values:\n")
    print(V)
  }
  
  # Iterate backward, such that there are j+1 asset values, where j is the
  # Number of time steps
  j.index <-seq(from=N-1, to=0, by=-1)
  for (j in j.index) {
    # S is the vector of prices at each time step and node
    S <- S0 * (u^(0:j)) * (d^(j - (0:j)))
    
    # V.new is the vector of option values at each time step and node
    V.new <- pmax(df * (p * V[2:(j+2)] + (1 - p) * V[1:(j+1)]), 0)
    #if((j <= 4) & (j != 0)){
    #  option_value[[j]] <- V.new
    #}
    V[1:(j+1)] <- V.new[1:(j+1)]
    if(verbose){
      cat("Time step: ", j, "\n", sep="")
      cat("Prices:\n")
      print(S)
      cat("Option Values:\n")
      print(V.new[1:(j+1)])
    }
  }
  # calculate the greeks
  #delta <- (f_02 - f_00) / (u^2 * S0 - d^2 * S0)
  #delta <- (option_value[[2]][3] - option_value[[2]][1]) / (u^2 * S0 - d^2 * S0)
  #delta_u <- (option_value[[2]][3] - option_value[[2]][2]) / (u^2 * S0 - S0)
  #delta_d <- (option_value[[2]][2] - option_value[[2]][1]) / (S0 - d^2 * S0)
  #gamma <- (delta_u - delta_d) / (0.5 * (u^2 * S0 - d^2 * S0))
  #theta <- (f_22 - f_01) / (2 * dt)
  #theta <- (option_value[[4]][3] - option_value[[2]][2]) / (2 * dt)
  # The final value is the option price
  f <- V[1]
  #list(option_price=f, delta=delta, gamma=gamma, theta=theta, tree_values=option_value)
  return(f)
}

# Binomial tree to price an american option
americanBinomial <- function(option, N, verbose=FALSE){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "american") stop("must be an american option")
  
  # N: number of time steps
  # type: call or put
  # S0: initial asset value
  # K: strike price
  # r: continuously compounded yearly risk-free rate
  # vol: annualized standard deviation of log return
  # q: continuous dividend yield
  # ttm: time to maturity (in years), i.e. the life of the option
  
  # Extract the parameters of the option
  type <- option$type
  S0 <- option$S0
  K <- option$K
  r <- option$r
  vol <- option$volatility
  q <- option$q
  ttm <- option$maturity
  
  # 1 for call, -1 for put
  if(type == "call"){
    mult <- 1
  } else if(type == "put") {
    mult <- -1
  } else {
    mult <- 0
  }
  
  # List to store option values
  # These are used at the end to compute greeks
  # option_value <- vector("list", 4)
  
  # Time step (delta t)
  dt <- ttm / N
  
  # Size of up move
  u <- exp(vol * sqrt(dt))
  
  # Size of down move
  d <- exp(-vol * sqrt(dt))
  
  # Risk neutral probability of an uptick
  p <- (exp((r - q) * dt) - d)/(u - d)
  
  # Discount factor
  df <- exp(-r * dt)
  
  # At the terminal node, there are N+1 asset values
  V <- pmax(0, mult * (S0 * (u^(0:N)) * (d^(N - (0:N))) - K))
  # if(N == 4) option_value[[4]] <- V
  
  if(verbose){
    cat("Time step: ", N, "\n", sep="")
    cat("Prices:\n")
    print(S0 * (u^(0:N)) * (d^(N - (0:N))))
    cat("Option Values:\n")
    print(V)
  }
  
  # Iterate backward, such that there are j+1 asset values, where j is the
  # Number of time steps
  j.index <-seq(from=N-1, to=0, by=-1)
  for (j in j.index) {
    # S is the vector of prices at each time step and node
    S <- S0 * (u^(0:j)) * (d^(j - (0:j)))
    
    # V.new is the vector of option values at each time step and node
    V.new <- pmax(df * (p * V[2:(j+2)] + (1 - p) * V[1:(j+1)]), mult * (S[1:(j+1)] - K))
    #if((j <= 4) & (j != 0)){
    #  option_value[[j]] <- V.new
    #}
    V[1:(j+1)] <- V.new[1:(j+1)]
    if(verbose){
      cat("Time step: ", j, "\n", sep="")
      cat("Prices:\n")
      print(S)
      cat("Option Values:\n")
      print(V.new[1:(j+1)])
    }
  }
  # calculate the greeks
  #delta <- (f_02 - f_00) / (u^2 * S0 - d^2 * S0)
  #delta <- (option_value[[2]][3] - option_value[[2]][1]) / (u^2 * S0 - d^2 * S0)
  #delta_u <- (option_value[[2]][3] - option_value[[2]][2]) / (u^2 * S0 - S0)
  #delta_d <- (option_value[[2]][2] - option_value[[2]][1]) / (S0 - d^2 * S0)
  #gamma <- (delta_u - delta_d) / (0.5 * (u^2 * S0 - d^2 * S0))
  #theta <- (f_22 - f_01) / (2 * dt)
  #theta <- (option_value[[4]][3] - option_value[[2]][2]) / (2 * dt)
  # The final value is the option price
  f <- V[1]
  #list(option_price=f, delta=delta, gamma=gamma, theta=theta, tree_values=option_value)
  return(f)
}

##### Black-Scholes #####

europeanBS <- function(option){
  if(!is.option(option)) stop("option must be of class 'option'")
  if(option$style != "european") stop("must be a european option")
  
  type <- option$type
  
  S0 <- option$S0
  K <- option$K
  r <- option$r
  q <- option$q
  vol <- option$volatility
  ttm <- option$maturity
  
  if(type == "call"){
    out <- callEuropeanBS(S0=S0, K=K, r=r, q=q, vol=vol, ttm=ttm)
  } else if(type == "put"){
    out <- putEuropeanBS(S0=S0, K=K, r=r, q=q, vol=vol, ttm=ttm)
  } else {
    out <- NULL
  }
  return(out)
}

callEuropeanBS <- function(S0, K, r, q, vol, ttm){
  # S0: inital price of underlying
  # K: strike price
  # r: risk-free rate
  # q: dividend yield
  # vol: annualized volatility
  # ttm: time to maturity (in years)
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  call <- S0 * pnorm(d1) * exp(-q * ttm) - K * pnorm(d2) * exp(-r * ttm)
  return(call)
}

putEuropeanBS <- function(S0, K, r, q, vol, ttm){
  # S0: initial price of underlying
  # K: strike price
  # vol: annualized volatility
  # r: risk-free rate of 
  # rf: risk-free rate of foreign currency
  # ttm: time to maturity in years
  
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  put <- K * exp(-r * ttm) * pnorm(-d2) - S0 * exp(-q * ttm) * pnorm(-d1)
  return(put)
}

##### Greeks #####
# delta
# theta
# gamma
# vega
# rho

#' Option Greeks
#' 
#' Compute the greeks of an option using the Black-Scholes-Merton framework
#' 
#' @param option an \code{option} object created with \code{\link{optionSpec}}
#' @param greek one of "delta", "theta", "gamma", "rho", or "vega"
#' @param prices vector of values to compute the greeks as time
#' to maturity varies
#' @param maturities vector of values to compute the greeks as time
#' to maturity varies
#' @param plot TRUE/FALSE to plot the greek value as the underlying price and/ time to maturity vary
#' @param \dots passthrough parameters to \code{\link{plot}}
#' @param S0 underlying asset price
#' @param K strike price
#' @param r risk free rate
#' @param q continuous dividend yield rate for options on stocks or stock 
#' indices paying a dividend. Also the foreign risk free rate for options on 
#' currencies
#' @param vol volatility of the underlying asset price
#' @param ttm tmie to maturity, the life of the option, measured in years
#' @param type type of the option; "call" or "put"
#' @author Ross Bennett
#' @examples
#' euro.call <- optionSpec(style="european", type="call", S0=30, K=30, maturity=1, r=0.05, volatility=0.25, q=0)
#' # European call greeks
#' computeGreeks(euro.call, greek = "delta")
#' computeGreeks(euro.call, greek = "gamma")
#' computeGreeks(euro.call, greek = "theta")
#' computeGreeks(euro.call, greek = "vega")
#' computeGreeks(euro.call, greek = "rho")
#' 
#' # Plotting
#' computeGreeks(euro.call, "delta", prices = seq(20, 40, 1), plot = TRUE)
#' computeGreeks(euro.call, "delta", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)
#' @aliases deltaBS, thetaBS, gammaBS, vegaBS, rhoBS
#' @export
computeGreeks <- function(option, 
                          greek=c("delta", "theta", "gamma", "rho", "vega"), 
                          prices=NULL, 
                          maturities=NULL, 
                          plot=FALSE, ...){
  
  greek <- match.arg(greek)
  switch(greek,
         delta = {FUN <- match.fun(deltaBS)},
         theta = {FUN <- match.fun(thetaBS)},
         gamma = {FUN <- match.fun(gammaBS)},
         rho = {FUN <- match.fun(rhoBS)},
         vega = {FUN <- match.fun(vegaBS)})
  
  if(!is.null(prices) & !is.null(maturities)){
    out <- vector("list", 2)
    # First compute the greek value as we vary the underlying price, holding ttm constant
    out[[1]] <- FUN(S0 = prices, 
                    K = option$K, 
                    r = option$r, 
                    q = option$q, 
                    vol = option$volatility, 
                    ttm = option$maturity, 
                    type = option$type)
    # Next compute the greek value as we vary time to maturity, holding S0 constant
    out[[2]] <- FUN(S0 = option$S0, 
                    K = option$K, 
                    r = option$r, 
                    q = option$q, 
                    vol = option$volatility, 
                    ttm = maturities, 
                    type = option$type)
    if(plot){
      par(mfrow=c(2,1))
      plot(x = prices, y = out[[1]], type="l", ylab=greek, xlab="price", ...=...)
      plot(x = maturities, y = out[[2]], type="l", ylab=greek, xlab="time to maturity", ...=...)
      par(mfrow=c(1,1))
      invisible(out)
    } else {
      # return the list
      return(out)
    }
  }
  
  if(!is.null(prices)){
    S0 <- prices
    xs <- S0
  } else {
    S0 <- option$S0
  }
  
  if(!is.null(maturities)){
    ttm <- maturities
    xs <- ttm
  } else {
    ttm <- option$maturity
  }
  
  out <- FUN(S0 = S0, 
             K = option$K, 
             r = option$r, 
             q = option$q, 
             vol = option$volatility, 
             ttm = ttm, 
             type = option$type)
  if(plot){
    if(!is.null(maturities)){
      xlab <- "time to maturity"
    } else {
      xlab <- "price"
    }
    plot(x = xs, y = out, type="l", ylab=greek, xlab=xlab, ...=...)
    invisible(out)
  } else {
    return(out)
  }
}

#' @name computeGreeks
#' @export
deltaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- exp(-q * ttm) * pnorm(d1)
  } else if(type == "put"){
    out <- exp(-q * ttm) * (pnorm(d1) - 1)
  } else {
    # Not a valid type
    out <- NULL
  }
  return(out)
}

# delta.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   delta <- exp(-q * ttm) * pnorm(d1)
#   return(delta)
# }
# 
# delta.put <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   delta <- exp(-q * ttm) * (pnorm(d1) - 1)
#   return(delta)
# }

#' @name computeGreeks
#' @export
thetaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) - r * K * exp(-r * ttm) * pnorm(d2)
  } else if(type == "put"){
    out <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) + r * K * exp(-r * ttm) * pnorm(-d2)
  } else {
    # Not a valid type
    out <- NULL
  }
  return(out)
}

# theta.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   theta <- - (S0 * dnorm(d1) * vol) / (2 * sqrt(ttm)) - r * K * exp(-r * ttm) * pnorm(d2)
#   return(theta)
# }
# 
# theta.put <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   theta <- - (S0 * normCDF(d1) * vol) / (2 * sqrt(ttm)) + r * K * exp(-r * ttm) * pnorm(-d2)
#   return(theta)
# }

#' @name computeGreeks
#' @export
gammaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  out <- dnorm(d1) / (S0 * vol * sqrt(ttm))
  return(out)
}

# gamma.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   gamma <- dnorm(d1) / (S0 * vol * sqrt(ttm))
#   return(gamma)
# }
# 
# gamma.put <- gamma.call

#' @name computeGreeks
#' @export
vegaBS <- function(S0, K, r, q, vol, ttm, type){
  d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  out <- S0 * sqrt(ttm) * dnorm(d1)
  return(out)
}

# vega.call <- function(S0, K, r, q, vol, ttm){
#   d1 <- (log(S0 / K) + (r - q + (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   vega <- S0 * sqrt(ttm) * dnorm(d1)
#   return(vega)
# }
# 
# vega.put <- vega.call

#' @name computeGreeks
#' @export
rhoBS <- function(S0, K, r, q, vol, ttm, type){
  d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
  if(type == "call"){
    out <- K * ttm * exp(-r * ttm) * pnorm(d2)
  } else if(type == "put"){
    out <- -K * ttm * exp(-r * ttm) * pnorm(-d2)
  } else {
    out <- NULL
  }
  return(out)
}

# rho.call <- function(S0, K, r, q, vol, ttm){
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   rho <- K * ttm * exp(-r * ttm) * pnorm(d2)
#   return(rho)
# }
# 
# rho.put <- function(S0, K, r, q, vol, ttm){
#   d2 <- (log(S0 / K) + (r - q - (vol^2 / 2)) * ttm) / (vol * sqrt(ttm))
#   rho <- -K * ttm * exp(-r * ttm) * pnorm(-d2)
#   return(rho)
# }

##### Implied Volatility #####

#' Implied Volatility
#' 
#' Compute the implied volatility of a european option using the 
#' Black-Scholes-Merton model.
#' 
#' @details A bisection algorithm is used to compute the implied volatility
#' of a European option priced with the Black-Scholes-Merton model
#' 
#' @param option an \code{option} object created with \code{\link{optionSpec}}
#' @param price market price of the option
#' @param lower the lower bound of implied volatility to search
#' @param upper the upper bound of implied volatility to search
#' @param \dots any passthrough parameters to \code{\link{impliedVolBS}}
#' 
#' @return implied volatility estimate
#' @author Ross Bennett
#' @export
impliedVolatility <- function(option, price, lower=0, upper=0.5, ...){
  if(!is.option(option)) stop("option must be of class 'option'")
  
  if(hasArg(tol)) tol = match.call(expand.dots=TRUE)$tol else tol = sqrt(.Machine$double.eps)
  if(hasArg(max_it)) max_it = match.call(expand.dots=TRUE)$max_it else max_it = 200
  
  out <- try(impliedVolBS(vol_range = c(lower[1], upper[1]), 
                          S0 = option$S0, 
                          K = option$K, 
                          r = option$r, 
                          q = option$q, 
                          ttm = option$maturity, 
                          P_mkt = price, 
                          type = option$type,
                          tol = tol, 
                          max_it = max_it), silent=TRUE)
  if(inherits(out, what = "try-error")){
    print("Bisection algorithm did not converge on implied volatility estimate")
    return(NULL)
  } else {
    return(out)
  }
}

#' Implied Volatility Bisection Method
#' 
#' Bisection method to compute the implied volatility of a european option 
#' using the Black-Scholes-Merton model.
#' 
#' @details A bisection algorithm is used to compute the implied volatility
#' of a European option priced with the Black-Scholes-Merton model
#' 
#' @param vol_range c(lower, upper) the lower and upper bounds of the implied 
#' volatility range to search
#' @param S0 underlying asset price
#' @param K strike price
#' @param r risk free rate
#' @param q continuous dividend yield rate for options on stocks or stock 
#' indices paying a dividend. Also the foreign risk free rate for options on 
#' currencies
#' @param ttm time to maturity, the life of the option, measured in years
#' @param P_mkt market price
#' @param type type of the option; "call" or "put"
#' @param tol tolerance used for stopping criteria
#' @param max_it maximum number of iterations
#' 
#' @return implied volatility estimate
#' @author Ross Bennett
#' @export
impliedVolBS <- function(vol_range, S0, K, r, q, ttm, P_mkt, type, tol=.Machine$double.eps, max_it=200){
  # use bisection to compute the implied volatility
  # http://en.wikipedia.org/wiki/Bisection_method
  tmp_vol_lower <- vol_range[1]
  tmp_vol_upper <- vol_range[2]
  i <- 1
  while(i <= max_it){
    vol_mid <- (tmp_vol_lower + tmp_vol_upper) / 2
    obj_mid <- obj_fun(S0 = S0, K = K, r = r, q = q, sigma = vol_mid, ttm = ttm, P_mkt = P_mkt, type = type)
    if(abs(obj_mid) <= tol){
      out <- vol_mid
    } else{
      tmp_obj_lower <- obj_fun(S0 = S0, K = K, r = r, q = q, sigma = tmp_vol_lower, ttm = ttm, P_mkt = P_mkt, type = type)
      if(sign(obj_mid) == sign(tmp_obj_lower)){
        tmp_vol_lower <- vol_mid
      } else{
        tmp_vol_upper <- vol_mid
      }
    }
    i <- i + 1
  }
  if(i == max_it) warning("maximum iteratations reached")
  # out is the volatility that sets the Black-Scholes model price equal to the 
  # market price
  out <- vol_mid
  
  #out_obj <- obj_fun(S0 = S0, K = K, r = r, q = q, sigma = out, ttm = ttm, P_mkt = P_mkt, type = type)^2
  # check the boundary conditions
  #lb_obj <- obj_fun(S0 = S0, K = K, r = r, q = q, sigma = vol_lower, ttm = ttm, P_mkt = P_mkt, type = type)^2
  #if(lb_obj <= out_obj) warning("Objective function at lower boundary")
  
  #ub_obj <- obj_fun(S0 = S0, K = K, r = r, q = q, sigma = vol_upper, ttm = ttm, P_mkt = P_mkt, type = type)^2
  #if(ub_obj <= out_obj) warning("Objective function at upper boundary")
  
  return(out)
}

# Objective function for use in europeanImpliedVolatility
obj_fun <- function(S0, K, r, q, sigma, ttm, P_mkt, type){
  if(type == "call"){
    out <- callEuropeanBS(S0=S0, K=K, r=r, q=q, vol=sigma, ttm=ttm)
  } else if(type == "put"){
    out <- putEuropeanBS(S0=S0, K=K, r=r, q=q, vol=sigma, ttm=ttm)
  } else {
    stop("A type must be specified")
  }
  return(out - P_mkt)
}

