
#' Simple Volatility
#' 
#' This function calculates the volatility of asset returns using a simplified
#' equation.
#' 
#' @details
#' This function is different from \code{sd} in two ways.
#' \itemize{
#'   \item \code{simpleVolatility} uses a denominator of \code{n}.
#'   \item \code{simpleVolatility} assumes the mean to be zero.
#' }
#' The simple volatility of x is defined as
#' 
#' \deqn{
#'   \sigma = \sqrt{\frac{1}{n} \sum_{i=1}^n x_i^2}
#' }
#' 
#' @param R xts or zoo object of asset returns
#' @author Ross Bennett
#' @seealso \code{\link{sd}}
#' @examples
#' data(crsp_weekly)
#' R <- largecap_weekly[,1:4]
#' simpleVolatility(R[,1])
#' simpleVolatility(R)
#' @export
simpleVolatility <- function(R){
  if(!inherits(R, c("xts", "zoo"))) stop("R must be an xts or zoo object")
  n <- nrow(R)
  if(ncol(R) == 1){
    out <- .simpleVolatility(R)
  } else if(ncol(R) > 1){
    out <- apply(R, 2, .simpleVolatility)
  }
  names(out) <- colnames(R)
  return(out)
}

.simpleVolatility <- function(R){
  n <- length(R)
  out <- sqrt(sum(R^2) / n)
  return(out)
}

