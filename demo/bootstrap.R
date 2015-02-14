# Bootstrapping is a statistical method for estimating the sampling 
# distribution of an estimator by sampling with replacement from the original sample.
# bootstrap
library(GARPFRM)
data(crsp_weekly)
R <- largecap_weekly[,1:4]

# function to calculate the annualized return using the most recent n periods
foo <- function(R, n){
  Return.annualized(tail(R, n), geometric=TRUE)
}

bootFUN(R[,1], FUN="foo", n=104, replications=100)

# Bootstrap various statistics

# Bootstrap mean estimate.
bootMean(R[,1])
bootMean(R)

# Bootstrap standard deviation estimate.
bootSD(R[,1])
bootSD(R)

# Bootstrap standard deviation estimate using the StdDev function from
# PerformanceAnalytics.
bootStdDev(R[,1])
bootStdDev(R)

# Bootstrap simpleVolatility estimate.
bootSimpleVolatility(R[,1])
bootSimpleVolatility(R)

# Bootstrap correlation estimate.
bootCor(R[,1:2])
bootCor(R[,1:2], method="kendall")
bootCor(R)

# Bootstrap covariance estimate.
bootCov(R[,1:2])
bootCov(R)

# Here is an example of how to calculate historical Value-at-Risk with bootstrapped returns.
# Bootstrap Value-at-Risk (VaR) estimate using the VaR function from
# PerformanceAnalytics.
bootVaR(R[,1], p=0.9, method="historical")
bootVaR(R[,1], p=0.9, method="gaussian")
bootVaR(R, p=0.9, method="historical", invert=FALSE)

# Bootstrap Expected Shortfall (ES) estimate using the ES function from
# PerformanceAnalytics. Also known as Conditional Value-at-Risk (CVaR) and 
# Expected Tail Loss (ETL).
bootES(R[,1], p=0.9, method="gaussian")
bootES(R[,1], p=0.92, method="historical", invert=FALSE)
bootES(R, p=0.9, method="historical")