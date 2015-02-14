# bootstrap
library(GARPFRM)
data(crsp_weekly)
R <- largecap_weekly[,1:4]

# set.seed(123)
# bootFUN(R[,1], FUN="mean", replications=10000, parallel=FALSE)
# 
# library(doMC)
# registerDoMC(2)
# set.seed(123)
# bootFUN(R[,1], FUN="mean", replications=10000, parallel=TRUE)

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

# bootPar <- function(){
#   bootES(R[,1], p=0.92, method="historical", replications=20000, parallel=TRUE)
# }
# bootSeq <- function(){
#   bootES(R[,1], p=0.92, method="historical", replications=20000, parallel=FALSE)
# }
# 
# rbenchmark::benchmark(bootPar(), 
#                       bootSeq(), 
#                       replications=1)

# foo1 <- function(x){
#   # Use sample.int and subset
#   x[sample.int(length(x), replace=TRUE)]
# }

# foo2 <- function(x){
#   # sample directly from the returns
#   sample(x, length(x), replace=TRUE)
# }

# which is faster?

# set.seed(123)
# x <- rnorm(1e6)
# rbenchmark::benchmark(foo1(R[,1]), foo2(R[,1]), replications=1e5)

# foo1 is slightly faster