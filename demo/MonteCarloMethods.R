library(knitr)
opts_chunk$set(tidy=FALSE, warning=FALSE, fig.width=5, fig.height=5)

suppressPackageStartupMessages(library(GARPFRM))

# Monte Carlo: Geometric Brownian Motion (GBM) mode
# drift rate
mu <- 0

# volatility rate
sigma <- 0.1

# starting price
S0 <- 100

# number of steps
N <- 100

dt <- 1 / N

# Generate N standard normal random variables
set.seed(123)
eps <- rnorm(N)

# Allocate a vector to hold the prices
S <- vector("numeric", N+1)
S[1] <- S0

# Precompute some of the terms
mu_dt <- mu * dt
sig_dt <- sigma * sqrt(dt)

for(i in 2:length(S)){
  S[i] <- S[i-1] + S[i-1] * (mu_dt + sig_dt * eps[i-1])
}
head(S)

# Plot Simulated Price Path
plot(S, main="Simulated Price Path", type="l")

# Allocate a vector to hold the prices
S1 <- vector("numeric", N+1)
S1[1] <- S0

# Precompute terms
mu_sig_dt <- (mu - 0.5 * sigma^2) * dt
sig_dt <- sigma * sqrt(dt)

for(i in 2:length(S1)){
  S1[i] <- S1[i-1] * exp(mu_sig_dt + sig_dt * eps[i-1])
}
head(S1)
plot(S1, main="Simulated Price Path", type="l")

mu <- 0.05
sigma <- 0.15
N <- 10000
time <- 1
steps <- 52
startingValue <- 100

# Run Monte Carlo simulation and store simulated price paths
mcSim <- monteCarlo(mu, sigma, N, time, steps, startingValue)
summary(endingPrices(mcSim))

par(mfrow=c(2,1))
plot(mcSim)
plotEndingPrices(mcSim)
par(mfrow=c(1,1))

data(crsp_weekly)
R.MSFT <- largecap_weekly[, "MSFT"]

# Project number of periods ahead
nAhead <- 5

# Previous price
S.p <- 25

# Using a for loop
bootS <- vector("numeric", nAhead)
for(i in 1:nAhead){
  bootS[i] <- S.p * (1 + sample(R.MSFT, 1, TRUE))
  S.p <- bootS[i]
}
bootS

# Vectorized solution
S.p <- 25
bootS1 <- S.p * cumprod(1 + sample(coredata(R.MSFT), nAhead, TRUE))
bootS1

# Number of boostrap replications
rep <- 10000

# Allocate vector to hold VaR statistic
out <- vector("numeric", rep)
for(i in 1:rep){
  out[i] <- VaR(R.MSFT[sample.int(nrow(R.MSFT), replace=TRUE)], 
                method="historical")
}

# Bootstrapped VaR
mean(out)

# Standard error of Bootstrapped VaR
sd(out)

R <- largecap_weekly[,1:4]

# function to calculate the annualized StdDev using the most recent n periods
foo <- function(R, n){
  StdDev.annualized(tail(R, n), geometric=TRUE)
}

bootFUN(R[,1], FUN="foo", n=104, replications=1000)

# Bootstrap mean estimate.
bootMean(R)

# Bootstrap standard deviation estimate.
bootSD(R)

# Bootstrap standard deviation estimate using the StdDev function from
# PerformanceAnalytics.
bootStdDev(R)

# Bootstrap simpleVolatility estimate.
bootSimpleVolatility(R)

# Bootstrap correlation estimate.
bootCor(R)

# Bootstrap covariance estimate.
bootCov(R)

# Bootstrap Value-at-Risk (VaR) estimate using the VaR function from
# PerformanceAnalytics.
bootVaR(R, p=0.9, method="historical", invert=FALSE)
bootVaR(R, p=0.9, method="gaussian", invert=FALSE)

# Bootstrap Expected Shortfall (ES) estimate using the ES function from
# PerformanceAnalytics. Also known as Conditional Value-at-Risk (CVaR) and 
# Expected Tail Loss (ETL).
bootES(R, p=0.9, method="historical")
bootES(R, p=0.9, method="gaussian")



# Register multicore parallel backend with 3 cores
# Note that this example does not work on Windows
# Windows users should use doSNOW
# library(doMC)
# registerDoMC(3)

# Estimate VaR via bootstrap
# bootVaR(R[,1], p=0.9, method="historical", replications=1000, parallel=TRUE)


# Benchmark the performance of running the bootstrap in parallel
# Bootstrap VaR with parallel=TRUE
# bootPar <- function(){
#   bootVaR(R[,1], p=0.9, method="historical", replications=5000, parallel=TRUE)
# }

# Bootstrap VaR with parallel=FALSE
# bootSeq <- function(){
#   bootVaR(R[,1], p=0.9, method="historical", replications=5000, parallel=FALSE)
# }

# Benchmark these functions
# library(rbenchmark)
# benchmark(bootPar(), bootSeq(), replications=1)[,1:4]