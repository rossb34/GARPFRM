library(PerformanceAnalytics)
library(GARPFRM)
library(rugarch)
library(rmgarch)
data(crsp.short)
R <- largecap.ts[, 1:4]
options(digits=4)

# Remember: log-returns for GARCH analysis
asset1 = R[,1] 
asset2 = R[,3]
   
# Create combined data series
cAssets = cbind(asset1,asset2)

# Scatterplot of returns
plot(coredata(asset1), coredata(asset2), xlab=colnames(asset1), ylab=colnames(asset2), main ="Scatterplot of Returns")
abline(h=0,v=0,lty=3)

# Compute rolling cor to illustrate the later smoothing effect of EWMA
cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

rollCov = rollapply(cAssets, FUN=cov.fun, width=10, by.column=FALSE, align="right")
rollCor = rollapply(cAssets, FUN=cor.fun, width=10, by.column=FALSE, align="right")
par(mfrow=c(2,1))
# First Rolling Cov
plot(na.omit(rollCov), main="20-Day Rolling Cov", ylab="covariance")
grid()
abline(h=cov(cAssets)[1,2], lwd=3, col="red")

# Second Rolling Cor
plot(na.omit(rollCor), main="20-Day Rolling Cor",ylab="correlation")
grid()
abline(h=cor(cAssets)[1,2], lwd=3, col="red")
par(mfrow=c(1,1))

# Calculate EWMA cov and cor, applying default lambda - 0.96
cAssetsEWMACov <- EWMA(cAssets,lambda=0.94, initialWindow=30, cor=FALSE)
cAssetsEWMACor <- EWMA(cAssets,lambda=0.94, initialWindow=30, cor=TRUE)

# Plots
par(mfrow=c(2,1))
plot(cAssetsEWMACov,asset1=1,asset2=2)
plot(cAssetsEWMACor, asset1=1,asset2=2)
par(mfrow=c(1,1))

# Compute EWMA cov and cor for longer half-life of 
halfLife = log(0.5)/log(0.94) + 5
lambda = exp(log(0.5)/halfLife)
covEwma <- EWMA(cAssets, lambda)

# Garch11 testing
data(returns)
cAssetsReturns = cbind(returns[, "SPY"],returns[,"AAPL"])
# Dynamic Conditional Cor/Cov
garch11 <- garch11(cAssetsReturns)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, residuals, plot, infocriteria, rcor, rcov show, nisurface
# show dcc fit
garch11

# Conditional Sigma (vs Realized Absolute Returns)
plot(garch11, which=2)

# Conditional covar of each series
plot(garch11, which=3)

# Conditional cor
plot(garch11, which=4)

# extracting correlation series
ts.plot(rcor(garch11)[1,2,])

# Forecasting conditional vol and cor, default wd = 100
fcstGarch11 = fcstGarch11(garch11,100)

# Many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# Show forecasts
fcstGarch11

# Plot Conditional Covar Forecast 
plot(fcstGarch11, which=3)