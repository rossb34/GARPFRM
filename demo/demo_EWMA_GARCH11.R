library(PerformanceAnalytics)
library(GARPFRM)
library(rugarch)
library(rmgarch)
data(crsp.short)
R <- largecap.ts[, 1:4]
options(digits=4)


# Remember: log-returns for GARCH analysis
temp_1 = R[,1] 
temp_2 = R[,3]

# Create combined data series
temp = merge(temp_1,temp_2)

# scatterplot of returns
plot(coredata(temp_1), coredata(temp_2), xlab=colnames(temp_1), ylab=colnames(temp_2), 
     main ="Scatterplot of Returns")
abline(h=0,v=0,lty=3)

# Compute rolling cor
cor.fun = function(x){
  cor(x)[1,2]
}

cov.fun = function(x){
  cov(x)[1,2]
}

roll.cov = rollapply(as.zoo(temp), FUN=cov.fun, width=20,
                     by.column=FALSE, align="right")
roll.cor = rollapply(as.zoo(temp), FUN=cor.fun, width=20,
                     by.column=FALSE, align="right")
par(mfrow=c(2,1))
# First Rolling Cov
plot(roll.cov, main="20-Day Rolling Cov",
     ylab="covariance", lwd=3, col="blue")
grid()
abline(h=cov(temp)[1,2], lwd=3, col="red")

# Second Rolling Cor
plot(roll.cor, main="20-Day Rolling Cor",
     ylab="correlation", lwd=3, col="blue")
grid()
abline(h=cor(temp)[1,2], lwd=3, col="red")
par(mfrow=c(1,1))

# Calculate EWMA cov and cor, applying default lambda - 0.96
tempEWMACov <- EWMA(temp,lambda=0.94, initialWindow=10, cor=FALSE)
tempEWMACor <- EWMA(temp,lambda=0.94, initialWindow=10, cor=TRUE)

# Plots
par(mfrow=c(2,1))
plot(tempEWMACov,asset1=1,asset2=2)
plot(tempEWMACor, asset1=1,asset2=2)
par(mfrow=c(1,1))

# Compute EWMA cov and cor for longer half-life of 
halfLife = log(0.5)/log(0.94) + 5
lambda = exp(log(0.5)/halfLife)
covEwma <- EWMA(temp, lambda)

# Garch11 testing
data(returns)
tempReturns = cbind(returns[, "SPY"],returns[,"AAPL"])
# Dynamic Conditional Cor/Cov
garch11 <- garch11(tempReturns)

# many extractor functions - see help on DCCfit object
# coef, likelihood, rshape, rskew, fitted, sigma, residuals, plot, infocriteria, rcor, rcov show, nisurface
# show dcc fit
garch11

# Conditional sd of each series
plot(garch11, which=2)

# Conditional covar of each series
plot(garch11, which=3)

# Conditional cor
plot(garch11, which=4)

# extracting correlation series
ts.plot(rcor(garch11)[1,2,])

# Forecasting conditional vol and cor, default wd = 100
fcstGarch11 = fcstGarch11(garch11,100)

class(fcstGarch11)
slotNames(fcstGarch11)
class(fcstGarch11@mforecast)
names(fcstGarch11@mforecast)

# many method functions - see help on DCCforecast class
# rshape, rskew, fitted, sigma, plot, rcor, rcov, show

# Show forecasts
fcstGarch11

# Plot Conditional Covar Forecast 
plot(fcstGarch11, which=3)