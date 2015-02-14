

library(knitr)
opts_chunk$set(cache=TRUE, tidy=FALSE, warning=FALSE, fig.width=5, fig.height=5)



# Load the package and data
library(GARPFRM)
data(crsp_weekly)
R <- largecap_weekly
R.COP <- R[,"COP"]



plot(R.COP, main="COP Weekly Returns")



hist(R.COP, breaks=50, main="Histogram of COP Returns", 
     col="lightblue", probability=TRUE)
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
legend("topleft", legend=c("density", "normal"), 
       col=c("black", "red"), lty=c(1, 2), bty="n")



hist(R.COP, breaks=50, main="Histogram of COP Returns", 
     col="lightblue", probability=TRUE, 
     xlim=c(min(density(R.COP)$x), -0.05))
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
legend("topleft", legend=c("density", "normal"), 
       col=c("black", "red"), lty=c(1, 2), bty="n")



chart.QQPlot(R.COP)



# Compute rolling standard deviation estimate
SD6 <- rollSD(R.COP, 6)
SD13 <- rollSD(R.COP, 13)
SD52 <- rollSD(R.COP, 52)
# Plot rolling standard deviation estimates
plot(SD6, type="n", main="Rolling Standard Deviation", 
     ylab="standard deviation")
lines(SD6, col="blue", lty=2)
lines(SD13, col="red")
lines(SD52, lwd=2)
legend("topleft", legend=c("rollSD (6)", "rollSD(13)", "rollSD(52)"), 
       bty="n", lty=c(2, 1, 1), col=c("blue", "red", "black"), cex=0.8)



# Estimating volatility
# EWMA Model
initialWindow <- 100
n <- 13
type <- "volatility"

# Fit an EWMA model to estimate volatility
# Choose an optimal lambda parameter that minimizes the mean squared error
# betwen realized volatility and the EWMA model volatility estimate
ewmaModel <- EWMA(R.COP, lambda=NULL, initialWindow, n, type)
ewmaModel

# One period ahead forecast
ewmaVolForecast <- forecast(ewmaModel)

# VaR using EWMA volatility forecast
# Here we assume the expected value of returns is simply the mean of returns
mean(R.COP) + as.numeric(ewmaVolForecast) * qnorm(0.05)



# Specify and fit a GARCH Model
garchModel <- uvGARCH(R.COP, armaOrder=c(0,0))

# One period ahead forecast of GARCH model
garchForecast <- forecast(garchModel, 1)

# VaR forecast using GARCH Model
fitted(garchForecast) + sigma(garchForecast) * qnorm(0.05)



# Historical VaR estimate at the 5% level
historicalVaR5 <- VaR(R.COP, p=0.95, method="historical")

# VaR estimate assuming a normal distribution
normalVaR5 <- VaR(R.COP, p=0.95, method="gaussian")

# Bootstrapped historical VaR estimate 
bootHistVaR5 <- bootVaR(R.COP, p=0.95, method="historical")

rnames <- c("Historical", "Normal", "Bootstrap Historical")
matrix(c(historicalVaR5, normalVaR5, bootHistVaR5[1,]), 
       nrow=3, ncol=1, dimnames=list(rnames, "VaR (5%)"))



hist(R.COP, main="Histogram of COP returns", breaks=50, 
     col="blue", probability=TRUE)
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
arrows(historicalVaR5, 0, historicalVaR5, 6, code=1, lwd=2)
text(historicalVaR5, 6, labels="historical VaR (5%)", pos=2, cex=0.7)



# Estimating VaR at the 1% level
historicalVaR1 <- VaR(R.COP, p=0.99, method="historical")
normalVaR1 <- VaR(R.COP, p=0.99, method="gaussian")
bootHistVaR1 <- bootVaR(R.COP, p=0.99, method="historical")

matrix(c(historicalVaR1, normalVaR1, bootHistVaR1[1,]), 
       nrow=3, ncol=1, dimnames=list(rnames, "VaR (1%)"))



hist(R.COP, main="Histogram of COP returns", breaks=50, 
     col="blue", probability=TRUE)
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
arrows(historicalVaR1, 0, historicalVaR1, 4, code=1, lwd=2)
text(historicalVaR1, 4, labels="Historical VaR (1%)", pos=2, cex=0.7)
arrows(normalVaR1, 0, normalVaR1, 6, code=1, lwd=2)
text(normalVaR1, 6, labels="Normal VaR (1%)", pos=2, cex=0.7)



# Asset returns
R <- largecap_weekly[, 1:10]

# Lookback period
K <- 52

# Set the weights K periods ago
weights <- xts(matrix(rep(1 / 10, 10), nrow=1), index(R)[nrow(R) - K])

# Calculate the portfolio returns for the most recent K periods
R.portfolio <- Return.rebalancing(R, weights)



par(mfrow=c(2,1))
# Portfolio returns
plot(R.portfolio, main="Portfolio Returns")

# Histogram of portfolio returns
hist(R.portfolio, main="Histogram of Portfolio returns", breaks=50, 
     col="blue", probability=TRUE)
lines(density(R.portfolio), lwd=2)
curve(dnorm(x, mean=mean(R.portfolio), sd=sd(R.portfolio)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.portfolio)
par(mfrow=c(1,1))



# Estimate the VaR of the portfolio using the last 52 periods
# Historical VaR estimate
portfVaR.HS <- VaR(R.portfolio, p=0.95, method="historical")

# Bootstrapped VaR estimate
portfVaR.BootHS <- bootVaR(R.portfolio, p=0.95, method="historical")

# Normal VaR estimate
portfVaR.normal <- VaR(R.portfolio, p=0.95, method="gaussian")



# Use the most recent 52 periods to compute the sample covariance matrix
sampleCov <- cov(tail(R, 52))
# Convert the xts object of weights to a matrix
weights <- matrix(weights, ncol=1)

# Compute the portfolio VaR estimate using the VarCov approach with sample
# covariance matrix
portfVaR.cov <- sqrt(t(weights) %*% sampleCov %*% weights) * qnorm(0.05)

# Use EWMA model to compute variance covariance matrix
# EWMA model to compute variance covariance matrix
ewmaCov <- EWMA(tail(R, 52), lambda=0.9, initialWindow=10, 
                type="covariance")$estimate

# Compute the portfolio VaR estimate using the VarCov approach with EWMA
# model estimated covariance matrix
portfVaR.ewmaCov <- sqrt(t(weights) %*% ewmaCov %*% weights) * qnorm(0.05)



# Compute rolling correlation estimates
cor13 <- rollCor(R[,1:2], 13)
cor26 <- rollCor(R[,1:2], 26)

# Compute rolling covariance estimates
cov13 <- rollCov(R[,1:2], 13)
cov26 <- rollCov(R[,1:2], 26)

# Plot rolling correlation estimates
plot(cor13, type="n", main="Rolling Correlation", 
     ylab="correlation")
lines(cor13, col="blue")
lines(cor26, col="red")
legend("topleft", legend=c("rollCor(13)", "rollCor(26)"), 
       bty="n", lty=c(1, 1), col=c("blue", "red"), cex=0.8)

# Plot rolling covariance estimates
plot(cov13, type="n", main="Rolling Covariance", 
     ylab="covariance")
lines(cov13, col="blue")
lines(cov26, col="red")
legend("topleft", legend=c("rollCov(13)", "rollCov(26)"), 
       bty="n", lty=c(1, 1), col=c("blue", "red"), cex=0.8)



# Portfolio VaR estimate with EWMA model
ewmaModel <- EWMA(R.portfolio, lambda=NULL, initialWindow=10, n, type)
# One period ahead forecast
ewmaVolForecast <- forecast(ewmaModel)

# VaR using EWMA volatility forecast
# Here we assume the expected value of returns is simply the mean of returns
portfVaR.EWMA <- mean(R.portfolio) + as.numeric(ewmaVolForecast) * qnorm(0.05)



dfVaR <- t(data.frame(portfVaR.HS, portfVaR.BootHS[1,], portfVaR.normal, 
                    portfVaR.cov, portfVaR.ewmaCov, portfVaR.EWMA))
rownames(dfVaR) <- c("portfVaR.HS", "portfVaR.BootHS", "portfVaR.normal", 
                    "portfVaR.cov", "portfVaR.ewmaCov", "portfVaR.EWMA")
dfVaR



R <- largecap_weekly[, 1:10]

# Annual rebalance dates
rebalanceDates <- index(R)[endpoints(index(R), on="years")]

# Create an xts object of weights at the specified rebalance dates
weights <- xts(matrix(1 / 10, nrow=length(rebalanceDates), 
                      ncol=10), rebalanceDates)

# Calculate the aggregate portfolio return
R.portfolio <- Return.rebalancing(R, weights)



par(mfrow=c(2,1))
# Portfolio returns
plot(R.portfolio, main="Portfolio Returns")

# Histogram of portfolio returns
hist(R.portfolio, main="Histogram of Portfolio returns", breaks=50, 
     col="blue", probability=TRUE)
lines(density(R.portfolio), lwd=2)
curve(dnorm(x, mean=mean(R.portfolio), sd=sd(R.portfolio)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.portfolio)
par(mfrow=c(1,1))



garchModel <- uvGARCH(R.portfolio, armaOrder=c(0,0))
btVaR.GARCH <- backtestVaR.GARCH(garchModel, p=0.95, refitEvery=5, window=100)
btVaR.GARCH



# Plot the GARCH VaR backtest
plot(btVaR.GARCH, pch=20, legendLoc="topright")



# Run a VaR backtest on portfolio returns
# Compute VaR estimate using gaussian, historical, and modified methods
backtest <- backtestVaR(R.portfolio, window=100, p=0.95, 
                        method=c("gaussian", "historical", "modified"))
backtest



# plot the VaR backtest
plot(backtest, pch=18, legendLoc="topright")


