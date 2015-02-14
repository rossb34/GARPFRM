library(GARPFRM)
data(crsp_weekly)
R <- largecap_weekly

cnames <- colnames(R)
for(i in 1:ncol(R)){
  hist(R[,i], breaks=50, col="blue", probability=TRUE, main=cnames[i])
  lines(density(R[,i]), lwd=2)
  curve(dnorm(x, mean=mean(R[,i]), sd=sd(R[,i])), 
        add=TRUE, col="red", lty=2, lwd=2)
  Sys.sleep(1)
}

# Use the weekly returns for Conoco Phillips
R.COP <- R[,"COP"]

# Plot the returns
plot(R.COP, main="COP Weekly Returns")

# plot the histogram with kernal density estimate and normal curve overlay
hist(R.COP, breaks=50, main="Histogram of COP Returns", 
     col="lightblue", probability=TRUE)
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
legend("topleft", legend=c("density", "normal"), 
       col=c("black", "red"), lty=c(1, 2), bty="n")

chart.Histogram(R.COP, methods=c("add.normal", "add.rug"), breaks=50)
legend("topleft", legend="normal", 
       col="blue", lty=1, bty="n")

# Quantile-Quantile plot confirms fat left tails
chart.QQPlot(R.COP, envelope=TRUE)


# same plot, zoom in on the left tail
hist(R.COP, breaks=50, main="Histogram of COP Returns", 
     col="lightblue", probability=TRUE, xlim=c(min(density(R.COP)$x), -0.05))
lines(density(R.COP), lwd=2)
curve(dnorm(x, mean=mean(R.COP), sd=sd(R.COP)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.COP)
legend("topleft", legend=c("density", "normal"), 
       col=c("black", "red"), lty=c(1, 2), bty="n")

chart.Histogram(R.COP, methods=c("add.normal", "add.rug"), breaks=50,
                xlim=c(min(density(R.COP)$x), -0.05))
legend("topleft", legend="normal", 
       col="blue", lty=1, bty="n")


# Rolling standard deviation
SD6 <- rollSD(R.COP, 6)
SD13 <- rollSD(R.COP, 13)
SD52 <- rollSD(R.COP, 52)
plot(SD6, type="n", main="Rolling Standard Deviation", ylab="standard deviation")
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

# One period ahead forecast of EWMA model
ewmaVolForecast <- forecast(ewmaModel)

# VaR forecast using EWMA volatility forecast
# Here we assume the expected value of returns is simply the mean of returns
mean(R.COP) + as.numeric(ewmaVolForecast) * qnorm(0.05)


# GARCH Model
garchModel <- uvGARCH(R.COP, armaOrder=c(0,0))
coef(getFit(garchModel))

# One period ahead forecast of GARCH model
garchForecast <- forecast(garchModel, 1)

# VaR forecast using GARCH Model
fitted(garchForecast) + sigma(garchForecast) * qnorm(0.05)
# equivalent to above
quantile(garchForecast, 0.05)


# Historical Simulation

# Estimate VaR directly using historical data

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

# MDE (not enough information in text for example)

# hybrid method

# Return aggregation
# involves a portfolio with multiple positions
# suppose we have an equal weight portfolio of the first 10 assets in the 
# largecap_weekly dataset. Also suppose the portfolio is rebalanced annually.
R <- largecap_weekly[, 1:10]
rebalanceDates <- index(R)[endpoints(index(R), on="years")]

# create an xts object of weights at the specified rebalance dates
weights <- xts(matrix(1 / 10, nrow=length(rebalanceDates), 
                      ncol=10), rebalanceDates)

# calculate the aggregate portfolio return
R.portfolio <- Return.rebalancing(R, weights)
head(R.portfolio)

# Portfolio returns
plot(R.portfolio, main="Portfolio Returns")

# histogram of portfolio returns
hist(R.portfolio, main="Histogram of Portfolio returns", breaks=50, 
     col="blue", probability=TRUE)
lines(density(R.portfolio), lwd=2)
curve(dnorm(x, mean=mean(R.portfolio), sd=sd(R.portfolio)), 
      add=TRUE, col="red", lty=2, lwd=2)
rug(R.portfolio)

# Compute the VaR of the portfolio

# Estimate the VaR of the portfolio using the last 52 periods
# Historical VaR estimate
VaR(tail(R.portfolio, 52), p=0.95, method="historical")

# Bootstrapped VaR estimate
bootVaR(tail(R.portfolio, 52), p=0.95, method="historical")

# Normal VaR estimate
VaR(tail(R.portfolio, 52), p=0.95, method="gaussian")


# GARCH Model
garchModel <- uvGARCH(R.portfolio, armaOrder=c(0,0))
garchForecast <- forecast(garchModel, 1)

# VaR using GARCH Model
fitted(garchForecast) + sigma(garchForecast) * qnorm(0.05)

# Portfolio VaR estimate with EWMA model
ewmaModel <- EWMA(R.portfolio, lambda=NULL, initialWindow, n, type)
# one period ahead forecast
ewmaVolForecast <- forecast(ewmaModel)

# VaR using EWMA volatility forecast
# Here we assume the expected value of returns is simply the mean of returns
mean(R.portfolio) + as.numeric(ewmaVolForecast) * qnorm(0.05)

# variance-covariance approach
# weights matrix
w <- matrix(rep(1 / 10, 10), ncol=1)
# use the most recent 52 periods to compute the covariance matrix
sampleCov <- cov(tail(R, 52))
sqrt(t(w) %*% sampleCov %*% w) * qnorm(0.05)

# EWMA model to compute variance covariance matrix
ewmaCov <- EWMA(tail(R, 52), lambda=0.9, type="covariance")$estimate
sqrt(t(w) %*% ewmaCov %*% w) * qnorm(0.05)


# VaR Backtesting
# GARCH model to estimate VaR
# http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
# http://faculty.washington.edu/ezivot/econ589/econ589univariateGarch.r

garchModel <- uvGARCH(R.portfolio, armaOrder=c(0,0))
modelRoll <- ugarchroll(getSpec(garchModel), data=R.portfolio,window.size=100, 
                        refit.every=5, refit.window="moving", VaR.alpha=c(0.01, 0.05))
estimatedVaR <- modelRoll@forecast$VaR[,"alpha(1%)"]
ret <- modelRoll@forecast$VaR[,"realized"]
violations <- sum(ret < estimatedVaR)
violations / length(ret)

btVaR.GARCH <- backtestVaR.GARCH(garchModel,refitEvery=5, window=100)
btVaR.GARCH
head(getVaREstimates(btVaR.GARCH))
head(getVaRViolations(btVaR.GARCH))
plot(btVaR.GARCH, pch=20)

# Replicate VaR calc from PerformanceAnalytics
# R <- tail(R.portfolio, 52)
# PerformanceAnalytics:::centeredmoment(R, 2)
# mean((R - mean(R))^2)
# 
# PerformanceAnalytics:::VaR.Gaussian
# -mean(R) - sqrt(mean((R - mean(R))^2)) * qnorm(0.05)