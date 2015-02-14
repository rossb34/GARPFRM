
# Exponentially Weighted Moving Average Model
library(GARPFRM)
data(crsp_weekly)

# Use the weekly MSFT returns
R <- largecap_weekly[, "MSFT"]

lambda <- 0.94
initialWindow <- 15
volEst <- EWMA(R, lambda, initialWindow, type="volatility")
volEst

vol <- realizedVol(R, n=5)

plot(vol, main="EWMA Volatility Estimate vs. Realized Volatility")
lines(volEst$estimate, col="red")
legend("topright", legend=c("Realized Volatility", "EWMA Volatility Estimate"), 
       col=c("black", "red"), lty=c(1,1), cex=0.8, bty="n")

# Estimate lambda
# Use initialWindow = 15 for the EWMA volatility estimate and
# n = 5 to calculate the realized volatility
lambda <- estimateLambdaVol(R, initialWindow, n=5)
lambda

volEst2 <- EWMA(R, lambda, initialWindow, type="volatility")
volEst2

# Realized volatility
plot(vol, main="EWMA Volatility Estimate vs. Realized Volatility")
# EWMA volatility estimate, lambda = 0.94
lines(volEst$estimate, col="red")
# EWMA volatility estimate, lambda = 0.0.7359253
lines(volEst2$estimate, col="blue")
legend("topright", legend=c("Realized Volatility", 
                            "EWMA Volatility, lambda = 0.94",
                            "EWMA Volatility, lambda = 0.7359253"),
       col=c("black", "red", "blue"), lty=c(1, 1, 1), cex=0.7, bty="n")

# Use the first 2 columns of the large cap weekly returns
R <- largecap_weekly[,1:2]
initialWindow <- 52
covEst <- EWMA(R, lambda=NULL, initialWindow, n=10, "covariance")
covEst
plot(covEst, main="EWMA Estimated Covariance")

corEst <- EWMA(R, lambda=NULL, initialWindow, n=10, "correlation")
corEst
plot(corEst, main="EWMA Estimated Correlation")

# Use the first 4 columns of the largecap_weekly dataset
R <- largecap_weekly[,1:4]

# calculate the sample covariance matrix
sample_cov <- cov(R)
sample_cov

# EWMA covariance matrix estimate
lambda <- 0.94
initialWindow <- 52
covEst <- EWMA(R, lambda, initialWindow, type="covariance")
covEst

# calculate the sample covariance matrix
sample_cor <- cor(R)
sample_cor

# EWMA covariance matrix estimate
lambda <- 0.94
initialWindow <- 52
corEst <- EWMA(R, lambda, initialWindow, type="correlation")
corEst

# Use the weekly MSFT returns
R <- largecap_weekly[,"MSFT"]

# The GARCH(1,1) Model: Estimating GARCH(1,1) Parameters
# Specify and fit the MSFT returns to a standard ARMA(0,0)-GARCH(1,1) model
# Note that the default is ARMA(1,1)-GARCH(1,1) so we only need to change
# the ARMA order. The default arguments were chosen to be consistent with the 
# default arguments in rugarch.
model <- uvGARCH(R, armaOrder=c(0,0))

# Get the fitted GARCH model
fit <- getFit(model)

# Get the coefficients
coef(fit)

# Show the summary results of the fit
fit

# Using GARCH(1,1) to Forecast Future Volatility
# n period ahead forecast
forecast10 <- forecast(model, nAhead=10)
forecast10

plot(forecast10, which=3)

model11 <- uvGARCH(R, armaOrder=c(0,0), outSample=100)
forecast2 <- forecast(model11, nRoll=10)
forecast2

plot(forecast2, which=4)