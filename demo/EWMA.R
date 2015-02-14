# EWMA Demo

library(GARPFRM)
data(crsp_weekly)

# Use the first 5 assets in largecap.ts for the returns data
R <- largecap_weekly[, 1:5]

# The Exponentially Weighted Moving Average (EWMA) Model is a special case of a weighted 
# moving average where the weights decrease exponentially as we move backwards through 
# time. Greater weights are given to more recent observations.
# Estimate volatility via EWMA
volEst <- EWMA(R[,1], lambda=NULL, initialWindow=52, n=26, type="volatility")
volEst
tail(getEstimate(volEst))

# Estimate volatility of each asset
mvVolEst <- EWMA(R, lambda=NULL, initialWindow=52, n=26, type="volatility")
mvVolEst
tail(getEstimate(mvVolEst))
plot(mvVolEst, legendLoc="topright")

# Estimate the covariance matrix via EWMA
covEst <- EWMA(R, 0.94, 15, type="covariance")
names(covEst)
covEst$estimate

# get the covariance between ORCL and HON
covORCLHON <- getCov(covEst, assets=c("ORCL", "HON"))
cov13 <- getCov(covEst, assets=c(1, 3))
all.equal(covORCLHON, cov13)

# Plot the covariance estimate between MSFT and DELL
# Note that we are passing the covEst object created by the EWMA function
plot(covEst, assets=c("MSFT", "DELL"))

# specifying a single asset will extract the variance from the EWMA estimate
varMSFT <- getCov(covEst, assets="MSFT")

# Estimate the correlation matrix
corEst <- EWMA(R, 0.94, 25, TRUE, type="correlation")
corEst

# get the correlation between MSFT and DELL
corMSFTDELL <- getCor(corEst, assets=c("MSFT", "DELL"))
cor24 <- getCor(corEst, assets=c(2, 5))
all.equal(corMSFTDELL, cor24)

# Plot the correlation estimate between ORCL and EMC
# Note that we are passing the covEst object created by the EWMA function
plot(corEst, assets=c("ORCL", "EMC"))