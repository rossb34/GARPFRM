library(GARPFRM)

data(crsp_weekly)
R <- largecap_weekly

# Run a VaR backtest on MSFT returns
# Compute VaR estimate using gaussian, historical, and modified methods
backtest <- backtestVaR(R[, "MSFT"], window=100, p=0.95, 
                        method=c("gaussian", "historical", "modified"))
backtest

# get the VaR estimates
head(getVaREstimates(backtest))

# get the VaR violations
head(getVaRViolations(backtest))

# plot the VaR backtest
plot(backtest, pch=18, legendLoc="topright")

# Compare the historical VaR bootstrapped historical VaR estimates
btHistorical <- backtestVaR(R[, "MSFT"], window=100, p=0.95, method="historical")

btHistoricalBoot <- backtestVaR(R[, "MSFT"], window=100, p=0.95, 
                                method="historical", replications=100, 
                                bootstrap=TRUE)

btHistorical
btHistoricalBoot

# plot the historical and boostrapped historical VaR backtest
plot(btHistorical, colorset="blue")
lines(getVaREstimates(btHistoricalBoot), col="red")
legend("topright", legend=c("returns", "Historical VaR (0.05)", 
                            "Boot Historical VaR (0.05)"), 
       lty=rep(1,3), col=c("black", "blue", "red"), bty="n", cex=0.75)

# Backtest GARCH Model VaR
garchModel <- uvGARCH(R[, "MSFT"], armaOrder=c(0,0))
btVaR.GARCH <- backtestVaR.GARCH(garchModel, p=0.95, refitEvery=5, window=100)
btVaR.GARCH