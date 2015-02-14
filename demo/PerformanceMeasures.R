
# Load the GARPFRM package and the CRSP dataset.
library(GARPFRM)
data(crsp.short)

# Market returns
R.market <- largecap.ts[, "market"]

# risk free rate
rf <- largecap.ts[,"t90"]

# The portfolio we will consider is an equal weight portfolio of the first 
# 10 assets in largecap.ts
R.portfolio <- Return.portfolio(largecap.ts[,1:10])

# Precompute excess returns
R.Ex.portfolio <- R.portfolio - rf
R.Ex.market <- R.market - rf

# Compute portfolio beta using the covariance of the portfolio and benchmark 
# portfolio returns divided by the variance of the market portfolio returns
cov(R.Ex.portfolio, R.Ex.market) / var(R.Ex.market)

# Compute beta using CAPM
fit <- CAPM(R.Ex.portfolio, R.Ex.market)
getBetas(fit)

# We can also directly use the CAPM.beta function from PerformanceAnalytics
CAPM.beta(R.portfolio, R.market, rf)

# Treynor ratio for portfolio and market

# Treynor Ratio for portfolio
TreynorRatio(R.portfolio, R.market, rf)

# Treynor Ratio for market
TreynorRatio(R.market, R.market, rf)

# Compute Sharpe and annualized Sharpe Ratio
# Sub-period Sharpe Ratio
SharpeRatio(R.portfolio, rf, FUN="StdDev")

# Annualized Sharpe Ratio
SharpeRatio.annualized(R.portfolio, rf)

SharpeRatio(R.portfolio, rf, p=0.95, FUN=c("VaR", "ES"))

# Compute Jensen's alpha by carrying out a linear regression
fit <- lm(R.Ex.portfolio ~ R.Ex.market)
alpha <- coef(fit)[1]
p_value <- coef(summary(fit))[1,4]
summary(fit)

# Compute Jensen's alpha with PerformanceAnalytics function
CAPM.jensenAlpha(R.portfolio, R.market, mean(rf))

# Replicate CAPM.jensenAlpha
# Compute annualized returns
R.P <- Return.annualized(R.portfolio)
R.M <- Return.annualized(R.market)
# Compute the CAPM beta
beta <- CAPM.beta(R.portfolio, R.market, mean(rf))

# Jensen's alpha
R.P - mean(rf) - beta * (R.M - mean(rf))

# Compute Tracking Error
TrackingError(R.portfolio, R.market)

# Replicate TrackingError
sd(R.portfolio - R.market) * sqrt(12)

# Compute Information Ratio
# InformationRatio = ActivePremium / TrackingError
# Active Premium = Investment's annualized return - Benchmark's annualized return
InformationRatio(R.portfolio, R.market)

# Replicate the Information Ratio computation
activePremium <- Return.annualized(R.portfolio) - Return.annualized(R.market)
trackingError <- TrackingError(R.portfolio, R.market)
activePremium / trackingError

# Compute Downside Deviation
MAR <- 0
# PA computation of Downside Deviation
DownsideDeviation(R.portfolio, MAR)

# Compute Sortino Ratio 
SortinoRatio(R.portfolio, MAR)
