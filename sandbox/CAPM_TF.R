# Load the GARPFRM package and CRSP dataset for CAPM analysis.
# Standard Capital Asset Pricing Model (CAPM) fitting and testing using CRSP data. 
# Where CAPM describes the relationship between risk and expected return.
suppressMessages(library(GARPFRM))
options(digits=3)
data(crsp.short)

stock.df <- largecap.ts[, 1:20]
mrkt <- largecap.ts[, "market"]
rfr <- largecap.ts[, "t90"]

# Plot first four stocks from 
plot.zoo(stock.df[,1:4], main="First Four Large Cap Returns")

# Illustrate the type of data being analzyed: start-end dates.
start(stock.df[,1:4])
end(stock.df[,1:4])
# Count the number of rows: sample size.
nrow(stock.df)

# Excess Returns initialized before utilizing in CAPM
exReturns <- Return.excess(stock.df, rfr)
colnames(exReturns)= c(colnames(stock.df))


# Univariate CAPM
uv <- CAPM(exReturns[,1], mrkt)
getStatistics(uv)

# Plot data with regression line
plot(uv)

# MLM CAPM for AMAT, AMGN, and CAT
mlm <- CAPM(exReturns[,1:3], mrkt)
getStatistics(mlm)

# Plot data with regression line
plot(mlm)

# For uv example
# Estimate CAPM with alpha = 0 & beta = 1 for asset
getBetas(uv)
getAlphas(uv)
hypTest(uv, significanceLevel=0.05)
# For mlm
getBetas(mlm)
getAlphas(mlm)
hypTest(mlm, significanceLevel=0.05)

# The CAPM function can handle multiple assets at once, 
# and will cycle through each asset one at a time and output the results.
# MLM CAPM
mlm <- CAPM(exReturns[,], mrkt)

# Plot expected returns versus betas
chartSML(mlm)


# Load FED consumption data: CONS
# To illustate the power of the CAPM model 
# test its relationship with explanatory variable con- sumption.
data(consumption)

# Convert to yearmon index and align consumption and mrkt
consumption <- xts(consumption, as.yearmon(index(consumption)))
mrkt <- xts(mrkt, as.yearmon(index(mrkt)))
consumption <- consumption[index(mrkt)]

capm.cons = CAPM(consumption, mrkt)
coef(summary(capm.cons))

# Plot data with regression line
plot(capm.cons)
