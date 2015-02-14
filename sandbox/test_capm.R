# Efficient Frontier w/ constraints options: short-sale & borrowing
#   Standard Capital Asset Pricing Model

# CAPM Assumptions
#1. Identical investors who are price takers
#2. Investment over the same time horizon 
#3. No transaction costs or taxes
#4. Can borrow and lend at risk-free rate
#5. Investors only care about portfolio expected return and variance
#6. Market consists of all publicly traded assets

# Load Libraries
library(zoo)
library("PerformanceAnalytics")
options(digits=3)

# Read returns from .csv file
stock.df <- read.csv("~/Documents/R_ FRM EXAM/Stocks_data.csv")
colnames(SP_data)

# Estimate a zooreg object: regularly spaced zoo object
stock.z = zooreg(stock.df[,-1], start=c(1993, 1), end=c(2013,11), frequency=12)
index(stock.z) = as.yearmon(index(stock.z))
# Summarize Start, End, and Number of Rows
start(stock.z)
end(stock.z)
nrow(stock.z)

# Estimate excess returns: subtracting off risk-free rate
# To strip off the dates and just return a plain vector/matrix coredata() can be used.
# as.data.frame to check if an object is a data frame, or coerce it if possible.
returns.mat = as.matrix(coredata(stock.z))
exReturns.mat = returns.mat - returns.mat[,"RFREE"]
exReturns.df = as.data.frame(exReturns.mat)

# Run CAPM regression for AAPL (AAPL) using first 4 years
# 48 months divided by 12 months in a years = 4 years
capm.fit = lm(AAPL~MARKET,data=exReturns.df,subset=1:48)
summary(capm.fit)

# plot data and regression line
plot(exReturns.df$MARKET,exReturns.df$AAPL, main="CAPM for AAPL",
     ylab="Excess Return: AAPL",
     xlab="Excess Return: MARKET")
# Plot CAPM regression estimate
abline(capm.fit)    
# Create Axis 
abline(h=0,v=0,lty=3)

# Use a capm.tstats function:
# Estimating CAPM with alpha=0 for asset using first 4 years of data


# Testing CAPM function
tmp = capm.tstats(exReturns.mat[1:60,1],
                  exReturns.mat[1:60,"MARKET"])
tmp
