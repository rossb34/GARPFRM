library(quantmod)

tickers <- c("SPY", "AAPL", "XOM", "GOOG", "MSFT", "GE")

# Download daily data
getSymbols(Symbols=tickers, from="2005-01-01", to="2013-11-22")

# Convert to weekly
for(ticker in tickers) {
  x <- get(ticker)
  x <- to.weekly(x, indexAt='lastof')
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x", ticker, colnames(x))
  assign(ticker, x)
}

# Calculate the returns based on the adjusted close price and combine into a 
# single object
for(i in 1:length(tickers)){
  x <- get(tickers[i])
  x <- Ad(x)
  x.ret <- ROC(x=x, n=1, type="discrete", na.pad=FALSE)
  colnames(x.ret) <- tickers[i]
  if(i == 1){
    returns <- x.ret
    prices <- x
  } else {
    returns <- cbind(returns, x.ret)
    prices <- cbind(prices, x)
  }
}
colnames(prices) <- gsub(".Adjusted", "", colnames(prices))
#save(prices, file="/Users/rossbennett/devel/R/UWGARP/uwgarp/pkg/GARPFRM/data/prices.rda")
#save(returns, file="/Users/rossbennett/devel/R/UWGARP/uwgarp/pkg/GARPFRM/data/returns.rda")
#save(AAPL, GE, GOOG, MSFT, SPY, XOM, file="/Users/rossbennett/devel/R/UWGARP/uwgarp/pkg/GARPFRM/data/equity_data.rda")
