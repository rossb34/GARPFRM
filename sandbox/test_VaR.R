library(GARPFRM)
data(crsp.short)
R <- largecap.ts[, 1:4]
temp = R[,1]
# BackTesting Window
initialWindow = 10
CI = 0.99
lags = -1
resultVaR = rollapply(temp, width= initialWindow, FUN = backTestVaR, CI=CI, by.column = FALSE, align = "right")
# VaR lags original data by definition
resultVaR = lag(resultVaR, k=lags)
# Chart together
resultVaR = xts(resultVaR, index(R))
temp = xts(temp, index(temp))

# chart result for the 3 types of models
chart.TimeSeries(cbind(temp,resultVaR), legend.loc="topright")

# Show in table format the result of the chart
VaRviolations = countViolations(resultVaR, temp, initialWindow, CI =0.95)
