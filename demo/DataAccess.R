

library('knitr')
opts_chunk$set(message=FALSE, fig.path='figures/', fig.align='center', fig.width=4, fig.height=3, fig.keep='last', dev.args=list(pointsize=8))
options(width=80)



library(quantmod)
args(getSymbols)



getSymbols('^GSPC')
chart_Series(GSPC)



getSymbols('DGS3MO',src='FRED')
plot(DGS3MO,main="3-Month Treasury Constant Maturity Rate",cex.main=0.75)


