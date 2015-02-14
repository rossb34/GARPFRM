# 'Load the GARPFRM package and the CAPM dataset.
suppressMessages(library(GARPFRM))
options(digits=5)
data(crsp.short)

# Make the assumption that the last two time series are the Market and RF series

#stock_rets.df <- largecap.ts

# Store results
Measures = rep(NULL,4)



# If equalWts is true portfolio weights will be 1/numberOfSecurities
equalWts = TRUE

randWts = TRUE
if(equalWts) randWts = FALSE

# Number of securities to use is 
numberOfSecurities = 3

# Security Selection Method, randomSeq or serialSeq
secSel = "serialSeq"

# type of chaining in forming the aggregate return
# If geometric is TRUE use geometric chaining if False use arithemetic chaining
geometricB = TRUE

# Definitioned/Constant variables
relation = c("Less Than","Equal To","Greater Than")

# Some of run time parameters
# Should be amongst the input for shiny
# Minimum Acceptable Return for Sortino Ratio or Downside Deviation
# defaults to 0
MAR_set = 0

# Denominator for Sortino Ratio or Downside Deviation 
# one of "full" or "subset", indicating whether to use the length of the full series
# or the length of the subset of the series below the MAR as the denominator,
# defaults to "full"
denom = "full"

# Portfolio name used in output
PortName = "Portfolio"

# Market name used in output
MrktName ="Market"

#Market Name in data set
mrktDataName = "market"

# Risk free rate name used in output
RFName="Risk_Free_Rate"

# Risk Free Name in data set
rfDataName = "t90"

Measures = rbind(Measures, c("Series Name",PortName,MrktName,RFName))
colnames(Measures) = c("Measure","Portfolio","Market","RiskFree")

# List column headers predominantly tickers/name of securities

# Summarize the first and last data values corresponding to the first 5 dates for the first 5 returns.

colnames(largecap.ts)
head(largecap.ts)
tail(largecap.ts)

# Number time periods for year for data set
freQ = 12



nRow = nrow(largecap.ts)
nRow

# pointer to column with the market returns
# ptrMkt = which(colnames(stock.z)==mrktDataName,arr.ind=TRUE)
ptrMkt = which(colnames(largecap.ts)==mrktDataName,arr.ind=TRUE)
ptrMkt
# pointer to column of Risk Free Ratw
ptrRF = which(colnames(largecap.ts)==rfDataName,arr.ind=TRUE)
ptrRF

# Market returns
R.market <- largecap.ts[, ptrMkt]

# risk free rate
rf <- largecap.ts[, ptrRF]
## 


# Number of time series in data set
nSec = ncol(largecap.ts)

if (numberOfSecurities>(nSec-2)) numberOfSecurities = nSec - 2
cat('\n Number of Securities being used in portfolio = ',numberOfSecurities, '\n')

# Security position numbers of securities in portfolio
secNbrs = seq(1,numberOfSecurities)
if(secSel=="randomSeq") secNbrs = sample.int(nSec, size = numberOfSecurities, replace = FALSE)

cat('\n Selected Security position indicies')
print(secNbrs)

#
# Created random wts from the non-market non RF series if selected
if (randWts){
  coefs = runif(numberOfSecurities)
  alphas = coefs/sum(coefs)
  sum(alphas)
}  

# Generate portfolio from selected securities and associated weigts
if (equalWts) {
  R.portfolio <- Return.portfolio(largecap.ts[, secNbrs],geometric=geometricB)  
} else {
  R.portfolio = Return.portfolio(largecap.ts[, secNbrs],weights=alphas,geometric=geometricB) 
}

# Print names of securities in portfolio
cat('\n Securities in Portfolio')
print(colnames(largecap.ts)[secNbrs]) 

colnames(portfolio) ="PortRets"

salient = matrix(data=c("Mean",mean(R.portfolio),mean(R.market),mean(rf),
                   "Stdev",sd(R.portfolio),sd(R.market),sd(rf),
                   "NbrSecuritiesInPort",numberOfSecurities,"","",
                   "AnnuallyFrequency",rep(freQ,3),
                   "Number of Samples",rep(nRow,3)),ncol=4,byrow=TRUE)

Measures = rbind(Measures, salient)


# Start to compute performance measures using performance analytic and as a check
# compute from underlying formula

# Some of comparison match, but some are inexplicable different.
# Really should investigate the latter,  I am sure the users will do so

# Compute beta
beta = cov(R.portfolio,R.market)/var(R.market)
betaRF = cov((R.portfolio[,1]-rf),(R.market-rf))/var((R.market-rf))

cat('\n Beta =',beta,', between the portfolio represented by ',PortName,' and the market represented by ',MrktName,' \n')
cat('\n Beta =',betaRF,', between the excess portfolio represented by ',PortName,' and the excess market represented by ',MrktName,' \n')

Measures = rbind(Measures,c("Beta",betaRF,"",""))

# Compute Treynor

   # Performance Analytics (PA) computation of Treynor for portfolio and market
tr = TreynorRatio(R.portfolio, R.market,rf,freQ )
trMarket = TreynorRatio(R.market,R.market,rf,freQ )
  
   # From formula (Hand) computation of Treynor for portfolio and market
trHandGeom = (prod(1+(R.portfolio-rf))^(freQ/nRow) - 1)/betaRF
trHand = freQ*mean(R.portfolio-rf)/betaRF

# beta of Treynor for market is 1
trHandMGeom = (prod(1+(R.market-rf))^(freQ/nRow) - 1)
trHandM = freQ*(mean(R.market-rf))

   # Output results from Treynor Ratio computation
cat('\n PA computed for ',PortName,' Treynor Ratio = ',tr,'\n')
cat('\n Hand computed for ',PortName,' Treynor Ratio = ',trHand,'\n')
cat('\n PA computed for ',MrktName,' Treynor Ratio = ',trMarket,'\n')
cat('\n Hand computed for ',MrktName,' Treynor Ratio = ',trHandM,'\n')

Measures = rbind(Measures,c("Treynor Ratio",tr,trMarket,""))

   # Compare Treynor Ratio for market returns against Treynor Ratio for portfolio returns
cat('\n Treynor Ratio of ',MrktName,' is ',relation[sign(trMarket - tr) + 2],' Treynor Ratio of ',PortName,'\n' )
TR_Mrkt2Port = relation[sign(trMarket - tr) + 2]

##
# calculate excess returns of R.portfolio
R.portfolio.x <- R.portfolio - rf

# calculate excess returns of R.market
R.market.x <- R.market - rf

# run regression to get beta
fit <- lm(R.portfolio.x ~ R.market.x)
beta.lm <- coef(fit)[2]

# calculate beta another way
beta2 <- as.numeric(cov(R.portfolio.x, R.market.x) / var(R.market.x))

# Annualized portfolio excess returns / beta
# This should match the TreynorRatio from PerformanceAnalytics
TR.rb <- as.numeric(Return.annualized(R.portfolio.x,scale=12) / beta.lm)
TR.rb2 <- as.numeric(Return.annualized(R.portfolio.x,scale=12,geometric=FALSE) / beta.lm)

as.numeric(Return.annualized(R.portfolio.x) / beta2)

# Calcualte Treynor Ratio with PerformanceAnalytics
TR.PA <- TreynorRatio(R.portfolio, R.market, rf)

all.equal(TR.rb, TR.PA)
cat("Treynor Ratio by hand: ", TR.rb, "\n")
cat("Treynor Ratio with PerformanceAnalytics: ", TR.PA, "\n")
##

# Compute Sharpe

   # PA computation of Sharpe
shr = SharpeRatio(R.portfolio, rf, FUN = "StdDev")
shrAnn = SharpeRatio.annualized(R.portfolio, rf, scale=freQ, geometric=TRUE)

   # Compute excess portfolio returns (portfolio returns net of RF returns) 
xCess = R.portfolio - rf
   # Hand computation of Sharpe

shrHand = mean(xCess)/sd(xCess)

shrHandAnn =((prod(1+xCess))^(freQ/nRow) - 1)/(sqrt(freQ)*sd(xCess))

   # Output results from Sharpe Ratio computation
cat('\n PA computed ',PortName,' Sharpe Ratio = ',shr,'\n')
cat('\n Hand computed ',PortName,' Sharpe Ratio = ',shrHand,'\n')

cat('\n PA computed ',PortName,' Annualized Sharpe Ratio = ',shrAnn,'\n')
cat('\n Hand computed ',PortName,' Annualized Sharpe Ratio = ',shrHandAnn,'\n')

cat('\n Difference PA and hand computed Sharpe Ratio = ',as.numeric(shr) - as.numeric(shrHand),'\n')


Measures = rbind(Measures,matrix(c("Sharpe Ratio",shr,"","","Sharpe Ratio Annualized",shrAnn,"",""),ncol=4,byrow=TRUE))

# Compute Jensen's Alpha

   # Mean of risk free rate over sampling time series
meanRF = mean(rf)

   # PA computation of Jensen's Alpha

## The risk-free rate varies through time so just computing the mean
## is oversimplifying and could be part of the reason for the differences
## you are seeing between your hand calculation and PerformanceAnalytics

   # True but If put rf rather than meanRF will get a series of ja's
   # rather than a single value  

   # It appears to be an annualized value

   # Produces one value
jaStatic = CAPM.jensenAlpha(R.portfolio,R.market,meanRF)

   # Produces multiple values
jaTV = CAPM.jensenAlpha(R.portfolio, R.market,rf)


# Hand computation of Jensen's Alpha using regression apprach


   # Precompute excess returns

xCessP1 = R.portfolio - rf
xCessM1 = R.market - rf

   # Regression computation and salient results
lm_ja1.fit = lm(xCessP1 ~ xCessM1)
values1 = summary(lm_ja1.fit)
values1
analT1 = values1[[4]]

   # Annualize Jensen's Alpha
   # (mean(R.portfolio) - meanRF - betaRF*(mean(R.market)-meanRF)) same as regression coefficent
jaHandA = (1+(mean(R.portfolio) - meanRF - betaRF*(mean(R.market)-meanRF)))^freQ -1
   
   # Alternative annualization using individual values
jaHandA2 = prod((1+((R.portfolio - rf) - as.numeric(betaRF)*(R.market-rf))))^(freQ/nRow) - 1


   # Use Delta Method to approximate standard error of analyzed Jensen's Alpha
deltaCoef = (freQ*(1+analT1[1,1])^(freQ-1))^2

   # Output salient results for Jensen's Alpha
cat('\n PA computed ',PortName,' Static Jensen\'s Alpha = ',jaStatic,'\n')
cat('\n Regression computed ',PortName,' Jensen\'s Alpha, Not Annualized= ',analT1[1,1],'\n')
cat('\n Hand computed Annualized ',PortName,' Jensen Alpha = ',jaHandA,'\n')
cat('\n Delta Method Coefficient Value = ', deltaCoef,'\n')

   # t stat and pvalue under H0: alpha = 0 against HA: alpha != 0 
tStat = jaHandA/(analT1[1,2]*deltaCoef^0.5)
pValue = 2*(1-pt(tStat,nRow-2))
cat('\n H0: alpha = 0, HA: alpha != 0  p-value: ',pValue,'\n')

Measures = rbind(Measures,matrix(c("Jensen's Alpha",jaStatic,"","","Jensen's Alpha Annualized",jaHandA,"","",
                                   "Jensen's Alpha P Value",pValue,"",""),ncol=4,byrow=TRUE))


# Compute Tracking Error

   # PA computation of Tracking Error   
te = TrackingError(R.portfolio, R.market, scale = freQ)

   # Precompute required values to computr Tracking Error by Hand
   # Lipper Definition ARR =SUM(sp RR)/n sp, where sp == sub-period    

RR = R.portfolio-R.market
ARR = mean(RR)

   # Hand computation of Tracking Error   
teHand = sqrt(sum((RR-ARR)^2)/(nRow-1))*sqrt(freQ)

   # Output results from Tracking Error computation
cat('\n PA computed ',PortName,' Tracking Error = ',te,'\n')
cat('\n Hand computed ', PortName,' Tracking Error = ',teHand,'\n')
cat('\n Difference PA_TE - Hand_TE = ',te - teHand,'\n')

Measures = rbind(Measures,c("Tracking Error",te,"",""))

# Compute Information Ratio

   # PA computation of Information Ratio
ir = InformationRatio(R.portfolio, R.market, scale = freQ)

   # Hand computation of Information Ratio 
   # Lipper definition = ARR/TE
irHand = (ARR/te)*sqrt(freQ)

# Definition PA, InformationRatio = ActivePremium/TrackingError
#Active Premium = Investment's annualized return - Benchmark's annualized return
irHand2 = ((prod(1+(R.portfolio))^(freQ/nRow) - prod(1+(R.market))^(freQ/nRow))/te)

# Output results from Information Ratio computation  
cat('\n PA computed ',PortName,' Information Ratio = ',ir,'\n')
cat('\n Hand computed ',PortName,' Lipper Method Information Ratio = ',irHand,'\n')
cat('\n Hand computed ',PortName,' PA formula Information Ratio = ',irHand,'\n')

Measures = rbind(Measures,c("Information Ratio",ir,"",""))

# Compute Sortino Ratio (including Downside Deviation)

   # PA computation of Sortino Ratio
sr = SortinoRatio(R.portfolio, MAR = MAR_set, weights = NULL)

   # Set value of denominator for Downside Deviation from the parameter denom set earlier
denomVal = nRow
if (denom != "full")
   { 
    denomVal = sum(ifelse((R.portfolio - MAR_set)<0,1,0))
   }
cat('\n Denominator parameter is set to: ',denom,' which results in a denominator value of ',denomVal,'\n')

   # PA computation of Downside Deviation
dwn = DownsideDeviation(R.portfolio, MAR = MAR_set, method = "full")

   # Hand computation of Downside Deviation
   # Lipper Difference is denom d.f.
dwnHand = sqrt(sum(apply(cbind((R.portfolio - MAR_set),rep(0,nRow)),1,min)^2)/(denomVal-1))

   # Output computation of Downside Deviation
cat('\n PA computed ',PortName,' Downside Deviation = ',dwn,'\n')
cat('\n Hand computed ',PortName,' Downside Deviation = ',dwnHand,'\n')

   # Hand computation of Sortino Ratio Lipper 
srHand = sqrt(freQ)*ARR/dwn

   # Hand computation PA Formula
srHand2 = mean(R.portfolio-MAR_set)/dwn

   # Output computation of Sortino Ratio
cat('\n PA computed ',PortName,'  Sortino Ratio = ',sr,'\n')
cat('\n Hand computed ',PortName,' Annualized Lipper Sortino Ratio = ',srHand,'\n')
cat('\n Hand computed ',PortName,' PA formula Sortino Ratio = ',srHand2,'\n')

Measures = rbind(Measures,c("Downside Deviation",dwn,"",""))
Measures = rbind(Measures,c("Denominator DD",denomVal,"",""))
Measures = rbind(Measures,c("Sortino Ratio",sr,"",""))
Measures.few = cbind(Measures[,1],substr(Measures[,2:4],1,8))
Measures.few = rbind(Measures[1,],Measures.few[-1,])

Measures.df= as.data.frame(Measures.few,stringsAsFactors=FALSE)

print(Measures.df)
