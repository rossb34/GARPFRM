

suppressMessages(library(GARPFRM))
suppressMessages(library(lattice))
suppressMessages(library(pcaPP))
suppressMessages(library(hexbin))
data(crsp.short)

# Use the first 6 stocks in largecap.ts
crsp.returns <- largecap.ts[, 1:6]

# Get the names of the stocks and view the first 5 rows of crsp.returns
colnames(crsp.returns)
head(crsp.returns, 5)



xyplot(crsp.returns, scale=list(y="same"), main="Monthly Returns")



boxplot(coredata(crsp.returns), pch=20, main="Monthly Returns")



# Extract the column labeled "market" to get the market returns
MKT.ret <- largecap.ts[, "market"]



plot(MKT.ret, main="Market Monthly Returns")



hist(MKT.ret, probability=TRUE, main="Histogram of Market Returns", 
     col="lightblue", ylim=c(0, 10))
lines(density(MKT.ret), lty=2)
rug(MKT.ret)
legend("topleft", legend="kernel density estimate", lty=2,
       cex=0.8, bty="n")



# Plot the kernel density estimate of market returns
plot(density(MKT.ret), main="Density of Market Returns")
rug(MKT.ret)
# sample estimates
curve(dnorm(x, mean=mean(MKT.ret), sd=sd(MKT.ret)), 
      add=TRUE, col="red", lty=2, lwd=2)
# robust estimates
curve(dnorm(x, mean=median(MKT.ret), sd=mad(MKT.ret)), 
      add=TRUE, col="blue", lty=2, lwd=2)
legend("topleft", legend=c("kernel density estimate", "normal density estimate", 
                           "robust normal density estimate"), 
       col=c("black", "red", "blue"), lty=c(1, 2, 2), bty="n", cex=0.8)



chart.QQPlot(MKT.ret, envelope=0.95, pch=18, main="Market Returns QQ Plot",
             xlab="Theoretical Normal Quantiles")
legend("topleft", legend=c("Quartile-Pairs Line", "95% Confidence Envelope"), 
       col=c("blue", "blue"), lty=c(1, 2), cex=0.8, bty="n")



shapiro.test(coredata(MKT.ret))



# Sample mean of SPY return
mean(MKT.ret)

# Sample Variance of SPY returns
var(MKT.ret)

# Sample standard deviation of SPY returns
sd(MKT.ret)

# Standard error of SPY returns
sd(MKT.ret) / sqrt(nrow(MKT.ret))

# Sample skewness of SPY returns.
# See ?skewness for additional methods for calculating skewness
skewness(MKT.ret, method="sample")

# Sample kurtosis of SPY returns.
# See ?kurtosis for additional methods for calculating kurtosis
kurtosis(MKT.ret, method="sample")

# Summary statistics of SPY returns
summary(MKT.ret)

# Sample quantiles of SPY returns
quantile(MKT.ret, probs=c(0, 0.25, 0.5, 0.75, 1))



pairs(coredata(crsp.returns), pch=20, col=rgb(0,0,100,50,maxColorValue=255))
hexplom(coredata(crsp.returns), varname.cex=0.75)



# Sample correlation of returns
cor(crsp.returns)



suppressWarnings(plotcov(cor(crsp.returns), 
                         method1="Sample Correlation Estimate"))



# Sample covariance of returns
cov(crsp.returns)



suppressWarnings(plotcov(cov(crsp.returns), 
                         method1="Sample Covariance Estimate"))



curve(dnorm(x), from=-4, to=4, main="Standard Normal pdf")



pnorm(q=2, mean=1, sd=2)
# Normalize as is done in the book
pnorm(q=0.5)



qnorm(p=0.975)



# Set the seed for reproducible results
set.seed(123)
rnorm(n=10, mean=0.0015, sd=0.025)



t.test(x=MKT.ret, alternative="two.sided", mu=0)



# Replicate the results of t.test using the method outlined in the book
t_stat <- (mean(MKT.ret) - 0) / (sd(MKT.ret) / sqrt(nrow(MKT.ret)))
p_value <- 2 * pt(q=-abs(t_stat), df=nrow(MKT.ret))
df <- nrow(MKT.ret) - 1
ci <- mean(MKT.ret) + c(-1, 1) * 1.96 * sd(MKT.ret) / sqrt(nrow(MKT.ret))
paste("t = ", round(t_stat, 4), ", df = ", df, ", p-value = ", 
      round(p_value, 4), sep="")
print("95% Confidence Interval")
print(ci)



CAT.ret <- crsp.returns[, "CAT"] - largecap.ts[, "t90"]
MKT.ret <- largecap.ts[, "market"] - largecap.ts[, "t90"]

# Fitting linear models works with xts objects, but works better with data.frame objects. This is especially true with the predict method for linear models.
ret.data <- as.data.frame(cbind(CAT.ret, MKT.ret))



plot(coredata(MKT.ret), coredata(CAT.ret), pch=19,
     col=rgb(0,0,100,50,maxColorValue=255),
     xlab="Market returns", ylab="CAT returns",
     main="CAT vs. Market Excess Returns")



model.fit <- lm(CAT ~ market, data=ret.data)



# The print method displays the call and the coefficients of the linear model
model.fit

# The summary method displays additional information for the linear model
model.summary <- summary(model.fit)
model.summary



# Note that some are commented out

# Coefficients
coef(model.fit)
# Extract the fitted values
head(fitted(model.fit))
# Extract the residuals
head(resid(model.fit))
# Exctract the standardized residuals
head(rstandard(model.fit))



# Coefficients
model.coef <- coef(model.summary)
model.coef

# Sigma
model.summary$sigma
# R squared
model.summary$r.squared
# Adjusted R squared
model.summary$adj.r.squared



ESS <- sum((fitted(model.fit) - mean(CAT.ret))^2)



TSS <- sum((CAT.ret - mean(CAT.ret))^2)



SSR <- sum(resid(model.fit)^2)



r_squared <- model.summary$r.squared
r_squared



ESS / TSS
1 - SSR / TSS



n <- nrow(CAT.ret)
SER <- sqrt(SSR / (n - 2))
SER



plot(resid(model.fit), type="h")



new <- data.frame(market=seq(from=-0.2, to=0.2, length.out=nrow(ret.data)))
model.ci <- predict(object=model.fit, newdata=new, interval="confidence")
model.pi <- predict(object=model.fit, newdata=new, interval="prediction")



# Get the betas and the standard errors
alpha <- round(model.coef[1, 1], 6)
alpha.se <- round(model.coef[1, 2], 6)
beta <- round(model.coef[2, 1], 6)
beta.se <- round(model.coef[2, 2], 6)



plot(coredata(MKT.ret), coredata(CAT.ret),
     col=rgb(0,0,100,50,maxColorValue=255), pch=20,
     xlab="Market returns", ylab="CAT returns", xlim=c(-0.2, 0.2),
     main="lm(CAT ~ Market)")
abline(model.fit, col="black")
lines(x=model.ci[, "fit"], y=model.ci[, "upr"], col="blue", lty=1)
lines(x=model.ci[, "fit"], y=model.ci[, "lwr"], col="blue", lty=1)
legend("topleft", legend=c(paste("alpha = ", alpha, " (", alpha.se, ")", sep=""),
                           paste("beta = ", beta, " (", beta.se, ")", sep=""),
                           "Fitted Values", "95% Confidence Interval"), 
       col=c("black", "blue"), lty=c(0, 0, 1, 1), cex=0.8, bty="n")



plot(coredata(MKT.ret), coredata(CAT.ret),
     col=rgb(0,0,100,50,maxColorValue=255), pch=20,
     xlab="Market returns", ylab="CAT returns", xlim=c(-0.2, 0.2),
     main="lm(CAT ~ Market)")
abline(model.fit, col="black")
lines(x=model.pi[, "fit"], y=model.pi[, "upr"], col="red", lty=2)
lines(x=model.pi[, "fit"], y=model.pi[, "lwr"], col="red", lty=2)
legend("topleft", legend=c(paste("alpha = ", alpha, " (", alpha.se, ")", sep=""),
                           paste("beta = ", beta, " (", beta.se, ")", sep=""),
                           "Fitted Values", "95% Prediction Interval"), 
       col=c("black", "red"), lty=c(0, 0, 1, 2), cex=0.8, bty="n")



data(fama_french_factors)

# The first 3 columns are the factors, the 4th column is the risk free rate.
ff_factors <- fama_french_factors[, 1:3]
head(ff_factors, 5)



data(returns)
# Align the dates of the Fama-French Factors and the returns
returns <- returns['/2013-10-25']
AAPL.ret <- returns[, "AAPL"]

# AAPL excess returns
AAPL.e <- AAPL.ret - fama_french_factors[, "RF"] / 100



# Fit the model
ff.fit <- lm(AAPL.e ~ ff_factors)
ff.fit
summary(ff.fit)



# Omit the first column of returns because it is the SPY weekly returns, 
# which is a proxy for the market.
returns <- returns[, -1]

# Calculate the excess returns of all assets in the returns object
ret.e <- returns - (fama_french_factors[, "RF"] / 100) %*% rep(1, ncol(returns))



# Show the first 5 rows of ret.e
head(ret.e, 5)



ff.fit <- lm(ret.e ~ ff_factors)
# Display the coefficients of each model
ff.fit



beta0 <- coef(ff.fit)[1,]
beta1 <- coef(ff.fit)[2,]
beta2 <- coef(ff.fit)[3,]
beta3 <- coef(ff.fit)[4,]
rsq <- sapply(X=summary(ff.fit), FUN=function(x) x$r.squared)
names(rsq) <- colnames(ret.e)

par(mfrow=c(2,2))
barplot(beta1, main="Beta for Market-RF", col=bluemono, cex.names=0.8)
barplot(beta2, main="Beta for SMB", col=bluemono, cex.names=0.8)
barplot(beta3, main="Beta for HML", col=bluemono, cex.names=0.8)
barplot(rsq, main="R Squared Values", col=bluemono, cex.names=0.8)
par(mfrow=c(1,1))



summary(ff.fit)


