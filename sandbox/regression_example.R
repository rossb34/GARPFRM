# regression example

library(GARPFRM)

# The returns data includes weekly returns
data(returns)

# Extract the SPY returns from the returns object
SPY.ret <- returns[, "SPY"]

# Exploratory Data Analysis, Basic Probability, Basic Statistics

# Plot the SPY weekly returns
plot(SPY.ret, main="SPY Weekly Returns")

# Plot the density of SPY Weekly Returns
plot(density(SPY.ret), main="Density of SPY Weekly Returns")
rug(SPY.ret)
# sample estimates
curve(dnorm(x, mean=mean(SPY.ret), sd=sd(SPY.ret)), 
      add=TRUE, col="red", lty=2, lwd=2)
# robust estimates
curve(dnorm(x, mean=median(SPY.ret), sd=mad(SPY.ret)), 
      add=TRUE, col="blue", lty=2, lwd=2)
legend("topleft", legend=c("estimated density", "normal density", "robust normal density"), 
       col=c("black", "red", "blue"), lty=c(1, 2, 2), bty="n", cex=0.8)

# Quantile-Quantile Plot of SPY Weekly Returns
qqnorm(SPY.ret)
qqline(SPY.ret)

# Shapiro-Wilk Normality test
# Null hypothesis that a sample came from a normally distributed population
shapiro.test(coredata(SPY.ret))

# Sample mean of SPY return
mean(SPY.ret)

# Sample Variance of SPY returns
var(SPY.ret)

# Sample standard deviation of SPY returns
sd(SPY.ret)

# Standard error of SPY returns
sd(SPY.ret) / sqrt(nrow(SPY.ret))

# Sample skewness of SPY returns
# See ?skewness for additional methods for calculating skewness
skewness(SPY.ret, method="sample")

# Sample kurtosis of SPY returns
# See ?kurtosis for additional methods for calculating kurtosis
kurtosis(SPY.ret, method="sample")

# Summary statistics of SPY returns
summary(SPY.ret)

# Sample quantiles of SPY returns
quantile(SPY.ret, probs=c(0, 0.25, 0.5, 0.75, 1))

# Scatter plot of each pair of assets in the returns dataset
pairs(coredata(returns), pch=18, col=rgb(0,0,100,50,maxColorValue=255))
lattice::splom(coredata(returns), pch=18, col=rgb(0,0,100,50,maxColorValue=255))

# Sample correlation of returns
cor(returns)

# Sample covariance of returns
cov(returns)

# Distributions
# R has functions to compute the density, distribution function, quantile, and 
# random number generation for several distributions.

# Normal Distribution
# dnorm, pnorm, qnorm, rnorm

# Chi-Squared Distribution
# dchisq, pchisq, qchisq, rchisq

# Student t Distribution
# dt, pt, qt, rt

# F Distribution
# df, pf, qf, rf

# These functions will be demonstrated for the normal distribution.

# Use dnorm to plot the pdf of a standard normal distribution
curve(dnorm(x), from=-4, to=4, main="Standard Normal pdf")

# Calculate the probability that Y <= 2 when Y is distributed N(1, 4) with mean of 1 and variance of 4
pnorm(q=2, mean=1, sd=2)
pnorm(q=0.5)

# Quantile function of a standard normal at probability 0.975
qnorm(p=0.975)

# Generate 10 random numbers from a normal distribution with mean 0.0015 and standard deviation 0.025
rnorm(n=10, mean=0.0015, sd=0.025)

# Hypothesis testing
# The null hypothesis is that the true mean return of SPY is equal to 0
t.test(x=SPY.ret, alternative="two.sided", mu=0)

# Replicate the results of t.test using the method outlined in the book
t_stat <- (mean(SPY.ret) - 0) / (sd(SPY.ret) / sqrt(nrow(SPY.ret)))
p_value <- 2 * pt(q=-abs(t_stat), df=462)
df <- nrow(SPY.ret) - 1
ci <- mean(SPY.ret) + c(-1, 1) * 1.96 * sd(SPY.ret) / sqrt(nrow(SPY.ret))
paste("t = ", round(t_stat, 4), ", df = ", df, ", p-value = ", round(p_value, 4), sep="")
print("95% Confidence Interval")
print(ci)

##### Regression #####
# Signle Regressor
# Extract the weekly returns of AAPL and SPY from the returns object
AAPL.ret <- returns[, "AAPL"]
SPY.ret <- returns[, "SPY"]

# Plot the AAPL vs. SPY returns
plot(x=coredata(SPY.ret), y=coredata(AAPL.ret), 
     xlab="SPY returns", ylab="AAPL returns")

# Fit the linear regression model
model.fit <- lm(AAPL.ret ~ SPY.ret)

# The print method displays the call and the coefficients of the model
print(model.fit)

# Accessor methods for the lm object
coef(model.fit)
fitted(model.fit)
resid(model.fit)
rstandard(model.fit)

# The summary method displays additional information for the linear model
model.summary <- summary(model.fit)

print(model.summary)

coef(model.summary)
model.summary$sigma
model.summary$r.squared
model.summary$adj.r.squared

# Predict method
predict(object=model.fit, newdata=data.frame(SPY.ret=c(-0.1, 0, 0.1)))
model.ci <- predict(object=model.fit, interval="confidence")
model.pi <- predict(object=model.fit, interval="prediction")

# Plot AAPL vs SPY returns
plot(x=coredata(SPY.ret), y=coredata(AAPL.ret), 
     xlab="SPY returns", ylab="AAPL returns")
abline(model.fit, col="red")
lines(x=coredata(SPY.ret), y=model.ci[, "upr"], col="blue", lty=1)
lines(x=coredata(SPY.ret), y=model.ci[, "lwr"], col="blue", lty=1)
lines(x=coredata(SPY.ret), y=model.pi[, "upr"], col="red", lty=2)
lines(x=coredata(SPY.ret), y=model.pi[, "lwr"], col="red", lty=2)

# Plot the residuals of the linear model
plot(resid(model.fit), type="h")

# Multiple Regressors
# Fama French Factor Model
data(returns)
data(fama_french_factors)

# Fit the factors and AAPL returns
# AAPL returns can be explained by the Fama French 3-Factor model 
# AAPL excess returns

ff_factors <- fama_french_factors[, 1:3]

# Align the dates of the Fama-French Factors and the returns
returns <- returns['/2013-10-25']
# Omit the first column of returns because it is the SPY weekly returns, which is
# a proxy for the market.
returns <- returns[, -1]
AAPL.ret <- returns[, "AAPL"]

# AAPL excess returns
AAPL.e <- AAPL.ret - fama_french_factors[, "RF"] / 100

# Fit the model
ff.fit <- lm(AAPL.e ~ ff_factors)
print(ff.fit)
summary(ff.fit)

# Fit the Fama-French 3 Factor Model to all the assets in the returns object
# Calculate the excess returns of all assets in the returns object
ret.e <- returns - (fama_french_factors[, "RF"] / 100) %*% rep(1, ncol(returns))
ff.fit <- lm(ret.e ~ ff_factors)
print(ff.fit)
print(summary(ff.fit))


beta0 <- coef(ff.fit)[1,]
beta1 <- coef(ff.fit)[2,]
beta2 <- coef(ff.fit)[3,]
beta3 <- coef(ff.fit)[4,]
rsq <- sapply(X=summary(ff.fit), FUN=function(x) x$r.squared)
names(rsq) <- colnames(ret.e)

par(mfrow=c(2,2))
barplot(beta1, main="Beta for Market-RF", col=c(2:6))
barplot(beta2, main="Beta for SMB", col=c(2:6))
barplot(beta3, main="Beta for HML", col=c(2:6))
barplot(rsq, main="R Squared Values", col=c(2:6))
