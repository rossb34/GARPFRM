library(GARPFRM)

# Load weekly returns datasets
data(crsp_weekly)

# Use the MSFT (Microsoft) returns for univariate GARCH modeling
R <- largecap_weekly[,"MSFT"]

# Specify and fit the MSFT returns to a standard ARMA(0,0)-GARCH(1,1) model
model0 <- uvGARCH(R, armaOrder=c(0,0))

# The uvGARCH function uses the excellent rugarch package, which has a rich
# set of functions for analysis of fitted GARCH models. The fitted model can 
# be extracted with the getFit function. Refer to help("uGARCHfit-class")
# for available all methods for the uGARCHfit object that is returned by getFit.
# Here we can extract the GARCH model specification and fit
spec <- getSpec(model0)
spec

fit <- getFit(model0)
fit

slotNames(fit)
names(fit@fit)

# Here are a few functions to extract parameter estimates
coef(fit)
head(fitted(fit))
head(residuals(fit))
head(sigma(fit))

# 12 different plots are available. The user will be prompted to make a plot 
# or can specify a plot with the which argument
plot(fit)
plot(fit, which=1)

# Forecast 10 periods ahead using the standard ARMA(0,0)-GARCH(1,1) model
forecast1 <- forecast(model0, nAhead=10)
forecast1
plot(forecast1)

# Note that to do rolling forecasts, the outSample argument in uvGARCH must
# be greater than or equal to the nRoll argument specified in the forecast
# function.

# Here we specify the model with outSample=100 so that we can keep the last 100
# data points available for out of sample testing and can call the forecast
# method with the nRoll argument specified.
model11 <- uvGARCH(R, outSample=100)
forecast2 <- forecast(model11, nRoll=10)
plot(forecast2)
plot(forecast2, which=2)

# Several distributions are available for the innovations. Distributions include:
# "norm": normal distibution
# "snorm": skew-normal distribution
# "std": student-t distribution
# "sstd": skew-student distribution
# "ged": generalized error distribution
# "sged": skew-generalized error distribution
# "nig": normal inverse gaussian distribution
# "ghyp": Generalized Hyperbolic distribution
# "jsu": Johnson's SU distribution.

# Here we specify and fit the MSFT returns to a standard ARMA(0,0)-GARCH(1,1)
# model with student-t innovations
model0.std <- uvGARCH(R, armaOrder=c(0,0), distribution="std")

# The default arguments for uvGARCH are to specify and fit a standard 
# ARMA(1,1)-GARCH(1,1)model
model11 <- uvGARCH(R)

# In addition to specifyin the model with different ar and ma orders, the 
# ARCH(q) and GARCH(p) orders can also be specified. Here we fit a standard
# ARMA(1,1)-GARCH(2,1) model
model21 <- uvGARCH(R, garchOrder=c(2,1))
getSpec(model21)
getFit(model21)

