library(GARPFRM)
data(crsp.short)
R <- largecap.ts[, 1:4]

# Univariate CAPM
# Run CAPM regression
# Fiting CAPM
object = CAPM(R[,3], R[,4])

# Retrieve alpha
getAlphas(object)
# Retrieve beta
getBetas(object)
# Retrieve statistics
getStatistics(object)
# Run Hypothesis test where default is CI: 5%
# For alpha = 0 and beta = 1 two-tail test
hypTest(object)
#Run hypothesis test for CI: 10%
CI = 0.1
hypTest(object,CI)

# Plot data with regression line: with coefficients, and tstat specified on graph
plot(object)


# Multiple Linear Model CAPM
# Run CAPM regression
# Fiting CAPM
object = CAPM(R[,1:3], R[,4])

# Plot security market line
chartSML(object)
par(mfrow=c(1,1))

# Retrieve alpha
getAlphas(object)
# Retrieve beta
getBetas(object)
# Retrieve statistics
getStatistics(object)
# Run Hypothesis test where default is CI: 5%
# For alpha = 0 and beta = 1 two-tail test
hypTest(object)
#Run hypothesis test for CI: 10%
CI = 0.1
hypTest(object,CI)

# Plot data with regression line: with coefficients, and tstat specified on graph
plot(object)
par(mfrow=c(1,1))