library(GARPFRM)

# Colonel Motors expected return
R_C <- 0.14

# Colonel Motors standard deviation
sigma_C <- 0.06

# Separated Edison expected return
R_S <- 0.08

# Separated Edison standard deviation
sigma_S <- 0.03

# Correlation coefficient
rho <- 1

# Create a vector of portfolio weights
X_C <- seq(from=0, to=1, by=0.2)

# Calculate the portfolio expected return (rho = -1)
R_P <- portReturnTwoAsset(R_C, R_S, X_C)

# Calculate the portfolio standard deviation (rho = 0)
sigma_P <- portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho)

# Combine the portfolio returns and standard deviations in a data.frame object.
# Note that this is the transpose t() of the data.frame object to match the
# layout of the tables in the FRM book.
df <- t(data.frame(R_P=R_P, sigma_P=sigma_P))
colnames(df) <- X_C
df

# Plot the portfolio return and standard deviation to 
# understand the risk and return of the portfolio
# Create and plot the efficient frontier object
ef <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho)
plot(ef)

# Correlation coefficient
rho <- -1

# Calculate the portfolio expected return
R_P <- portReturnTwoAsset(R_C, R_S, X_C)

# Calculate the portfolio standard deviation
sigma_P <- portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho)

# Combine the portfolio returns and standard deviations in a data.frame object.
# Note that this is the transpose t() of the data.frame object to match the
# layout of the tables in the FRM book.
df <- t(data.frame(R_P=R_P, sigma_P=sigma_P))
colnames(df) <- X_C
df

# Create and plot the efficient frontier object
ef <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho)
plot(ef)

# Correlation coefficient
rho <- 0

# Calculate the portfolio expected return
R_P <- portReturnTwoAsset(R_C, R_S, X_C)

# Calculate the portfolio standard deviation
sigma_P <- portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho)

# Combine the portfolio returns and standard deviations in a data.frame object.
# Note that this is the transpose t() of the data.frame object to match the
# layout of the tables in the FRM book.
df <- t(data.frame(R_P=R_P, sigma_P=sigma_P))
colnames(df) <- X_C
df

# Create and plot the efficient frontier object
ef <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho)
plot(ef)

minRisk <- function(R_A, R_B, sigma_A, sigma_B, rho){
  top_term <- sigma_B^2 - sigma_A * sigma_B * rho
  bottom_term <- sigma_A^2 + sigma_B^2 - 2 * sigma_A * sigma_B * rho
  # x is the fraction of asset A 
  x <- top_term / bottom_term
  # calculate the weights for the minimum risk portfolio
  weights <- c(x, 1-x)
  names(weights) <- c("Asset_A", "Asset_B")
  # calculate the portfolio return and standard deviation of the minimum 
  # risk portfolio
  port_ret <- portReturnTwoAsset(R_A, R_B, x)
  port_sd <- portSDTwoAsset(R_A, R_B, x, sigma_A, sigma_B, rho)
  return(list(weights=weights, 
              portfolio_return=port_ret, 
              portfolio_sd=port_sd))
}

minRisk(R_C, R_S, sigma_C, sigma_S, rho)

# Correlation coefficient (rho = 0.5)
rho <- 0.5

# Calculate the portfolio expected return
R_P <- portReturnTwoAsset(R_C, R_S, X_C)

# Calculate the portfolio standard deviation
sigma_P <- portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho)

# Combine the portfolio returns and standard deviations in a data.frame object.
# Note that this is the transpose t() of the data.frame object to match the
# layout of the tables in the FRM book.
df <- t(data.frame(R_P=R_P, sigma_P=sigma_P))
colnames(df) <- X_C
df

# Create and plot the efficient frontier object
ef <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho)
plot(ef)

minRisk(R_C, R_S, sigma_C, sigma_S, rho)

X_C <- seq(from=0, to=1, by=0.01)
R_P <- portReturnTwoAsset(R_C, R_S, X_C)
# rho = 1
plot(portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho=1), R_P, 
     main="Portfolio Return and Standard Deviation", 
     type="l", xlim=c(0, 0.08), ylim=c(0, 0.18),
     xlab="Portfolio Standard Deviation",
     ylab="Portfolio Expected Return", cex.lab=0.8)
# rho = -1
lines(portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho=-1), R_P, col="blue")
# rho = 0
lines(portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho=0), R_P, col="red")
# rho = 0.5
lines(portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho=0.5), R_P, col="green")
legend("topleft", legend=c(legend=expression(paste(rho, " = 1")),
                           legend=expression(paste(rho, " = -1")),
                           legend=expression(paste(rho, " = 0")),
                           legend=expression(paste(rho, " = 0.5"))), 
       col=c("black", "blue", "red", "green"), 
       lty=rep(1, 4), bty="n")


# The Efficient Frontier with Short Sales Allowed
# correlation coefficient
rho <- 0.5

# Fraction of the portfolio invested in Colonel Motors
X_C <- seq(from=-1, to=2, by=0.2)

# Calculate the portfolio expected return
R_P <- portReturnTwoAsset(R_C, R_S, X_C)

# Calculate the portfolio standard deviation
sigma_P <- portSDTwoAsset(R_C, R_S, X_C, sigma_C, sigma_S, rho)

# Combine the portfolio returns and standard deviations in a data.frame object.
# Note that this is the transpose t() of the data.frame object to match the
# layout of the tables in the FRM book.
df <- t(data.frame(R_P=R_P, sigma_P=sigma_P))
colnames(df) <- X_C
print(df, digits=4)

# Create and plot the efficient frontier object
ef_short <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho, 
                                      allowShorting=TRUE)
ef_long <- efficientFrontierTwoAsset(R_C, R_S, sigma_C, sigma_S, rho)
plot(ef_short)
lines(ef_long$efficient_frontier[,2], ef_long$efficient_frontier[,1], 
      lty=2, lwd=2, col="blue")



# Estimated inputs for equity
R_SP <- 0.125
sigma_SP <- 0.149

# Estimated inputs for bonds
R_B <- 0.06
sigma_B <- 0.048

# Estimated correlation between equity and bonds
rho <- 0.45

# Calculate the allocation and values for the minimum variance portfolio
minRisk(R_SP, R_B, sigma_SP, sigma_B, rho)

ef <- efficientFrontierTwoAsset(R_SP, R_B, sigma_SP, sigma_B, rho,
                                rf=0.05, allowShorting=TRUE)
plot(ef)

# Estimated inputs for domestic portfolio
R_SP <- 0.125
sigma_SP <- 0.149

# Estimated inputs for international portfolio
R_int <- 0.105
sigma_int <- 0.14

# Estimated correlation between domestic and international portfolio
rho <- 0.33

# Calculate the allocation and values for the minimum variance portfolio
minRisk(R_SP, R_int, sigma_SP, sigma_int, rho)

ef <- efficientFrontierTwoAsset(R_SP, R_int, sigma_SP, sigma_int, rho, rf=0.05)
plot(ef)

# Extended Examples for Portfolios with Multiple Assets
data(crsp_weekly)
R_large <- largecap_weekly[,1:5]
R_mid <- midcap_weekly[,1:5]
R_small <- smallcap_weekly[,1:5]

# Create and plot an efficient frontier of large cap stocks
ef_large <- efficientFrontier(R_large)
plot(ef_large, main="Large Cap Efficient Frontier", cexAssets=0.6)

# Create an efficient frontier object with box constraints
# Add box constraints such that each asset must have a weight greater than 15%
# and less than 55%
ef_large_box <- efficientFrontier(R_large, minBox=0.15, maxBox=0.55)
plot(ef_large_box, main="Large Cap Efficient Frontier\nwith Box Constraints",
     cexAssets=0.6)

# Combine the large cap, mid cap, and small cap stocks
R <- cbind(R_large, R_mid, R_small)

# Specify the list of groups such that each market cap is in its own group
groups <- list(c(1:5), c(6:10), c(11:15))

# Specify the sum of weights for each group:
# largecap: no more than 60% and no less than 30%
# midcap: no more than 50% and no less than 20%
# smallcap: no more than 40% and no less than 15%
groupMin <- c(0.3, 0.2, 0.15)
groupMax <- c(0.6, 0.5, 0.4)

# Create the efficient frontier with the group constraints
efGroup <- efficientFrontier(R, groupList=groups, 
                             groupMin=groupMin, 
                             groupMax=groupMax)
plot(efGroup, labelAssets=FALSE)
