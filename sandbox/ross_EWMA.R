
##### testing #####
library(GARPFRM)

# data and parameters for EWMA estimate
data(crsp_weekly)
R <- largecap_weekly[, 1:2]
mvR <- largecap_weekly[,1:4]
lambda <- 0.94
initialWindow <- 150

# volatility estimate of univariate data
lambda <- estimateLambdaVol(R[,1], initialWindow, n=10)
vol1 <- EWMA(R[,1], lambda=NULL, initialWindow, n=10, "volatility")
vol1a <- EWMA(R[,1], lambda, initialWindow, n=10, "volatility")
all.equal(vol1$estimate, vol1a$estimate)
vol1
plot(vol1)

# Calculate realized volatility
realizedVolatility <- realizedVol(R[,1], 10)

# covariance estimate of bivariate data
lambda <- estimateLambdaCov(R, initialWindow, n=10)
cov1 <- EWMA(R, lambda=NULL, initialWindow, n=10, "covariance")
cov1a <- EWMA(R, lambda, initialWindow, n=10, "covariance")
all.equal(cov1$estimate, cov1a$estimate)
cov1
plot(cov1)

# Calculate realized covariance
realizedCov <- realizedCov(R, 10)

# correlation estimate of bivariate data
lambda <- estimateLambdaCor(R, initialWindow, n=10)
cor1 <- EWMA(R, lambda=NULL, initialWindow, n=10, "correlation")
cor1a <- EWMA(R, lambda, initialWindow, n=10, "correlation")
all.equal(cor1$estimate, cor1a$estimate)
cor1
plot(cor1)

# Calculate realized correlation
realizedCorrelation <- realizedCor(R, 10)

# Multivariate EWMA estimate of covariance
lambda <- 0.94
cov_mv <- EWMA(mvR, lambda, initialWindow, type="covariance")
cov_mv
# Extract the estimated covariance between ORCL and MSFT
tail(getCov(cov_mv, assets=c("ORCL", "MSFT")))
# These two are equivalent
plot(cov_mv, assets=c("ORCL", "MSFT"))
plot(cov_mv, assets=c(1, 2))

# Multivariate EWMA estimate of correlation
cor_mv <- EWMA(mvR, lambda, initialWindow, type="correlation")

# Extract the estimated correlation between ORCL and EMC
tail(getCor(cor_mv, assets=c("ORCL", "EMC")))
cor_mv

# These two are equivalent
plot(cor_mv, assets=c("ORCL", "EMC"))
plot(cor_mv, assets=c(1, 4))


