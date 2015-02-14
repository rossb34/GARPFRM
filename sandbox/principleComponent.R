library(GARPFRM)
library(psych)
library(GPArotation)

# Applying duration as a hedge
# #1: Initialize the Discount Factors (DF)
DF_1 = rbind(0.968,0.9407242,0.9031545,0.8739803)
# Choose a 2 year bond with semiannual payments to match number of bond prices and CFs
time = seq(from=0.5, to=2, by=0.5)
# First define a bond object to be used throughout the analysis, where m is the compound frequency
bond_1 = bondSpec(time, face=100, m=2, couponRate = 0.0475)
# Duration measures the effect of a small parallel shift in the yield curve
mDuration_1 = bondDuration(bond_1,DF_1)
# # 2: Initialize the Discount Factors (DF)
DF_2 = rbind(0.95434,0.9434,0.917232,0.89,0.85678,0.8396,0.81242,0.7921,0.7693,0.7473,0.7298,0.7050)
# Choose a 2 year bond with semiannual payments to match number of bond prices and CFs
time = seq(from=0.5, to=6, by=0.5)
# First define a bond object to be used throughout the analysis, where m is the compound frequency
bond_2 = bondSpec(time, face=100, m=2, couponRate = 0.0475)
# Duration measures the effect of a small parallel shift in the yield curve
mDuration_2 = bondDuration(bond_2,DF_2)
# Hedging Ratio: for every bond_2 used there needs to be hedgeRatio amount of bond_1s
# If you hold the portfolio bond_2, you want to sell hedgeRatio units of bonds
hedgeRatio = - mDuration_2/mDuration_1


# Load Data for historcal analysis tools: load the 2, 5,10. and 30 year
data(treasuryts)
data = treasuryts
head(data)


# Empirical application: Linear hedge estimation 
# OLS Level-on-Level regression 
deltas = linearHedge(data[,1],data[,2:4])
# Insert the normalized hedged contract versus hedgeable contract value
deltas = c(1,deltas)

# In sample illustration: random, mean reverting spreads
hedgedInstruments = data%*%deltas
plot(hedgedInstruments, type="l", main = "Hedged Price Difference: Level", xlab="Time",ylab="Difference")

# OLS Change-on-Change regression 
deltas = linearHedge(diff(data[,1]),diff(data[,2:4]))
# Insert the normalized hedged contract versus hedgeable contract value
deltas = c(1,deltas)

# In sample illustration: random, mean reverting spreads
hedgedInstruments = data%*%deltas
plot(hedgedInstruments, type="l", main = "Hedged Price Difference: Change", xlab="Time",ylab="Difference")


# Have a single, empirical description of the behavior of the term structure that can be applied across all
# assets. Principal Compnents (PCs) provide such an emperical description 
# Retain components that combined account for x% of the cumulative variance
pca = PCA(data, nfactors = 3, rotate="none")
summary(pca)

# Retrieve Loadings and if loading is insignificant then omit
getLoadings(pca)

# Retrieve Weights
getWeights(pca)

## Structural Equation Modelling
# Determining the appropriate number of factors
# A graphic representation of the 3 oblique factors
fa.diagram(pca)

# Alternative to determining the number of factors to compare the solution 
# to random data with the same properties as the real data set.
fa.parallel(data)

# Plot up to the first three factors
plot(pca)
pca = PCA(data, nfactors = 2, rotate="none")
plot(pca)

# Creating factor scores: Linear composite of the weighted observed variables
  # Determine weights
  # Multiply variable for each observation by these weights
  # Sum the products
pca.r = principal(data, nfactors=2, rotate="varimax", scores=T)
scores = pca.r$scores
plot(pca$scores[,1],pca$scores[,2], xlab="PCA1", ylab="PCA2", main = "Scores: Observable Pattern")