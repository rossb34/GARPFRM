# Monte Carlo demo

library(GARPFRM)

# Simulate 500 asset price paths
mc <- monteCarlo(0.05, 0.25, 500, 1, 52, 10)

# plot the simulated asset paths from the monte carlo simulation
plot(mc)

# get the ending prices
ending_prices <- endingPrices(mc)

# plot the ending prices
plotEndingPrices(mc)

summary(ending_prices)
quantile(ending_prices, c(0.05, 0.95))

# Add examples of pricing options
# european
# path-dependent like Asian or barrier options
