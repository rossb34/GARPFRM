

# American call
am.call <- optionSpec(style="american", type="call")
am.call.val <- optionValue(am.call, N=4)

# American put
am.put <- optionSpec(style="american", type="put")
am.put.val <- optionValue(am.put)

# European call
euro.call <- optionSpec(style="european", type="call", S0=30, K=30, 
                        maturity=1, r=0.05, volatility=0.25, q=0)
euro.call.val.bs <- optionValue(euro.call, method="Black-Scholes")
euro.call.val.bin <- optionValue(euro.call, method="Binomial", N=100)

# European put
euro.put <- optionSpec(style="european", type="put")
euro.put.val.bs <- optionValue(euro.put, method="Black-Scholes") 
euro.put.val.bin <- optionValue(euro.put, method="Binomial") 

# European call greeks
computeGreeks(euro.call, greek = "delta")
computeGreeks(euro.call, greek = "gamma")
computeGreeks(euro.call, greek = "theta")
computeGreeks(euro.call, greek = "vega")
computeGreeks(euro.call, greek = "rho")

# delta
computeGreeks(euro.call, "delta", prices = seq(20, 40, 1), plot = TRUE)
computeGreeks(euro.call, "delta", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)
computeGreeks(euro.call, prices = seq(20, 40, 1), maturities = seq(0.5, 0.01, -0.01), plot = TRUE)

# theta
computeGreeks(euro.call, "theta", prices = seq(20, 40, 1), plot = TRUE)
computeGreeks(euro.call, "theta", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)

# gamma
computeGreeks(euro.call, "gamma", prices = seq(20, 40, 1), plot = TRUE)
computeGreeks(euro.call, "gamma", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)

# vega
computeGreeks(euro.call, "vega", prices = seq(20, 40, 1), plot = TRUE)
computeGreeks(euro.call, "vega", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)

# rho
computeGreeks(euro.call, "rho", prices = seq(20, 40, 1), plot = TRUE)
computeGreeks(euro.call, "rho", maturities = seq(0.5, 0.01, -0.01), plot = TRUE)





