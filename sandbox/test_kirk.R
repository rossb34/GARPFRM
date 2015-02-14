# Dynamic Conditional Cor
# UV N~GARCH(1,1) for each series

install.packages("rmgarch")

library(rmgarch)

garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

# DCC specification: GARCH(1,1) for conditional cor
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), dccOrder = c(1,1),  distribution = "mvnorm")


dcc.garch11.spec

# dccspec()
# standardGeneric for "dccspec" defined from package "rmgarch"
# 
# function (uspec, VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
#           lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, 
#           robust.control = list(gamma = 0.25, delta = 0.01, nc = 10, 
#                                 ns = 500), dccOrder = c(1, 1), model = c("DCC", "aDCC", 
#                                                                          "FDCC"), groups = rep(1, length(uspec@spec)), distribution = c("mvnorm", 
#                                                                                                                                         "mvt", "mvlaplace"), start.pars = list(), fixed.pars = list()) 
#   standardGeneric("dccspec")
# <environment: 0x26889470>
#   Methods may be defined for arguments: uspec, VAR, robust, lag, lag.max, lag.criterion, external.regressors, robust.control, dccOrder, model, groups, distribution, start.pars, fixed.pars
# Use  showMethods("dccspec")  for currently available ones.





