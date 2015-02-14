library(GARPFRM)
data(crsp.short)

head(largecap.ts)

Rf <- largecap.ts[, "t90"]
R <- largecap.ts[, "CAT"] - Rf
MKT <- largecap.ts[, "market"] - Rf

# Fit the CAPM model
tmp <- CAPM(R=R, Rmkt=MKT)

class(tmp)

plot(tmp)


tmp$raw_data

coef(tmp)
summary(tmp)



plot(tmp)

# Internally, R is calling print.lm and summary.lm because tmp is of class lm
# You can define your own print.capm_uv and summary.capm_uv for control over
# what values are calculated and the output that is displayed.
print(tmp)
summary(tmp)

# Demonstrate extractor functions
getAlphas(tmp)
getBetas(tmp)
getStatistics(tmp)

# multivariate CAPM
R <- largecap.ts[, c("CAT", "DD")] - cbind(Rf, Rf)
# Use the MKT object that was created earlier


mv <- CAPM(R, MKT)
mv
coef(mv)
mv.s <- summary(mv)
mv.s

coef(mv.s)

