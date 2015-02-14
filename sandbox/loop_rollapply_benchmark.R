library(xts)

# benchmark a rolling covariance using a for loop vs rollapply

foo <- function(x, width, na.pad=FALSE){
  n <- nrow(R)
  out <- vector("numeric", n-width+1)
  for(i in width:n){
    #cat("from = ", i-width+1, "\n")
    #cat("to = ", i, "\n")
    tmpR <- R[(i-width+1):i,]
    out[(i-width+1)] <- cov(tmpR[,1], tmpR[,2])
  }
  if(na.pad) {
    vecNA <- rep(NA, width-1)
    out <- c(vecNA, out)
  }
  out
}

rollfoo <- function(x, width){
  rollapply(R, width=width, FUN=function(x) cov(x=x[,1], y=x[,2]), by.column=FALSE)
}

# set.seed(123)
# x <- rnorm(15, 0, 0.1)
# y <- rnorm(15, 0, 0.15)
# R <- cbind(x, y)
# 
# cov(R[1:5,])[1,2]
# cov(R[2:6,])[1,2]
# cov(R[3:7,])[1,2]
# cov(R[4:8,])[1,2]

set.seed(123)
x <- rnorm(1e4, 0, 0.1)
y <- rnorm(1e4, 0, 0.15)
R <- cbind(x, y)

cov(R[1:5,])[1,2]
cov(R[2:6,])[1,2]
cov(R[3:7,])[1,2]
cov(R[4:8,])[1,2]
cov(R[5:9,])[1,2]
foo(R, 5)[1:5]

all.equal(foo(R, 5), rollfoo(R, 5))

rbenchmark::benchmark(foo(R, 5), rollfoo(R, 5))
