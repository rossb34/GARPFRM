\name{consumption}
\docType{data}
\alias{consumption}
\title{consumption data}
\description{
Monthly consumption data (CPI: Consumer Price Index) from the Bureau of Labor Statistics (BLS)
}
\usage{consumption}
\details{
Data is retrieved from the BLS
The CPI represents changes in prices of all goods and services purchased for consumption by urban households.
Prices are taken throughout the month.

see \url{http://www.bls.gov/opub/hom/pdf/homch17.pdf}
}
\format{xts object with monthly observations}
\source{\url{http://www.bls.gov/cpi/cpiovrvw.htm}}
\references{

}
\examples{
data(consumption)
head(consumption)
}
\keyword{datasets}
