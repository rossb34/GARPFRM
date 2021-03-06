% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/EWMA.R
\name{realizedCor}
\alias{realizedCor}
\title{Realized Correlation}
\usage{
realizedCor(R, n)
}
\arguments{
\item{R}{xts object of asset returns}

\item{n}{number of periods used to calculate realized volatility}
}
\value{
xts object of realized correlation
}
\description{
Calculate realized correlation
}
\details{
Realized correlation is calculated using the previous \code{n} periods.
}
\examples{
data(crsp_weekly)
R <- largecap_weekly[, 1:2]
# Calculate realized correlation
realizedCorrelation <- realizedCor(R, 10)
head(realizedCorrelation)
}
\author{
Ross Bennett
}

