\name{GARPFRM-package}
\alias{GARPFRM-package}
\alias{GARPFRM}
\docType{package}
\title{
Functions to implement the topics presented in 'Financial Risk Manager (FRM) Part 1' (2012) series of books.
}
\description{
This package provides a framework to implement the topics and ideas presented in 'Financial Risk Manager (FRM) Part 1: Quantitative Analysis' (2012) and 'Financial Risk Manager (FRM) Part 1: Foundations of Risk Management' (2012).
}

\details{
\tabular{ll}{
Package: \tab GARPFRM\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2014-04-03\cr
License: \tab GPL\cr
}

The purpose of this package is to implement the concepts and methods presented in the Global Association of Risk Professionals (GARP) Financial Risk Manager (FRM) Part I series of books. Developing the \code{GARPFRM} package is a collaborative project between the University of Washington Computational Finance & Risk Management Program and the Global Association of Risk Professionals to develop R packages that facilitate the learning of risk management concepts. The \code{GARPFRM} package provides a framework to implement the topics presented in the Financial Risk Manager (FRM) Part 1 books. The user should be able to follow along in the books and using the \code{GARPFRM} package and supporting packages.

TODO: Add stuff about GARP

TODO: Add stuff about UW-CF&RM

The current version of the package covers the majority of chapters in 'Financial Risk Manager (FRM) Part 1: Quantitative Analysis' (2012) and 'Financial Risk Manager (FRM) Part 1: Foundations of Risk Management' (2012). Due to the nature of econometrics involving time series data, nearly all functions in this package require data as an \code{xts} object and \verb{GARPFRM} uses the \code{\link[xts]{xts}} package for working with time series data.
\itemize{
  \item Delineating Efficient Portfolios
  
  This section covers the risk and return characteristics of combinations of securities and what is known as the efficient frontier. We first consider the case of a portfolio consisting of two assets and the impact that correlation has on the shape of the efficient frontier. We also learn how to compute the optimal portfolio, optimal in the sense that the portfolio has the maximium return for a given risk. These ideas are then generalized for a portfolio consisting of any number of assets. Key functions in this section are \code{\link{portSDTwoAsset}}, \code{\link{portReturnTwoAsset}}, \code{\link{efficientFrontierTwoAsset}}, \code{\link{efficientFrontierTwoAsset}}, and \code{\link{efficientFrontier}}. See \code{vignette("DelineatingEfficientPortfolios")} for several examples and explanations demonstrating these functions.
  
  \item Capital Asset Pricing Model
  
  The CAPM section discusses estimating alpha and beta as well as their statistical significance. Visually interest functions are chartSML and plot. Of primary applicability are the functions getStatistics and hypTest used to retrieve alpha, beta and their relevant statistic as well as estimating their significance. \code{\link{getStatistics}}, \code{\link{getAlphas}}, \code{\link{getBetas}}, \code{\link{plot}}, \code{\link{hypTest}}, and \code{\link{chartSML}}. See \code{vignette("CAPM_TF")} for an implementation and explanation of these functions.
  
  \item Performance Measures
  
  The Performance Measures section to evaluate performance in terms of return and risk, i.e. risk adjusted performance measures, to more easily compare assets with different levels of risk. The functions provided in this section are from the \code{PerformanceAnalytics} package. The key functions in this section are \code{\link[PerformanceAnalytics]{TreynorRatio}}, \code{\link[PerformanceAnalytics]{SharpeRatio}}, \code{\link[PerformanceAnalytics]{CAPM.jensenAlpha}}, \code{\link[PerformanceAnalytics]{CAPM.jensenAlpha}}, \code{\link[PerformanceAnalytics]{TrackingError}}, \code{\link[PerformanceAnalytics]{InformationRatio}}, and \code{\link[PerformanceAnalytics]{SortinoRatio}}. See \code{vignette("PerformanceMeasures")} for several examples and explanations demonstrating these functions. See \href{http://spark.rstudio.com/rossbennett3/PerformanceMeasures/}{here} for an interactive web application.
  
  \item Quantitative Analysis
  
  Chapters 1-8 in Quantitative Analysis cover probability, statistics, and linear regression. Functions to implement these topics are included in the base R distribution. See \code{vignette("QuantitativeAnalysisBasics")} for several examples and explanations.
  
  \item Monte Carlo Methods
  
  The Monte Carlo Methods chapter presents simulation methods used in derivatives pricing and risk management. The implementation in this package focuses on Monte Carlo simulations with one random variable (i.e. one source of risk) and bootstrap resampling. Key functions are \code{\link{monteCarlo}}, \code{\link{bootFUN}}, \code{\link{bootMean}}, \code{\link{bootSD}}, \code{\link{bootStdDev}}, \code{\link{bootSimpleVolatility}}, \code{\link{bootCor}}, \code{\link{bootCov}}, \code{\link{bootVaR}}, and \code{\link{bootES}}. See the demos \code{demo("monte_carlo")} and \code{demo("bootstrap")}. See \code{vignette("MonteCarloMethods")} for several examples and explanations. Additionally, an interactive web application is available \href{http://spark.rstudio.com/rossbennett3/MonteCarlo/}{here} for generating asset price paths via Monte Carlo simulation.
  
  \item Estimating Volatilities and Correlation
  
  The main emphasis of this section is estimating volatilities and correlations using an EWMA model and a GARCH model. We provide an \code{\link{EWMA}} function to estimate volatilities, covariances, and correlations. We also provide a \code{\link{forecast}} function to forecast volatility from an EWMA model. To implement univariate GARCH models, we make use of the \verb{rugarch} package. The \code{\link{uvGARCH}} function is a wrapper around \code{ugarchspec} and \code{ugarchfit} to specify and fit a GARCH model. We provide a \code{\link{forecast}} function to forecast volatility from a GARCH model. The \code{\link{forecast}} function is wrapper around \code{ugarchforecast}. 
  
  See \code{vignette("EstimatingVolatilitiesCorrelation")} for several examples and explanations of the EWMA and GARCH models. We also provide interactive web applications to implement \href{http://spark.rstudio.com/rossbennett3/EWMA/}{EWMA} and \href{https://github.com/rossb34/UnivariateGARCH}{GARCH} models.
  
  \item Quantifying Volatility in VaR Models
  
  This section discusses Value at Risk (VaR) models and different methods to estimate VaR levels. Among the methods discussed are EWMA, GARCH, historical simulation, and the VarCov approach. See \code{vignette("QuantifyingVolatilityVaRModels")} for several examples and explanations. Additionally, an interactive web application to implement VaR backtests is available \href{http://spark.rstudio.com/rossbennett3/BacktestVaR/}{here}.
}
}
\author{
Ross Bennett and Thomas Fillebeen

Maintainer: Thomas Fillebeen <tdf17@uw.edu>

Contributors: Mark L. Labovitz, Kirk Li, Doug Martin, Guy Yollin
}

\references{
TODO: Add references for GARP books
}

\keyword{ package }
\seealso{
% Optional links to other man pages, e.g.
\code{\link[PerformanceAnalytics:PerformanceAnalytics-package]{PerformanceAnalytics}} \cr
\code{\link[rugarch:rugarch-package]{rugarch}} \cr
\code{\link[xts:xts-package]{xts}} \cr
\code{\link[xts]{xts}} \cr
CRAN task view on Empirical Finance \cr \url{http://cran.r-project.org/src/contrib/Views/Econometrics.html} \cr
}
\examples{
% simple examples of the most important functions
}
