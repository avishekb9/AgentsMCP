% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annem_data.R
\name{load_market_data}
\alias{load_market_data}
\title{Market Data Functions for ANNEM}
\usage{
load_market_data(symbols, period = "2y", from = NULL, to = NULL)
}
\arguments{
\item{symbols}{Character vector of stock symbols (e.g., c("AAPL", "MSFT"))}

\item{period}{Character string specifying time period (default: "2y")}

\item{from}{Date to start data collection (optional)}

\item{to}{Date to end data collection (optional)}
}
\value{
List containing:
\itemize{
\item prices: Matrix of adjusted close prices
\item returns: Matrix of log returns
\item volatility: Vector of rolling volatility measures
}
}
\description{
Functions for loading, processing, and managing financial market data
for ANNEM simulations. Includes data retrieval from various sources,
return calculations, and market sentiment processing.
Load Market Data

Downloads and processes daily stock market data for ANNEM analysis.
Uses quantmod to retrieve data from Yahoo Finance.
}
\examples{
\dontrun{
data <- load_market_data(c("AAPL", "MSFT", "GOOGL"))
head(data$prices)
head(data$returns)
}

}
