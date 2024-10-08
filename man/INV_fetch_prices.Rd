% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/INV_fetch_prices.R
\name{INV_fetch_prices}
\alias{INV_fetch_prices}
\title{Fetch Historical Price Data from Investing.com}
\usage{
INV_fetch_prices(
  ticker_info,
  from = Sys.Date() - 365 * 2,
  to = Sys.Date(),
  time_frame = "Daily",
  max_retries = 5,
  retry_delay = 1
)
}
\arguments{
\item{ticker_info}{A string representing the ticker symbol or company name to fetch the financial data for (e.g., \code{"AAPL"} for Apple).}

\item{from}{The start date of the historical data in "YYYY-MM-DD" format. The default is 2 years prior to the current date.}

\item{to}{The end date of the historical data in "YYYY-MM-DD" format. The default is the current date.}

\item{time_frame}{A string representing the time frame for the data. Can be \code{"Daily"}, \code{"Weekly"}, or \code{"Monthly"}. The default is \code{"Daily"}.}

\item{max_retries}{An integer indicating the maximum number of retry attempts in case of a request failure. The default is 5.}

\item{retry_delay}{A numeric value representing the number of seconds to wait between retry attempts. The default is 1 second.}
}
\value{
A tibble containing the following columns:
\describe{
\item{Date}{The date of the data point.}
\item{Open}{The opening price for the date.}
\item{High}{The highest price for the date.}
\item{Low}{The lowest price for the date.}
\item{Close}{The closing price for the date.}
\item{Volume}{The trading volume for the date.}
\item{Change_percent}{The percentage change in price from the previous close.}
}
}
\description{
This function retrieves historical financial data such as Open, High, Low, Close prices, Volume, and Percentage Change from Investing.com for a specified ticker symbol. It allows you to specify the date range and the desired time frame (daily, weekly, or monthly).
}
\details{
The function makes use of Investing.com's API to retrieve historical financial data for the specified ticker symbol. It includes a retry mechanism to handle request failures with customizable retry limits and delay intervals between retries.
}
\examples{
\dontrun{
library(stringr)
library(rlang)
library(tibble)
# Fetch daily data for ETIT from 2024-09-08 to 2024-05-08
dt <- INV_fetch_prices("ETIT", from = "2024-09-08", to = "2024-05-08", time_frame = 'Daily')

# Fetch monthly data for AAPL from 2022-01-01 to 2024-01-01
dt_apple <- INV_fetch_prices("AAPL", from = "2022-01-01", to = "2024-01-01", time_frame = 'Monthly')
}
}
\seealso{
\code{\link{INV_hcDt}} for related data fetching utilities.
}
\author{
Koffi Frederic SESSIE
}
