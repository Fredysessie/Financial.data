% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/INV_base_url.R
\name{INV_base_url}
\alias{INV_base_url}
\title{Retrieve the Base URL of Investing.com Based on the Selected Language}
\usage{
INV_base_url(language_id = "en")
}
\arguments{
\item{language_id}{A two-letter string representing the language code (e.g., \code{"en"} for English or \code{"fr"} for French). The default is \code{"en"}.}
}
\value{
A string containing the base URL of Investing.com for the specified language. If the language ID is not supported, the function raises an error.
}
\description{
This function returns the base URL of Investing.com according to the selected language. By default, the base URL is returned in English. It also supports French (\code{"fr"}) or any other available language on the website.
}
\details{
The function uses web scraping to fetch available language options from Investing.com. If the language code is provided as \code{"en"} or \code{"fr"}, the URLs are returned directly without scraping. For other languages, the function checks the Investing.com language selector.
}
\examples{
# Get the URL in English (default)
INV_base_url("en")

# Get the URL in French
INV_base_url("fr")

}
\author{
Koffi Frederic SESSIE
}
