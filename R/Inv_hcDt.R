#' Fetch a Ticker's Historical Data from Investing.com
#'
#' @description This function retrieves historical data for a given financial ticker from Investing.com using their public API.
#' It allows specifying an interval and period to adjust the frequency of the data points.
#'
#' @param ticker_id The ticker identifier as a number. This is the unique ID for the financial asset on Investing.com.
#' @param interval A string representing the interval between data points.
#' It can be one of the following values:
#' \itemize{
#'  \item "PT1M" (1 minute),
#'  \item "PT5M" (5 minutes),
#'  \item "PT15M" (15 minutes),
#'  \item "PT30M" (30 minutes),
#'  \item "PT1H" (1 hour),
#'  \item "PT5H" (5 hours),
#'  \item "P1D" (1 day),
#'  \item "P1W" (1 week),
#'  \item "P1M" (1 month).
#' }
#' Defaults to \code{NULL}. If \code{period} is set, the function automatically adjusts \code{interval}.
#'
#' @param period A string representing the total time range for the data.
#' It can be one of the following values:
#' \itemize{
#'  \item "P1Y" (1 year),
#'  \item "P5Y" (5 years),
#'  \item "MAX" (maximum available data),
#'  \item "P1D" (1 day),
#'  \item "P1W" (1 week),
#'  \item "P1M" (1 month).
#' }
#' Defaults to \code{NULL}. If provided, the function adjusts \code{interval} accordingly.
#'
#' @param max_retries Integer value representing the maximum number of retries in case of a failed API request. Default is 5.
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \code{Date}: The date and time of the data point.
#'   \item \code{Open}: The opening price.
#'   \item \code{High}: The highest price during the interval.
#'   \item \code{Low}: The lowest price during the interval.
#'   \item \code{Close}: The closing price.
#'   \item \code{Volume}: The trading volume during the interval.
#'   \item \code{Extra_info}: Optional additional information (only included if present in the API response).
#' }
#'
#' @details The function queries Investing.com's API for historical financial data based on the given parameters.
#' It allows for the retrieval of data at various intervals and over different periods.
#' The function supports retry attempts in case of a failed API call.
#'
#' @author Koffi Frederic SESSIE
#'
#' @examples
#' library(httr)
#' library(jsonlite)
#' library(tibble)
#' library(glue)
#' library(rlang)
#' #\donttest{
#' # Fetch historical data daily for France 5Y OAT
#' data <- Inv_hcDt(ticker_id = 23769) #23763 is OAT 5y id
#' head(data)
#'
#' # Fetch historical data for France 5Y OAT over 1 year with weekly interval
#' data_w <- Inv_hcDt(ticker_id = 23769, interval <- 'P1W', period = "P1Y")
#' head(data_w)
#' #}
#'
#' @export
#'

Inv_hcDt <- function(ticker_id, interval = NULL, period = NULL, max_retries = 5) {
  period <- period
  interval <- interval

  acceptable_interval <- c("PT1M", "PT5M", "PT15M","PT30M", # For minutes
                           "PT1H", "PT5H", # For hours
                           "P1D", # one day
                           "P1W", # one week
                           "P1M" # one month
  )

  acceptable_period <- c("P1Y", "P5Y", "MAX",
                         "PT1M","PT5M", "PT15M", "PT30M",
                         "PT1H", "PT5H",
                         "P1D",
                         "P1W",
                         "P1M")

  pointscount <- 160

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"

  url <- glue::glue("https://api.investing.com/api/financialdata/{ticker_id}/historical/chart/")

  if (is.null(period)) {
    if (is.null(interval)) {
      # period <- 'MAX'
      # interval <- 'P1D'
      params = list(
        interval = 'P1D',
        period = 'MAX',
        pointscount = pointscount
      )

    } else {
      if (interval %in% acceptable_interval) {
        params = list(
          interval = interval,
          # period = period,
          pointscount = pointscount
        )
        # url <- glue::glue("https://api.investing.com/api/financialdata/{ticker_id}/historical/chart/?interval={interval}&pointscount={pointscount}")

      } else {
        stop(glue::glue("{interval} is not a valid interval"))
      }
    }
  } else {
    if (period %in% acceptable_period) {
      if (period == 'P1D') {
        interval <- 'PT5M'
      } else if (period == 'P1W') {
        interval <- 'PT30M'
      } else if (period == 'P1M') {
        interval <- 'PT5H'
      } else if (period == 'P3M') {
        interval <- 'P1D'
      } else if (period == 'P6M') {
        interval <- 'P1D'
      } else if (period == 'P1Y') {
        interval <- 'P1W'
      } else if (period == 'P5Y') {
        interval <- 'P1M'
      } else if (period == 'MAX') {
        interval <- 'P1M'
      }

      params = list(
        interval = interval,
        period = period,
        pointscount = pointscount
      )

      # url <- glue::glue("https://api.investing.com/api/financialdata/{ticker_id}/historical/chart/?interval={interval}&period={period}&pointscount={pointscount}")
    } else {
      stop(glue::glue("{period} is not a valid period"))
    }
  }

  for (attempt in 1:max_retries) {
    tryCatch({

      response <- httr::GET(url, add_headers(`Connection` = "keep-alive", `User-Agent` = UA), query = params)

      content <- httr::content(response, as = "text", encoding = "UTF-8")

      json_data <- jsonlite::fromJSON(content)

      # if("data" %in% names(json_data)){
      #   json_data <- json_data$data
      # }

      # try_get_data <- read_html(url)
      # Sys.sleep(2)
      # try_get_data <- xml_text(try_get_data %>% rvest::html_nodes('p')) %>% fromJSON()

      if (is_empty(json_data$data)) {
        # if (is_empty(try_get_data$data)) {
        stop('No data available for the ticker... Be sure to provide good parameters')
      } else {
        # ticker_dt <- try_get_data$data
        ticker_dt <- json_data$data

        if (all(ticker_dt[,7] == 0)) {
          ticker_dt <- ticker_dt[,-7]
          names(ticker_dt) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume')
        } else {
          names(ticker_dt) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Extra_info')
        }

        ticker_dt <- as_tibble(ticker_dt)
        names(ticker_dt) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume')
        ticker_dt$Date <- as.POSIXct((ticker_dt$Date+0.1)/1000, origin = "1970-01-01")

        return(ticker_dt)
      }
    },
    error = function(e) {
      message(glue::glue("Attempt {attempt} failed: {e$message}"))
      if (attempt == max_retries) {
        stop("Max retries reached. Make sure you have an active internet connection and valid parameters.")
      }
    },
    warning = function(w) {
      message(glue::glue("Attempt {attempt} warning: {w$message}"))
    })

    Sys.sleep(1) # Pause before the next attempt
  }
}
