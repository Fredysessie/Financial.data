#' Fetch Historical Price Data from Investing.com
#'
#' This function retrieves historical financial data such as Open, High, Low, Close prices, Volume, and Percentage Change from Investing.com for a specified ticker symbol. It allows you to specify the date range and the desired time frame (daily, weekly, or monthly).
#'
#' @param ticker_info A string representing the ticker symbol or company name to fetch the financial data for (e.g., `"AAPL"` for Apple).
#' @param from The start date of the historical data in "YYYY-MM-DD" format. The default is 2 years prior to the current date.
#' @param to The end date of the historical data in "YYYY-MM-DD" format. The default is the current date.
#' @param time_frame A string representing the time frame for the data. Can be `"Daily"`, `"Weekly"`, or `"Monthly"`. The default is `"Daily"`.
#' @param max_retries An integer indicating the maximum number of retry attempts in case of a request failure. The default is 5.
#' @param retry_delay A numeric value representing the number of seconds to wait between retry attempts. The default is 1 second.
#'
#' @return A tibble containing the following columns:
#' \describe{
#'   \item{Date}{The date of the data point.}
#'   \item{Open}{The opening price for the date.}
#'   \item{High}{The highest price for the date.}
#'   \item{Low}{The lowest price for the date.}
#'   \item{Close}{The closing price for the date.}
#'   \item{Volume}{The trading volume for the date.}
#'   \item{Change_percent}{The percentage change in price from the previous close.}
#' }
#'
#' @details The function makes use of Investing.com's API to retrieve historical financial data for the specified ticker symbol. It includes a retry mechanism to handle request failures with customizable retry limits and delay intervals between retries.
#'
#' @examples
#' # Fetch daily data for ETIT from 2024-09-08 to 2024-05-08
#' dt <- INV_fetch_prices("ETIT", from = "2024-09-08", to = "2024-05-08", time_frame = 'Daily')
#'
#' # Fetch monthly data for AAPL from 2022-01-01 to 2024-01-01
#' dt_apple <- INV_fetch_prices("AAPL", from = "2022-01-01", to = "2024-01-01", time_frame = 'Monthly')
#'
#' @seealso \code{\link{INV_hcDt}} for related data fetching utilities.
#'
#' @author
#' Koffi Frederic SESSIE
#'
#' @export


INV_fetch_prices <- function(ticker_info,
                             from = Sys.Date() - 365*2,
                             to = Sys.Date(),
                             time_frame = 'Daily',
                             max_retries = 5, retry_delay = 1) {

  time_frame <- str_to_title(time_frame)

  if(!(time_frame %in% c("Daily", "Weekly", "Monthly"))){
    stop("time_frame can only be 'Daily', 'Weekly' or 'Monthly'.")
  }

  # Récupérer l'ID du ticker
  the_info <- INV_get_info(ticker_info)
  ticker_id <- the_info$id

  # Paramètres de la requête
  params <- list(
    `start-date` = from,
    `end-date` = to,
    `time-frame` = time_frame,
    `add-missing-rows` = 'false'
  )

  url <- glue::glue("https://api.investing.com/api/financialdata/historical/{ticker_id}")

  # En-têtes de la requête
  headers <- c(
    "content-type" = "application/json",
    "domain-id" = "www",
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "\"Windows\"",
    "Referer" = "https://www.investing.com/",
    "Referrer-Policy" = "strict-origin-when-cross-origin"
  )

  # Boucle pour gérer les tentatives
  for (retry in 1:max_retries) {
    tryCatch({
      response <- GET(url, add_headers(.headers = headers), query = params)

      # Vérifier le code de statut HTTP
      if (status_code(response) != 200) {
        stop("Failed to retrieve data. HTTP Status: ", status_code(response))
      }

      # Lire le contenu de la réponse
      content <- httr::content(response, "text", encoding = "UTF-8")
      json <- jsonlite::fromJSON(content)

      # Vérifier si la structure des données est valide
      if (is.null(json$data)) {
        stop("No data found for the provided ticker.")
      }

      # Extraire les données et renommer les colonnes
      data <- as_tibble(json$data)[, c("rowDateTimestamp", "last_open", "last_max", "last_min", "last_close", "volumeRaw", "change_precent")]

      names(data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Change_percent")

      # Convertir les colonnes en numériques
      data <- data %>%
        mutate(
          Date = as.Date(as.POSIXct(Date, origin = "1970-01-01")),
          Open = as.numeric(gsub(",", "", Open)),
          High = as.numeric(gsub(",", "", High)),
          Low = as.numeric(gsub(",", "", Low)),
          Close = as.numeric(gsub(",", "", Close)),
          Volume = as.numeric(gsub(",", "", Volume)),
          Change_percent = as.numeric(gsub(",", "", Change_percent))
        )

      return(data)

    }, error = function(e) {
      if (retry < max_retries) {
        message(paste0("Error encountered: ", e$message, ". Retrying in ", retry_delay, " seconds..."))
        Sys.sleep(retry_delay)
      } else {
        stop("Max retries reached. Could not retrieve data.")
      }
    })
  }
}
