fetch_inv_prices <- function(ticker_info,
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
