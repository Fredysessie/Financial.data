#' Retrieve Information from Investing.com
#'
#' This function retrieves financial information, including ISIN and Year-To-Date (YTD) performance data, from Investing.com based on a search query.
#'
#' @param info Character. The search term or financial asset (e.g., stock symbol, ISIN) to query. Must be a string.
#' @param language Character. The language for the search results. Defaults to "en" for English. Other languages can be passed (e.g., "fr" for French).
#' @param exchange Character. Optional. The stock exchange to filter results by. If specified, the results will be filtered to match the provided exchange name.
#'
#' @return A data frame containing financial information related to the search query, including:
#'   \item{id}{The identifier of the asset on Investing.com.}
#'   \item{url}{The URL to the asset's page on Investing.com.}
#'   \item{description}{A brief description of the asset.}
#'   \item{symbol}{The asset's ticker symbol.}
#'   \item{exchange}{The stock exchange where the asset is traded.}
#'   \item{flag}{The country or region of the asset.}
#'   \item{type}{The type of financial asset (e.g., stock).}
#'   \item{isin}{The asset's ISIN (International Securities Identification Number), if available.}
#'   \item{ytd}{The asset's Year-To-Date (YTD) performance, if available.}
#'
#' @author Koffi Frederic SESSIE
#'
#' @examples
#' library(httr)
#' library(jsonlite)
#' library(rvest)
#' library(glue)
#' library(magrittr)
#'
#' # Retrieve information on Ecobank Transnational Inc.
#' INV_get_info("ETIT")
#'
#' # Retrieve information on a financial asset in French
#' INV_get_info("ETIT", language = "fr")
#'
#' \donttest{
#' # Retrieve 'societe general sa' information on EuroTLX exchange
#' INV_get_info("societe general sa", exchange = "EuroTLX")
#' }
#'
#' @import magrittr
#' @export

INV_get_info <- function(info, language="en", exchange = NULL) {

  if (!is.character(info)) {
    stop(glue::glue("{info} must be a character"))
  }

  # Supprimer le dernier espace si nécessaire
  info <- trimws(info)  # Utilise trimws pour supprimer les espaces superflus

  headers = c(
    accept = "*/*",
    `accept-language` = "fr,fr-FR;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    `content-type` = "application/json",
    `domain-id` = "www",
    origin = "https://www.investing.com",
    priority = "u=1, i",
    referer = "https://www.investing.com/",
    `sec-ch-ua` = '"Chromium";v="128", "Not;A=Brand";v="24", "Microsoft Edge";v="128"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"Windows"',
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-site",
    `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 Edg/128.0.0.0"
  )

  base_url = INV_base_url(language)

  search_url <- "https://api.investing.com/api/search/v2/search"

  params = list(
    q = info
  )

  # Effectuer la requête GET
  search_info <- httr::GET(url = search_url, httr::add_headers(.headers=headers), query = params)

  # Vérifier le statut de la réponse
  if (httr::status_code(search_info) == 200) {
    search_info_json <- httr::content(search_info, as = "text", encoding = "UTF-8")

    # Gérer les erreurs de conversion JSON
    tryCatch({
      json_info <- jsonlite::fromJSON(search_info_json)
    }, error = function(e) {
      stop("Erreur lors de la conversion JSON: ", e$message)
    })

    if ("quotes" %in% names(json_info)) {
      search_quotes <- json_info$quotes
      search_quotes$url = paste0(base_url, search_quotes$url)

      if (length(search_quotes) != 0) {
        all_isin = NULL
        all_ytd = NULL

        UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"

        for (elm in search_quotes$url) {
          response <- httr::GET(
            elm,
            httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"),
            httr::add_headers(
              Referer = "https://www.investing.com",
              `Accept-Language` = "en-US,en;q=0.9",
              `Accept-Encoding` = "gzip, deflate",
              Connection = "keep-alive"
            )
          )

          # Parse the HTML content from the response
          html_page <- httr::content(response, as = "text")
          html_page <- rvest::read_html(html_page)

          # Extraire ISIN et YTD en vérifiant la validité des données avant traitement
          the_isin <- html_page%>%
            html_node("dd[data-test='isin']")%>%
            html_text()

          the_ytd <- html_page%>%
            html_node("dd[data-test='oneYearReturn']")%>%
            html_text()

          # Vérifier si the_isin est NULL ou NA avant d'utiliser substr()
          if (!is.null(the_isin) && !is.na(the_isin) && nchar(the_isin) > 0) {
            the_isin <- trimws(the_isin)  # Utiliser trimws pour supprimer les espaces en début et fin
          } else {
            the_isin <- NA
          }

          # Vérifier si the_ytd est NULL ou NA avant traitement
          if (!is.null(the_ytd) && !is.na(the_ytd) && nchar(the_ytd) > 0) {
            the_ytd <- trimws(the_ytd)  # Utiliser trimws
            the_ytd <- as.numeric(gsub("%", "", gsub(",", ".", the_ytd)))
          } else {
            the_ytd <- NA
          }

          all_isin <- append(all_isin, the_isin)
          all_ytd <- append(all_ytd, the_ytd)
        }

        search_quotes$isin <- all_isin
        search_quotes$ytd <- all_ytd

        if (!is.null(exchange) && is.character(exchange)) {
          exchange <- toupper(exchange)
          index_exchange <- grep(exchange, toupper(search_quotes$exchange))

          if (length(index_exchange) != 0) {
            search_quotes <- tibble::as_tibble(search_quotes[index_exchange, ])
          }
        }

        return(search_quotes)
      } else {
        return("")
      }
    } else {
      stop(glue::glue("{info} has no ID. Please check your input."))
    }
  } else {
    # Gérer les erreurs de requête
    stop(glue::glue("Erreur {httr::status_code(search_info)}: {httr::content(search_info, as = 'text')}"))
  }
}



# INV_get_info <- function(info, language="en", exchange = NULL) {
#
#   if (!is.character(info)) {
#     stop(glue("{info} must be a character"))
#   }
#
#   # Supprimer le dernier espace si nécessaire
#   if (substr(info, nchar(info), nchar(info)) == " ") {
#     info <- substr(info, 1, nchar(info) - 1)
#   }
#
#   headers = c(
#     accept = "*/*",
#     `accept-language` = "fr,fr-FR;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
#     `content-type` = "application/json",
#     `domain-id` = "www",
#     origin = "https://www.investing.com",
#     priority = "u=1, i",
#     referer = "https://www.investing.com/",
#     `sec-ch-ua` = '"Chromium";v="128", "Not;A=Brand";v="24", "Microsoft Edge";v="128"',
#     `sec-ch-ua-mobile` = "?0",
#     `sec-ch-ua-platform` = '"Windows"',
#     `sec-fetch-dest` = "empty",
#     `sec-fetch-mode` = "cors",
#     `sec-fetch-site` = "same-site",
#     `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 Edg/128.0.0.0"
#   )
#
#   # Encoder l'info pour l'URL
#   # info <- URLencode(gsub(" ", "+", info))
#
#   # info <- URLencode(gsub("&", "%26", info))
#
#   base_url = INV_base_url(language)
#
#   # Construire l'URL de recherche
#   # search_url <- glue("https://api.investing.com/api/search/v2/search?q={info}")
#   search_url <- "https://api.investing.com/api/search/v2/search"
#
#   params = list(
#     q = info
#   )
#
#   # Effectuer la requête GET
#   # search_info <- GET(search_url)
#   search_info <- httr::GET(url = search_url, httr::add_headers(.headers=headers), query = params)
#
#   # Vérifier le statut de la réponse
#   if (status_code(search_info) == 200) {
#     search_info_json <- httr::content(search_info, as = "text", encoding = "UTF-8")
#
#     # Gérer les erreurs de conversion JSON
#     tryCatch({
#       json_info <- jsonlite::fromJSON(search_info_json)
#     }, error = function(e) {
#       stop("Erreur lors de la conversion JSON: ", e$message)
#     })
#
#     if ("quotes" %in% names(json_info)) {
#       search_quotes <- json_info$quotes
#
#       search_quotes$url = paste0(base_url, search_quotes$url)
#       if (length(search_quotes) != 0) {
#         # Après chercher à faire des filtre sur les type de stock
#         # search_quotes = search_quotes[1,]
#         all_isin = NULL
#         all_ytd = NULL
#
#         UA = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"
#
#         for(elm in search_quotes$url){
#           # html_page = rvest::read_html(elm)
#
#           response <- GET(
#             elm,
#             httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"),
#             add_headers(
#               Referer = "https://www.investing.com",
#               `Accept-Language` = "en-US,en;q=0.9",
#               `Accept-Encoding` = "gzip, deflate",
#               Connection = "keep-alive"
#             )
#           )
#
#           # Parse the HTML content from the response
#           html_page <- httr::content(response, as = "text")
#           html_page <- rvest::read_html(html_page)
#
#
#
#           the_isin = html_page%>%
#             html_node("dd[data-test='isin']")%>%
#             html_text()
#
#           the_ytd = html_page%>%
#             html_node("dd[data-test='oneYearReturn']")%>%
#             html_text()
#
#           if(length(the_isin)!=0){
#             # Supprimer le dernier espace si nécessaire
#             if (substr(the_isin, nchar(the_isin), nchar(the_isin)) == " ") {
#               the_isin <- substr(the_isin, 1, nchar(the_isin) - 1)
#             } #else{
#             #   the_isin <- the_isin
#             # }
#
#           } else{
#             the_isin <- NA
#           }
#
#           if(length(the_ytd)!=0){
#             # Supprimer le dernier espace si nécessaire
#             if (substr(the_ytd, nchar(the_ytd), nchar(the_ytd)) == " ") {
#               the_ytd <- substr(the_ytd, 1, nchar(the_ytd) - 1)
#
#
#             } #else{
#             #   the_ytd <- the_ytd
#             # }
#
#             # if(language == "en"){
#             the_ytd <- gsub(",", ".", the_ytd)
#             the_ytd <- as.numeric(gsub("%", "", the_ytd))
#             # }else{
#             #   the_ytd <- gsub(",", ".", the_ytd)
#             #   the_ytd <- as.numeric(gsub("%", "", the_ytd))
#             # }
#
#
#           } else{
#             the_ytd <- NA
#           }
#
#           all_isin = append(all_isin, the_isin)
#           all_ytd = append(all_ytd, the_ytd)
#
#         }
#
#         search_quotes$isin <- all_isin
#         search_quotes$ytd <- all_ytd
#
#         if (!is.null(exchange) && is.character(exchange)) {
#           exchange = toupper(exchange)
#
#           index_exchange <- grep(exchange, search_quotes$exchange)
#
#           if (length(index_exchange)!=0) {
#             search_quotes <- tibble::as_tibble(search_quotes[index_exchange,])
#
#           }
#
#         }
#
#         return(search_quotes)
#
#       } else {
#         return("")
#       }
#     } else {
#       stop(glue("{info} has no ID. Please check your input."))
#     }
#   } else {
#     # Gérer les erreurs de requête
#     stop(glue("Erreur {status_code(search_info)}: {httr::content(search_info, as = 'text')}"))
#   }
# }
