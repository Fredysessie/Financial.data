### Get url and ISIN
# Après préciser le type d'exchange
INV_get_info <- function(info, language="en", exchange = NULL) {

  if (!is.character(info)) {
    stop(glue("{info} must be a character"))
  }

  # Supprimer le dernier espace si nécessaire
  if (substr(info, nchar(info), nchar(info)) == " ") {
    info <- substr(info, 1, nchar(info) - 1)
  }

  # Encoder l'info pour l'URL
  info <- URLencode(gsub(" ", "+", info))

  info <- URLencode(gsub("&", "%26", info))

  base_url = INV_base_url(language)

  # Construire l'URL de recherche
  search_url <- glue("https://api.investing.com/api/search/v2/search?q={info}")

  # Effectuer la requête GET
  search_info <- GET(search_url)

  # Vérifier le statut de la réponse
  if (status_code(search_info) == 200) {
    search_info_json <- content(search_info, as = "text", encoding = "UTF-8")

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
        # Après chercher à faire des filtre sur les type de stock
        # search_quotes = search_quotes[1,]
        all_isin = NULL
        all_ytd = NULL

        for(elm in search_quotes$url){
          html_page = read_html(elm)

          the_isin = html_page%>%
            html_nodes("dd[data-test='isin']")%>%
            html_text()

          the_ytd = html_page%>%
            html_nodes("dd[data-test='oneYearReturn']")%>%
            html_text()

          if(length(the_isin)!=0){
            # Supprimer le dernier espace si nécessaire
            if (substr(the_isin, nchar(the_isin), nchar(the_isin)) == " ") {
              the_isin <- substr(the_isin, 1, nchar(the_isin) - 1)
            } #else{
            #   the_isin <- the_isin
            # }

          } else{
            the_isin <- NA
          }

          if(length(the_ytd)!=0){
            # Supprimer le dernier espace si nécessaire
            if (substr(the_ytd, nchar(the_ytd), nchar(the_ytd)) == " ") {
              the_ytd <- substr(the_ytd, 1, nchar(the_ytd) - 1)


            } #else{
            #   the_ytd <- the_ytd
            # }

            # if(language == "en"){
            the_ytd <- gsub(",", ".", the_ytd)
            the_ytd <- as.numeric(gsub("%", "", the_ytd))
            # }else{
            #   the_ytd <- gsub(",", ".", the_ytd)
            #   the_ytd <- as.numeric(gsub("%", "", the_ytd))
            # }


          } else{
            the_ytd <- NA
          }

          all_isin = append(all_isin, the_isin)
          all_ytd = append(all_ytd, the_ytd)

        }

        search_quotes$isin <- all_isin
        search_quotes$ytd <- all_ytd

        if (!is.null(exchange) && is.character(exchange)) {
          exchange = toupper(exchange)

          index_exchange <- grep(exchange, search_quotes$exchange)

          if (length(index_exchange)!=0) {
            search_quotes <- as_tibble(search_quotes[index_exchange,])

          }

        }

        return(search_quotes)

      } else {
        return("")
      }
    } else {
      stop(glue("{info} has no ID. Please check your input."))
    }
  } else {
    # Gérer les erreurs de requête
    stop(glue("Erreur {status_code(search_info)}: {content(search_info, as = 'text')}"))
  }
}
