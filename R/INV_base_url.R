#' Retrieve the Base URL of Investing.com Based on the Selected Language
#'
#' This function returns the base URL of Investing.com according to the selected language. By default, the base URL is returned in English. It also supports French (`"fr"`) or any other available language on the website.
#'
#' @param language_id A two-letter string representing the language code (e.g., `"en"` for English or `"fr"` for French). The default is `"en"`.
#'
#' @return A string containing the base URL of Investing.com for the specified language. If the language ID is not supported, the function raises an error.
#'
#' @details The function uses web scraping to fetch available language options from Investing.com. If the language code is provided as `"en"` or `"fr"`, the URLs are returned directly without scraping. For other languages, the function checks the Investing.com language selector.
#'
#' @author Koffi Frederic SESSIE
#'
#' @examples
#' # Get the URL in English (default)
#' INV_base_url("en")
#'
#' # Get the URL in French
#' INV_base_url("fr")
#'
#' @export

INV_base_url <- function(language_id = "en") {
  # Verifier que l'intitule de la langue est bien de deux caracteres
  if (nchar(language_id) != 2) {
    stop("The language identifier must be exactly two characters.")
  }

  # Si la langue par defaut est "en", retourner l'URL de base
  if (language_id == "en") {
    return("https://investing.com")
  } else if (language_id == "fr") {
    return("https://fr.investing.com")
  }

  # Definir l'URL de la page web
  start_url <- "https://investing.com"

  # Lire le contenu de la page web
  start_page <- rvest::read_html(start_url)

  # Les URLs selon la langue choisie
  Languages_hrefs <- start_page %>%
    rvest::html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
    rvest::html_attr("href")

  # Les textes des langues dans lesquelles le site est disponible
  Languages_texts <- start_page %>%
    rvest::html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
    rvest::html_text()

  # Extraire les identifiants de langue des URLs
  languages <- sub("^//([^.]+).*", "\\1", Languages_hrefs)

  # Creer un data frame avec les identifiants de langue, les URLs de base et les textes des langues
  Languages_df <- data.frame(Language_id = languages,
                             Base_url = paste0("https:", Languages_hrefs),
                             Language = Languages_texts,
                             stringsAsFactors = FALSE)

  # Chercher l'identifiant de langue dans le data frame
  result <- subset(Languages_df, Language_id == language_id)

  # Verifier si l'identifiant de langue existe
  if (nrow(result) == 0) {
    stop("The provided language identifier does not exist")
  } else {
    return(result$Base_url)
  }
}


# # Définir la fonction INV_base_url
# INV_base_url <- function(language_id = "en") {
#   # Vérifier que l'identifiant de langue est bien de deux caractères
#   if (nchar(language_id) != 2) {
#     stop("The language identifier must be exactly two characters.")
#   }
#
#   # Si la langue par défaut est "en", retourner l'URL de base
#   if (language_id == "en") {
#     return("https://investing.com")
#   } else if (language_id == "fr") {
#     return("https://fr.investing.com")
#   }
#
#   # Définir l'URL de la page web
#   start_url <- "https://investing.com"
#
#   # Lire le contenu de la page web
#   start_page <- read_html(start_url)
#
#   # Les URLs selon la langue choisie
#   Languages_hrefs <- start_page %>%
#     html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
#     html_attr("href")
#
#   # Les textes des langues dans lesquelles le site est disponible
#   Languages_texts <- start_page %>%
#     html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
#     html_text()
#
#   # extract_between_slashes_and_dot <- function(url) {
#   #   sub("^//([^.]+).*", "\\1", url)
#   # }
#
#   # Extraire les identifiants de langue des URLs
#   languages <- sub("^//([^.]+).*", "\\1", Languages_hrefs)
#
#   # Créer un data frame avec les identifiants de langue, les URLs de base et les textes des langues
#   Languages_df <- data.frame(Language_id = languages,
#                              Base_url = paste0("https:", Languages_hrefs),
#                              Language = Languages_texts,
#                              stringsAsFactors = FALSE)
#
#   # Chercher l'identifiant de langue dans le data frame
#   result <- subset(Languages_df, Language_id == language_id)
#   # result <- Languages_df %>% filter(Language_id == language_id)
#
#   # Vérifier si l'identifiant de langue existe
#   if (nrow(result) == 0) {
#     stop("The provided language identifier does not exist")
#   } else {
#     return(result$Base_url)
#   }
# }
