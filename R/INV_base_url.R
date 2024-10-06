# Définir la fonction INV_base_url
INV_base_url <- function(language_id = "en") {
  # Vérifier que l'identifiant de langue est bien de deux caractères
  if (nchar(language_id) != 2) {
    stop("L'intitulé de la langue doit être exactement deux caractères.")
  }

  # Si la langue par défaut est "en", retourner l'URL de base
  if (language_id == "en") {
    return("https://investing.com")
  } else if (language_id == "fr") {
    return("https://fr.investing.com")
  }

  # Définir l'URL de la page web
  start_url <- "https://investing.com"

  # Lire le contenu de la page web
  start_page <- read_html(start_url)

  # Les URLs selon la langue choisie
  Languages_hrefs <- start_page %>%
    html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
    html_attr("href")

  # Les textes des langues dans lesquelles le site est disponible
  Languages_texts <- start_page %>%
    html_nodes(css = '#__next > header > div.flex.justify-center.xxl\\:px-\\[160px\\].xxxl\\:px-\\[300px\\].header_top-row-wrapper__7SAiJ > section > div.EditionSelector_editions__ayDVY > div > div > ul a') %>%
    html_text()

  # extract_between_slashes_and_dot <- function(url) {
  #   sub("^//([^.]+).*", "\\1", url)
  # }

  # Extraire les identifiants de langue des URLs
  languages <- sub("^//([^.]+).*", "\\1", Languages_hrefs)

  # Créer un data frame avec les identifiants de langue, les URLs de base et les textes des langues
  Languages_df <- data.frame(Language_id = languages,
                             Base_url = paste0("https:", Languages_hrefs),
                             Language = Languages_texts,
                             stringsAsFactors = FALSE)

  # # Ou remplir directement la dataframe
  # Languages_df = structure(list(Language_id = c("uk", "in", "ca", "au", "za", "ph", "ng", "de", "es", "mx", "fr", "it", "nl", "pt", "pl", "br", "ru", "tr", "sa", "gr", "se", "fi", "il", "jp", "kr", "cn", "hk", "id", "ms", "th", "vn", "hi"),
  #                               Base_url = c("//uk.investing.com", "//in.investing.com", "//ca.investing.com", "//au.investing.com", "//za.investing.com", "//ph.investing.com", "//ng.investing.com", "//de.investing.com", "//es.investing.com", "//mx.investing.com",
  #                                            "//fr.investing.com", "//it.investing.com", "//nl.investing.com", "//pt.investing.com", "//pl.investing.com", "//br.investing.com", "//ru.investing.com", "//tr.investing.com", "//sa.investing.com", "//gr.investing.com", "//se.investing.com", "//fi.investing.com",
  #                                            "//il.investing.com", "//jp.investing.com", "//kr.investing.com", "//cn.investing.com", "//hk.investing.com", "//id.investing.com", "//ms.investing.com", "//th.investing.com", "//vn.investing.com", "//hi.investing.com"),
  #                               Language = c("English (UK)", "English (India)", "English (Canada)", "English (Australia)", "English (South Africa)", "English (Philippines)", "English (Nigeria)", "Deutsch", "Español (España)", "Español (México)", "Français",
  #                                            "Italiano", "Nederlands", "Português (Portugal)", "Polski", "Português (Brasil)", "Русский", "Türkçe", "‏العربية‏", "Ελληνικά", "Svenska", "Suomi", "עברית", "日本語", "한국어", "简体中文", "繁體中文", "Bahasa Indonesia",
  #                                            "Bahasa Melayu", "ไทย", "Tiếng Việt", "हिंदी")
  #                               ),
  #                          class = "data.frame",
  #                          row.names = c(NA, -32L))


  # Chercher l'identifiant de langue dans le data frame
  result <- subset(Languages_df, Language_id == language_id)
  # result <- Languages_df %>% filter(Language_id == language_id)

  # Vérifier si l'identifiant de langue existe
  if (nrow(result) == 0) {
    stop("L'identifiant de langue n'existe pas.")
  } else {
    return(result$Base_url)
  }
}
