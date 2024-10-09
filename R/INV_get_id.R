#' Retrieve the Investing.com ID for a Financial Asset
#'
#' This function retrieves the Investing.com ID of a specified financial asset based on the provided search term and optional exchange.
#'
#' @param info Character. The search term or financial asset (e.g., stock symbol) to query. Must be a string.
#' @param language Character. The language for the search results. Defaults to "en" for English. Other languages can be passed (e.g., "fr" for French).
#' @param exchange Character. Optional. The stock exchange to filter results by. If specified, the results will be filtered to match the provided exchange name.
#'
#' @return A character vector representing the ID of the financial asset from Investing.com. If the asset does not exist, an error is thrown.
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
#' # Retrieve the ID of "aaple" (Apple Inc.)
#' INV_get_id("aaple")
#'
#' # Retrieve the ID of 'SNTS' from BRVM exchange
#' INV_get_id("SNTS", exchange = "BRVM")
#'
#' @seealso \code{\link{INV_get_info}} for retrieving more detailed information about the asset.
#'
#' @export

INV_get_id <- function(info, language="en", exchange = NULL) {

  elem_info = INV_get_info(info = info,
                           language= language,
                           exchange = exchange)

  if (is.null(dim(elem_info))) {
    stop(glue::glue("{info} does not exist. Please check the information provided."))
  }else{
    the_id = elem_info$id
    return(the_id)
  }

}
