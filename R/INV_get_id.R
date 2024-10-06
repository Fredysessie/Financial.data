INV_get_id <- function(info, language="en", exchange = NULL) {

  elem_info = INV_get_info(info = info,
                           language= language,
                           exchange = exchange)

  if (is.null(dim(elem_info))) {
    stop(glue("{info} does not exist. Please check the information provided."))
  }else{
    the_id = elem_info$id
    return(the_id)
  }

}
