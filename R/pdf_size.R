#' Title
#'
#' @param url
#'
#' @return
#'
#'
#' @examples
pdf_size <- function(url) {

  as.numeric(httr::HEAD(url)$headers$`content-length`)
}
