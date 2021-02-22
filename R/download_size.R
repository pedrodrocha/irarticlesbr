#' Title
#'
#' @param url
#'
#' @return
#'
#'
#' @examples
download_size <- function(url) {

  as.numeric(httr::HEAD(url)$headers$`content-length`)
}
