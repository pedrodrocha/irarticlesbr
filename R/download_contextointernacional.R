#' Title
#'
#' @param year
#' @param volume
#' @param number
#' @param dir
#' @param info_data
#'
#' @return
#' @export
#'
#' @examples
download_contextointernacional <- function(year, volume, number, dir,  info_data = FALSE){

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')

  url_archive <- "http://www.scielo.br/j/cint/grid"

  xml2::read_html(url_archive) %>%
    rvest::html_nodes(".left .btn") %>%
    rvest::html_attr("href") %>%
    paste0("http://www.scielo.br",.) %>%
    Filter(x = ., f = function(x) { stringr::str_detect(x, "v[0-9]{2}n") & !stringr::str_detect(x, "goto=previous")})  -> primary_url

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody td:nth-child(1)") %>%
    rvest::html_text() -> anos

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody th") %>%
    rvest::html_text() %>%
    stringr::str_squish() -> volumes


  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody .left") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(.," ", ",") -> numeros


  tibble::tibble(
    ano  = anos,
    vol = volumes,
    numero = numeros
  )  %>%
    tidyr::separate_rows(numero, sep = ",") -> ano_vol_year

  tibble::tibble(
    url = primary_url
  ) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(ano_vol_year) %>%
    dplyr::filter(ano %in% year & numero %in% number & vol %in% volume) -> eds_url

  #/ Part II: Retrieve Pdf links
  usethis::ui_todo('Crawling pdfs for download')

  pdfs <- tibble:::tibble()

  for(i in seq_along(eds_url$url)) {
    url_lido <- xml2::read_html(eds_url$url[i])

    url_lido %>%
      rvest::html_nodes(".links a") %>%
      rvest::html_attr("href") %>%
      Filter(x = .,f = function(x) stringr::str_detect(x,"(.pdf)"))  %>%
      paste0("http://www.scielo.br",.) -> href

    pdfs <- tibble::tibble(
      url = href, vol = eds_url$vol[i], n = eds_url$numero[i], ano = eds_url$ano[i]
    ) %>%
      dplyr::bind_rows(.,pdfs)

  }

  #/ Part III Downloading


  usethis::ui_todo('Downloading articles')

  if(isTRUE(info_data)){

    dat <- purrr::imap_dfr(pdfs$url, function(x, .y) {

      loc_arquivo <- paste0(dir,"/", pdfs$ano[.y], "-", pdfs$vol[.y],"-",pdfs$n[.y],"-",ifelse(.y < 10, paste0("0",.y) , .y),".pdf")


      curl::curl_download(
        x,
        destfile = loc_arquivo
      )

      tibble::tibble(loc_arquivo = loc_arquivo, pdf_url = x, size = pdf_size(url = x))
    })

    return(dat)

  } else {
    purrr::imap(pdfs$url, function(x, .y) {

      path_file <- paste0(dir,"/", pdfs$ano[.y], "-", pdfs$vol[.y],"-",pdfs$n[.y],"-",ifelse(.y < 10, paste0("0",.y) , .y),".pdf")

      curl::curl_download(x, destfile = path_file)
    })
  }

  usethis::ui_done('Downloading articles')


}
