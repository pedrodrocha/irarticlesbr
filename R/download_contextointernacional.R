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
download_contextointernacional <- function(year, volume, number, dir,  info_data = FALSE) {

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')

  url_archive <- "https://www.scielo.br/scielo.php?script=sci_issues&pid=0102-8529&lng=en&nrm=iso"

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("b a") %>%
    rvest::html_attr("href")  -> primary_url

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE) %>%
    .[[5]] %>%
    dplyr::select(1:5) %>%
    dplyr::slice(-1) %>%
    tidyr::pivot_longer(cols = 3:5,
                        names_to = "X",
                        values_to = "numero",
                        values_drop_na = TRUE) %>%
    dplyr::select(-X) %>%
    dplyr::filter(numero %in% c("1","2","3")) %>%
    dplyr::rename(ano = "X1",
                  vol = "X2") %>%
    dplyr::mutate(ano = as.numeric(ano),
                  vol = as.numeric(vol),
                  numero = as.numeric(dplyr::if_else(numero == "special issue","3", numero))) %>%
    dplyr::mutate(url = primary_url) %>%
    dplyr::filter(ano %in% year &
                    numero %in% number &
                    vol %in% volume) -> eds_url


  usethis::ui_done('Retrieving issues from archive and filtering based on your input')

  #/ Part II: Retrieve Pdf links
  usethis::ui_todo('Crawling pdfs for download')

  pdfs <- purrr::map_dfr(eds_url$url, function(x) {
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes(".content div a") %>%
      rvest::html_attr("href") %>%
      Filter(x = .,f = function(x) stringr::str_detect(x,"(.pdf)")) %>%
      paste0('https://www.scielo.br/',.) -> href

    url_lido %>%
      rvest::html_nodes('p br+ font') %>%
      rvest::html_text() -> texto_ed

    stringr::str_extract(texto_ed,"(vol.[0-9]{2})|(vol.[0-9]{1})") -> vol
    stringr::str_extract(texto_ed, "no.[0-9]{1}") -> num
    stringr::str_extract(texto_ed, "[0-9]{4}") -> a

    pdf_url <- tibble::tibble(url = href, ed = paste(vol, num, a))


    return(pdf_url)


  }) %>% dplyr::mutate(
    vol = stringr::str_extract(ed, "(vol.[0-9]{2})|(vol.[0-9]{1})") %>%
      stringr::str_remove_all(., ' '),
    n = stringr::str_extract(ed,'no.[0-9]{1}')  %>%
      stringr::str_remove_all(., ' '),
    ano = stringr::str_extract(ed,"[0-9]{4}")
  )


  usethis::ui_done('Crawling pdfs for download')

  #/ Part III Downloading


  usethis::ui_todo('Downloading articles')

  if(isTRUE(info_data)){

    dat <- purrr::imap_dfr(pdfs$url, function(x, .y) {

      loc_arquivo <- paste0(dir,"/", pdfs$ano[.y], "-", pdfs$vol[.y],"-",pdfs$n[.y],"-",ifelse(.y < 10, paste0("0",.y) , .y),".pdf")


      curl::curl_download(
        x,
        destfile = loc_arquivo
      )

      tibble::tibble(loc_arquivo = loc_arquivo, pdf_url = x, size = pdf_size(x))
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
