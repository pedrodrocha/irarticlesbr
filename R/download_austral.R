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
download_austral <- function(year, volume, number, dir,  info_data = FALSE) {

  url_archive <- "https://seer.ufrgs.br/austral/issue/archive"

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')


  url_archive_lido <- xml2::read_html(url_archive)

  url_archive_lido %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_attr("href") -> primary_url


  url_archive_lido %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_text() %>%
    stringr::str_remove(":(.*)") -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(Vol [0-9]{2})|(Vol [0-9]{1})") %>%
        stringr::str_replace_all(.,'Vol ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(No [0-9]{2})|(No [0-9]{1})') %>%
        stringr::str_replace_all(.,'No ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.),
      url = paste0(url, "/showToc")
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) -> eds_url



  usethis::ui_done('Retrieving issues from archive and filtering based on your input')

  #/ Part II: Retrieve Pdf links
  usethis::ui_todo('Crawling pdfs for download')
  pdfs <- purrr::map_dfr(eds_url$url, function(x) {
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.tocGalleys a') %>%
      rvest::html_attr('href') %>%
      stringr::str_replace(.,"view","download") %>%
      stringr::str_replace(.,'downloadIssue','download')-> href

    url_lido %>%
      rvest::html_nodes('#breadcrumb .current') %>%
      rvest::html_text() -> ed

    pdf_url <- tibble::tibble(url = href, ed = ed)


    return(pdf_url)

  }) %>%
    dplyr::mutate(
      vol = stringr::str_extract(ed, "(Vol [0-9]{2})|(Vol [0-9]{1})") %>%
        stringr::str_replace_all(.,'Vol ','') %>%
        as.integer(.),
      n = stringr::str_extract(ed,'(No [0-9]{2})|(No [0-9]{1})') %>%
        stringr::str_replace_all(.,'No ','') %>%
        as.integer(.),
      ano = stringr::str_extract(ed,"[0-9]{4}") %>%
        as.double(.)
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
