#' Download articles from Estudos Internacionais
#'
#' @param year Issue Year
#' @param volume Issue volume
#' @param number Issue Number
#' @param dir Directory for storing files
#'
#' @return Nothing. It just download files based on user input
#' @export
#'
#' @examples
#' download_moncoes(year = 2020, volume = 9, number = 17, dir = "/home/std_pedrorocha/Documentos/irarticlesbr/teste")
download_moncoes <- function(year, volume, number, dir) {
  url_archive <- "http://periodicos.pucminas.br/index.php/estudosinternacionais/issue/archive"

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')

  url_archive <- "http://ojs.ufgd.edu.br/index.php/moncoes/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)

  url_archive_lido %>%
    rvest::html_nodes("h4") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") -> primary_url


  url_archive_lido %>%
    rvest::html_nodes("h4") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text() %>%
    stringr::str_remove(":(.*)") -> eds


  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.),
      ano = dplyr::case_when(

        vol == 4 ~ 2015,
        vol == 3 ~ 2014,
        vol == 2 ~ 2013,
        vol == 1 ~ 2012,
        TRUE ~ ano
      ),
      editions = paste0("v. ",vol," n. ",n," (", ano,")"),
      url = paste0(url, "/showToc")
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume)-> eds_url


  usethis::ui_done('Retrieving issues from archive and filtering based on your input')


  #/ Part II: Retrieve Pdf links
  usethis::ui_todo('Crawling pdfs for download')
  pdfs <- purrr::map_dfr(eds_url$url, function(x) {
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.file') %>%
      rvest::html_attr('href') %>%
      stringr::str_replace(.,"view","download") %>%
      stringr::str_replace(.,'downloadIssue','download')-> href

    url_lido %>%
      rvest::html_nodes('h2') %>%
      rvest::html_text() -> ed

    pdf_url <- tibble::tibble(url = href, ed = ed)

    return(pdf_url)

  }) %>%
    dplyr::mutate(
      vol = stringr::str_extract(ed, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(ed,'(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ','') %>%
        as.integer(.),
      ano = stringr::str_extract(ed,"[0-9]{4}") %>%
        as.double(.),
      ano = dplyr::case_when(

        vol == 4 ~ 2015,
        vol == 3 ~ 2014,
        vol == 2 ~ 2013,
        vol == 1 ~ 2012,
        TRUE ~ ano
      )
    )

  usethis::ui_done('Crawling pdfs for download')



  #/ Part III Downloading

  usethis::ui_todo('Downloading articles')

  purrr::imap(pdfs$url, function(x, .y) {

    curl::curl_download(
      x,
      destfile = paste0
      (
        dir,
        "/",
        pdfs$ano[.y],
        "-",
        pdfs$vol[.y],
        "-",
        pdfs$n[.y],
        "-",
        ifelse(.y < 10, paste0("0",.y) , .y),
        ".pdf")
    )
  })

  usethis::ui_done('Downloading articles')


}
