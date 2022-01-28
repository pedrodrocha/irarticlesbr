#' Download articles from Estudos Internacionais
#'
#' @param year Issue Year
#' @param volume Issue volume
#' @param number Issue Number
#' @param dir Directory for storing files
#' @param info_data Whether to return or not a tibble with info on collected pdfs
#'
#' @return Nothing. It just download files based on user input
#' @export
#'
#' @examples
#' download_moncoes(year = 2020, volume = 9, number = 17, dir = "/home/std_pedrorocha/Documentos/irarticlesbr/teste")
download_moncoes <- function(year, volume, number, dir, info_data = FALSE) {

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')

  url_archive <- "https://ojs.ufgd.edu.br/index.php/moncoes/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)

  xml2::read_html(url_archive) %>%
    rvest::html_nodes('.series') %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds

  xml2::read_html(url_archive) %>%
    rvest::html_nodes('.obj_issue_summary .title') %>%
    rvest::html_attr('href') -> primary_url




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
                    vol %in% volume) -> eds_url


  usethis::ui_done('Retrieving issues from archive and filtering based on your input')


  #/ Part II: Retrieve Pdf links
  usethis::ui_todo('Crawling pdfs for download')
  pdfs <- purrr::map_dfr(eds_url$url, function(x) {
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.pdf') %>%
      rvest::html_attr('href') %>%
      stringr::str_replace(.,"view","download") -> href
    url_lido %>%
      rvest::html_nodes('h1') %>%
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
