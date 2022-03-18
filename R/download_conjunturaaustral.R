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
download_conjunturaaustral <- function(year, volume, number, dir, info_data = FALSE) {

  #/ Part 0: Solving issue number problem

  tibble::tibble(number = as.character(number)) %>%
    dplyr::mutate(number = dplyr::case_when(
      number %in% c("39","40") ~ "39-40",
      number %in% c("33","34") ~ "33-34",
      number %in% c("27","28") ~ "27-28",
      number %in% c("21","22") ~ "21-22",
      number %in% c("15","16") ~ "15-16",
      number %in% c("9","10") ~ "9-10",
      number %in% c("3","4") ~ "3-4",
      TRUE ~ number
    )
    ) %>%
    dplyr::distinct() %>%
    dplyr::pull(number) -> number

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')

  base <- "https://seer.ufrgs.br/index.php/ConjunturaAustral/issue/archive/"
  url_archive <- stringr::str_c(base,c('1','2','3','4'))


  grab_ed_links <- function(archive){

    url_lido <- xml2::read_html(archive)

    url_lido %>%
      rvest::html_nodes('.title') %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t") -> eds

    url_lido %>%
      rvest::html_nodes('.title') %>%
      rvest::html_attr('href') -> primary_url

    tibble::tibble(url = primary_url,
                   editions = eds)

  }



  purrr::map_df(url_archive, grab_ed_links) %>%
    dplyr::mutate(
      editions = dplyr::case_when(
        stringr::str_detect(editions,"O SUL GLOBAL PENSADO") ~ "v. 12 n. 59 (2021)",
        stringr::str_detect(editions,"Dez anos de Conjuntura Austral") ~ "v. 11 n. 55 (2020)",
        stringr::str_detect(editions,"Especial Diplomacia") ~ "v. 11 n. 54 (2020)",
        stringr::str_detect(editions,"Especial BRICS") ~ "v. 11 n. 53 (2020)",
        TRUE ~ editions
      )
    ) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.)
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
      n = stringr::str_extract(ed,'(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
      ano = stringr::str_extract(ed,"[0-9]{4}") %>%
        as.double(.)
    ) %>%
    dplyr::filter(!stringr::str_detect(url,"issue"))



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
