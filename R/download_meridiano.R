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
download_meridiano <- function(year, volume, number, dir, info_data = FALSE) {


  #/ Part 0: Solving issue number problem

  tibble::tibble(number = as.character(number)) %>%
    dplyr::mutate(number = dplyr::case_when(
      number %in% c("71","72") ~ "71-72",
      number %in% c("52","53") ~ "52-53",
      number %in% c("50","51") ~ "50-51",
      number %in% c("44","45") ~ "44-45",
      number %in% c("42","43") ~ "42-43",
      number %in% c("40","41") ~ "40-41",
      number %in% c("38","39") ~ "38-39",
      number %in% c("36","37") ~ "36-37",
      number %in% c("34","35") ~ "34-35",
      number %in% c("32","33") ~ "32-33",
      number %in% c("4","5") ~ "4-5",
      number %in% c("20","21") ~ "20-21",
      number %in% c("23","24") ~ "23-24",
      number %in% c("8","9") ~ "8-9",
      number %in% c("12","10") ~ "10-12",
      number %in% c("14","15") ~ "14-15",
      number %in% c("29","29") ~ "28-29",
      number %in% c("30","31") ~ "30-31",
      TRUE ~ number
    )
    ) %>%
    dplyr::distinct() %>%
    dplyr::pull(number) -> number

  #/ Part I: Retrieve Issues From Archive and Filter from User Input
  usethis::ui_todo('Retrieving issues from archive and filtering based on your input')


  url_archive <- "https://periodicos.unb.br/index.php/MED/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)
  url_archive_lido %>%
    rvest::html_nodes("#main-content .title") %>%
    rvest::html_attr("href")  -> primary_url



  url_archive_lido %>%
    rvest::html_nodes(".lead") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") %>%
    stringr::str_replace("\\\\", "-") -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{3})|(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.),
      n = ifelse(ano > 2015, 0,n)
    )  %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume)  -> eds_url




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
      rvest::html_node('.active') %>%
      rvest::html_text() %>%
      stringr::str_remove_all(.,"(\\n)|(\\t)") -> ed

    pdf_url <- tibble::tibble(url = href, ed = ed)


    return(pdf_url)


  }) %>% dplyr::mutate(
    vol = stringr::str_extract(ed, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
      stringr::str_remove_all(., ' '),
    n = stringr::str_extract(ed,'(n. [0-9]{3})|(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})')  %>%
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
