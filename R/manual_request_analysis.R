#' Show response string as document in browser
#'
#' Display html files that are in string format are dispayed in the RStudio viewer or in the browser.
#'
#' @param doc The string that contains the content to be shown in the browser
#' @param fileExt The file extension that should be used to save the string to disk
#' @inheritParams utils::browseURL
#'
#' @examples
#' data("doc_html")
#' show_html_page(doc_html)
show_html_page <- function(doc, encode = "UTF-8", fileExt = ".html", browser = rstudioapi::viewer, encodeIfNeeded  = FALSE){
  tmp <- tempfile(fileext = fileExt)
  doc %>% toString(encode = encode) %>% writeLines(con = tmp)
  tmp %>% browseURL(browser, encodeIfNeeded)
}

# doc <- xml2::read_html(x = "https://www.handelsblatt.com")
# doc %>% show_html_page()

get_req_hdr <- function(headersRaw){
  headerValueLabel <- (headersRaw) %>%
    strsplit(split = "\n") %>%
    lapply(strsplit, split = ": ") %>%
    unlist(recursive = FALSE)

  header <- sapply(headerValueLabel, "[", 2)
  names(header) <- sapply(headerValueLabel, "[", 1)

  header[is.na(header)] <- ""
  return(header)
}

get_req_body <- function(body_raw){
  body_val_label <- (body_raw) %>%
    strsplit(split = "\n") %>%
    lapply(strsplit, split = ": ") %>%
    unlist(recursive = FALSE)

  body <- lapply(body_val_label, "[", 2)
  names(body) <- sapply(body_val_label, "[", 1)

  #header[is.na(body)] <- ""
  return(body)
}
# sivis$request_body %>% URLdecode() %>% strsplit(split = "&")

OneTimeScrape <- function(){
  splitted <- strsplit(
    x = readClipboard(),
    split = ";"
  )[[1]]

  browser_output <- list(
    url = tolower(splitted[1]),
    click_type = tolower(splitted[2]),
    expectedOutput = tolower(strsplit(splitted[3], split = "~~~")[[1]]),
    links = tolower(strsplit(splitted[4], split = "~~~")[[1]]),
    XPath = tolower(splitted[5]),
    XPathBlank = gsub(
      pattern = "[[]\\d+[]]",
      replacement = "",
      x = tolower(splitted[5])
    )
  )
  assign("scrapedData", browser_output$expectedOutput, envir = .GlobalEnv)
}
