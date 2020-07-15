# code works manually but not when performed in autmated testing

testthat::test_that(
  desc = "add_multi_cols_1",
  code = {
    data("doc_html")
    sivis <- new.env(parent = emptyenv())
    xpath <- "/html/body/table/tr/td[1]"
    sivis <- add_multi_cols(XPathes = xpath, response_string = doc_html, extract_pathes = list(), search_multi_cols = TRUE, page_url = "")
    res <- all(
      sivis$root_xpath == "/html/body/table/tr",
      sivis$more_cols %>% grepl(pattern = "shinyApp") %>% sum
    )
    testthat::expect_true(res)
  }
)
