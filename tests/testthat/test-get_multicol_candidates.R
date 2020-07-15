testthat::test_that(
  desc = "get_multicol_candidates_1",
  code = {
    data("doc_html")
    doc <- xml2::read_html(doc_html)
    xpath <- "/html/body/table/tr/td[1]"
    tags <- rvest::html_nodes(doc, xpath = xpath)
    result <- get_multicol_candidates(doc_html, xpath, list())
    res <- all(
      result$rootXPath == "/html/body/table/tr",
      result$multicol_filtOutput$`td[1]` == c("Target1", "Target2"),
      result$multicol_filtOutput$`td[2]` == c("Neighbour1", "Neighbour2")
    )

    testthat::expect_true(res)
  }
)
