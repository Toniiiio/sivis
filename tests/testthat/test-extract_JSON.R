expected_result <- list(
  all_found = FALSE,
  extract_pathes = list(json = list(
    reponse = "json",
    is_large_html = FALSE, neighbours = list(price = "69"), texts = "69",
    target_key = "price", base = NULL, base_follow = NULL)
  ),
  result_values = "69"
)

testthat::test_that(
  desc = "extract_JSON_1",
  code = {
    test <- json <- '{"price": "69"}'
    target_values <- "69"
    res <- extract_JSON(response_string = json, target_values)
    testthat::expect_true(identical(res, expected_result))
  }
)
