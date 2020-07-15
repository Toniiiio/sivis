doc_script_json <- 'var doc = {target: "target_text"}'
doc_script_jsons <- 'var doc = {target: "target_text"}; var doc2 = {target: "other text"}'
expected_result <- list(jsons = structure(list(jsons = "{target: \"target_text\"}",
                                               match = TRUE, index = 1L), row.names = c(NA, -1L), class = "data.frame"))

json_by_tv <- json_from_string(doc_script_json, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = "target_text", index_nr = NULL)
jsons_by_tv <- json_from_string(doc_script_json, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = "target_text", index_nr = NULL)
jsons_by_index <- json_from_string(doc_script_jsons, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = NULL, index_nr = 1)

testthat::test_that(
  desc = paste0("json_from_string"),
  code = {
    testthat::expect_identical(json_by_tv, expected_result)
    testthat::expect_identical(jsons_by_tv, expected_result)
    testthat::expect_identical(jsons_by_index$jsons, expected_result$jsons$jsons)
  }
)
