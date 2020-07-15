#devtools::test(filter = "JSON")
# setwd("tests/testthat/")


## id ## fileName ## docType ## extractResult
source("../../R/sivis.R")
load("unitMeta.RData")

# filterJSONIdx <- which(unitMeta$docType1 == "application/json")
# fileNamesJSON <- unitMeta$fileNames[filterJSONIdx]
#
# JSONExtract <- unitMeta$extractResult1[filterJSONIdx]
# names(JSONExtract) <- fileNamesJSON

# fileName  <- fileNamesJSON[1]
for(fileName in fileNamesJSON){
  print(fileName)
  load(file = paste0("../../R/fromWeb/", fileName))
  targetValues <- sivis$cbData$clipBoardText$selectedText
  responseString <- sivis$GETContents[[1]] %>% httr::content(type = "text")

  testthat::test_that(
    desc = paste0(fileName, " extractJSON"),
    code = {
      test <- identical(JSONExtract[[fileName]], extract_JSON(responseString, targetValues, extractPathes = list()))
      testthat::expect_true(test)
    }
  )
}
