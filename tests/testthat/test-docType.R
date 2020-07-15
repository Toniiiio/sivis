# setwd("tests/testthat/")
print("go")
source("../../R/sivis.R")
fileNames <- list.files(path = "../../R/fromWeb/", pattern = "*.RData")
fileName <- fileNames[95]

# types <- list()
# for(fileName in fileNames[1:length(fileNames)]){ #
#   print(fileName)
#   load(file = paste0("../../R/fromWeb/", fileName))
#   targetValues <- sivis$cbData$clipBoardText$selectedText
#   responseString <- sivis$GETContents[[1]] %>% content(type = "text")
#   types[[fileName]] <- findDocType(responseString, targetValues)
#
#   save(types, file = "types.RData")
# }
#

# Types:
# "text/html"
# "unknown type"
# "application/json"
# "script/json"


load("types.RData")
for(fileName in fileNames[1:length(fileNames)]){
  print(fileName)
  load(file = paste0("../../R/fromWeb/", fileName))
  targetValues <- sivis$cbData$clipBoardText$selectedText
  responseString <- sivis$GETContents[[1]] %>% content(type = "text")

  testthat::test_that(
    desc = paste0(fileName, " docType"),
    code = {
      test <- identical(types[[fileName]], findDocType(responseString, targetValues))
      testthat::expect_true(test)
    }
  )
}



