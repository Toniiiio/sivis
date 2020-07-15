#devtools::test(filter = "JSON")
# setwd("tests/testthat/")

# httpshanescareerscomsearch.RData
# httpscareercelaneseicimscomjobssearchss1searchKeywordin_iframe1.RData
fileName <-  "httpscareersberkleyicimscomjobssearchss1in_iframe1.RData"


## id ## fileName ## docType ## extractResult
source("../../R/sivis.R")
load("unitMeta.RData")

filterHTMLIdx <- which(unitMeta$docType1 == "text/html")
fileNamesHTML <- unitMeta$fileNames[filterHTMLIdx]

HTMLExtract <- unitMeta$extractResult1[filterHTMLIdx]
names(HTMLExtract) <- fileNamesHTML

# fileName  <- fileNamesHTML[1]
fileName <- "httpscareercelaneseicimscomjobssearchss1searchKeywordin_iframe1.RData"
for(fileName in fileNamesHTML){
  print(fileName)
  load(file = paste0("../../R/fromWeb/wrongResult/", fileName))
  targetValues <- sivis$cbData$clipBoardText$selectedText
  responseString <- sivis$GETContents[[1]] %>% httr::content(type = "text")

  testthat::test_that(
    desc = paste0(fileName, " extractHTML"),
    code = {
      testthat::expect_true(identical(HTMLExtract[[fileName]], extract_HTML(responseString, targetValues, extractPathes = list())))
    }
  )
  # extract_HTML(responseString, targetValues, extractPathes = list())
  # HTMLExtract[[fileName]]
}

HTMLExtract[[fileName]]
extract_HTML(responseString, targetValues, extractPathes = list())
