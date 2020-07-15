

source("../../R/sivis.R")
fileNames <- list.files(path = "../../R/fromWeb/", pattern = "*.RData")

unitMeta <-list(
  id = paste0("doc_", 1:length(fileNames)),
  fileNames = fileNames,
  docType1 = sapply(types, "[[", "type", USE.NAMES = FALSE) %>% unname,
  extractResult1 = list()
)

docTypes <- c("application/json", "text/html")
load("types.RData")
load("HTMLExtract.RData")

load("unitMeta.RData")
docType <- docTypes[2]
for(docType in DocTypes){
  hasdocType <- sapply(types, "[[", "type", USE.NAMES = FALSE) %>%
    unname %>%
    magrittr::equals(docType) %>%
    which
  fileNamesdocType <- fileNames[hasdocType]
}

nrr <- 0
docNr <- sapply(names(HTMLExtract), FUN = function(x) which(x == unitMeta$fileNames), USE.NAMES = FALSE)
for(nr in docNr){
  nrr <- nrr + 1
  unitMeta$extractResult1[[nr]] <- HTMLExtract[[nrr]]
}
save(unitMeta, file = "unitMeta.RData")

ff <- unitMeta$extractResult1 %>% lengths %>% equals(0)
unitMeta[[3]][ff]

docTypeExtract <- list()
for(fileName in fileNamesdocType){
  print(fileName)
  load(file = paste0("../../R/fromWeb/", fileName))
  targetValues <- sivis$cbData$clipBoardText$selectedText
  responseString <- sivis$GETContents[[1]] %>% content(type = "text")

  docTypeExtract[[fileName]] <-  extract_JSON(responseString, targetValues, extractPathes = list())
}
save(JSONExtract, file = "JSONExtract.RData")

load("JSONExtract.RData")






load("unitMeta.RData")

filterHTMLIdx <- which(unitMeta$docType1 == "text/html")
fileNamesHTML <- unitMeta$fileNames[filterHTMLIdx]

HTMLExtract <- list()
names(HTMLExtract) <- fileNamesHTML

fileName  <- fileNamesHTML[1]
for(fileName in fileNamesHTML){
  print(fileName)
  load(file = paste0("../../R/fromWeb/", fileName))
  targetValues <- sivis$cbData$clipBoardText$selectedText
  responseString <- sivis$GETContents[[1]] %>% httr::content(type = "text")
  HTMLExtract[[fileName]] <- extract_HTML(responseString, targetValues, extractPathes = list())
}

for(nrr in 1:length(filterHTMLIdx)){
  unitMeta$extractResult1[filterHTMLIdx[nrr]] <- HTMLExtract[nrr]
}
save(unitMeta, file = "unitMeta.RData")
