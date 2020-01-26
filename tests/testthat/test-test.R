# setwd("tests/testthat/")
#setwd("../..")

# to debug use:
#browser(), see here: https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio.


fileNames <- list.files(path = "../../R/fromWeb/", pattern = "*.RData")
# fileName <- fileNames[1]

for(fileName in fileNames){
  print(fileName)
  test_that(
    desc = paste0(fileName, " works"),
    code = {
      source("../../R/sivis.R")
      load(file = paste0("../../R/fromWeb/", fileName))
      expect_true(object = use_sivis(sivis))
    }
  )
}
