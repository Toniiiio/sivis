#setwd("tests/testthat/")
#setwd("../..")

# to debug use:
#browser(), see here: https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio.

# Loop over files?
#list.files(path = "../../R/fromWeb/", pattern = "*.RData") %>% print

fileNames <- list.files(path = "../../R/fromWeb/", pattern = "*.RData")
for(fileName in fileNames){
  test_that(
    desc = paste0(fileName, " works"),
    code = {
      source("../../R/sivis.R")
      load(file = paste0("../../R/fromWeb/", fileName))
      expect_true(object = use_sivis(sivis))
    }
  )
}

#
# xx
# test_that("activision works", {
#   source("../../R/sivis.R")
#   fl <- "httpscareersactivisioncomsearchresults.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
# test_that("intuitlong works", {
#   source("../../R/sivis.R")
#   fl <- "httpscareersintuitivecomapijobspage2internalfalseuserIdc584a5e0ba114d19b2e225622b9e90e7sessionId773f7.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
#
# test_that("intuit works", {
#   source("../../R/sivis.R")
#   fl <- "httpscareersintuitcomjobsearch.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
# test_that("phillips works", {
#   source("../../R/sivis.R")
#   fl <- "httpsphillips66jobsjobs.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
# test_that("rbloggers works", {
#   source("../../R/sivis.R")
#   fl <- "httpswwwrbloggerscom.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
# test_that("biogen works", {
#   source("../../R/sivis.R")
#   fl <- "httpswwwbiogencombindxpcareersearchsearchKeycategorynulllocationSwitzerlandZug20Headquartersregionnul.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
#
# test_that("lockheedmartin works", {
#   source("../../R/sivis.R")
#   fl <- "httpswwwlockheedmartinjobscomsearchjobs.RData"
#   load(file = paste0("../../R/fromWeb/", fl))
#   expect_true(object = use_sivis(sivis))
# })
