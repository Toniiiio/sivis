# setwd("tests/testthat/")
#setwd("../..")

# to debug use:
#browser(), see here: https://stackoverflow.com/questions/31548796/debugging-testthat-tests-in-rstudio.

# url <- "https://stackoverflow.com"
# f <- function(url){
#   url %>% httr::GET() %>% httr::status_code() %>% magrittr::equals(200)
# }

#httpscareerbelufthansacomglobaljobboard_apisearchdata7B22LanguageCode223A22DE222C22SearchParameters.RData

source("../../R/sivis.R")
fileNames <- list.files(path = "../../R/fromWeb/", pattern = "*.RData")
# fileName <- fileNames[1]

for(fileName in fileNames[121:170]){ #length(fileNames)
  print(fileName)
  testthat::test_that(
    desc = paste0(fileName, " full test"),
    code = {
      load(file = paste0("../../R/fromWeb/", fileName))
      testRun <- FALSE
      testEval <- TRUE
      expect_true(object = use_sivis(sivis, testRun = testRun, testEval = testEval))
    }
  )
}


#################   offline test ######

######restart sivisBrowser
#httpshenryscheintaleonetcareersectionhsi1moresearchajax.RData --> weird response initially


#httpsdeccepjobssearchjobsresultsActiveFacetID0CurrentPage2RecordsPerPage14Distance50RadiusUnitType0Ke.RData
#httpscareersunderarmourcomsearchjobsresultsActiveFacetID0CurrentPage2RecordsPerPage10Distance50Radius.RData
#httpscareerbelufthansacomglobaljobboard_apisearchdata7B22LanguageCode223A22DE222C22SearchParameters.RData
#httpswwwcapitalonecareerscomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50Radi.RData
#httpswwwattjobssearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50RadiusUnitType0Ke.RData

#timeout??
##"httpssjobsbrassringcomTgNewUISearchAjaxMatchedJobs.RData"
##"httpsrecruitingultiprocomROL1002ROLINJobBoardaeaa90be85cf4d60ab2b222c1c52621aJobBoardViewLoadSearchRe.RData"



# httpsjobsapiinternalmcloudioapijobcallbackjobsCallbackoffset49sortfieldopen_datesortorderdescendingfa.RData works
# lexical error: invalid char in json text.
# jobsCallback({"aggregations":nu

# error: httpscareerbelufthansacomglobaljobboard_apisearchdata7B22LanguageCode223A22DE222C22SearchParameters.RData works
# lexical error: invalid char in json text.
# jobsCallback({"aggregations":nu

# Lessons learned:
# - cant return easily from nth parent function, see example below.
# - pay attention to other scripts in R folder, they will be evaluated too.
# - cant pass environments across parent function (without hading over i guess) - why?
# - Fehler: "EXPR must be a length 1 vector" -->NULL in switch(NULL, no, yes)
# - is.function(f) is not TRUE ---> not is also part of "testthat", if usage then with magrittr::not

######################### FAIL for testing
# e <- function(){print(sys.nframe());do.call(return, list("a"), envir = sys.frame(1-sys.nframe()))}
# d <- function(){e();3}
# c <- function(){d();1}
# b <- function(){c();2}
# a <- function(){b()}
# # expect_true("a" == "a")
# x <- a()
# print(x)
# print("here")
# expect_true(x == "a")
