# Notation:
# Initial scrape: Data collection for building the sivis scraping code.
# Scheduled scrape: Automatic scrape, that will be done based on the code created by sivis80.

## General challenges:
# Mixed sources: First page html, second page xhr request. Example: https://jobs.disneycareers.com/search-jobs


# todo: inpage source reparieren und von get und post untersuchungen trennen: {"identifier":"SivisCBDataIdent","pageUrl":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","clickType":"contextmenu","selectedText":["\n            Mcclean, VA, US\n            \n        "],"links":[null],"XPath":"/html/body/div[2]/div[2]/div/div[4]/table/tbody/tr[1]/td[3]/span","XPathClassRaw":"/html[class = 'html5']/body[class = 'coreCSB search-page body']/div[class = 'outerShell']/div[class = 'innerShell']/div[class = 'content']/div[class = 'searchResultsShell']/table[class = 'searchResults full']/tbody/tr[class = 'dbOutputRow2 jobgrid-row']/td[class = 'colLocation']/span[class = 'jobLocation']","getUrl":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","postUrl":"","postBody":{"raw":"[object Object]"},"InPageSource":true}
# todo: https://www.clariant.com/de/Careers/Job-Openings/Global-Job-Openings
# todo: wrong get url for volkswagen
# todo: https://www.dmk.de/karriere/stellenangebote/?no_cache=1 --> mixed sources. First ones in document next in get requests.
# todo: Shiny hat krasse doppelungen, zb auc hbei volkswagen
# todo: Daimler geht net, findet den request net.
# todo: allianz failed: IST EIN GET, aber sivis findet ihn net - xx <- "https://jobs.allianz.com/sap/hcmx/hitlist_na?sap-client=100&sap-language=de" %>% GET - x <- POST(url = "https://careers.allianz.com/etc/clientlibs/onemarketing/career/paths/jobsearch.json", encode = "json", body = list(formData = list(cityID = "", companyID = "", countryID = "32760000",     functionalAreaID = "", hierarchyLevelID = "", language = "de_DE",     orderBy = "desc", resultSet = "9", searchLimit = "9")))
# todo: Linde multi rvest add xpathes und page change
#todo: sivis - cant click specific element
#todo: sivis: inpagesource falsch: https://www.ewe.com/de/karriere/aktuelle-stellenangebote.
#todo: error in sivis insertData HTML https://stcareers.talent-soft.com/job/list-of-jobs.aspx?page=3&LCID=2057
# todo: Missing clipboardtext: https://connekthq.com/plugins/ajax-load-more/examples/default/
#todo: FÃ¼r Unterschied von   url = cbData$request$request$url und url <- cbData$clipBoardText$pageUrl bei abendblatt.de. Ersterer ist irgendein JS von anderer Seite.

print <- function(x, max = 500){
  if(is.character(x)){
    message("a")
    base::print(substr(x = x, start = 1, stop = max))
  }else{
    base::print(x[1:max])
  }
}

#todo: BenÃ¶tige Class oder Indexierung
#"https://www.facebook.com/careers/jobs/?page=2"

#### GET Probleme:
# in Javascript callback : https://careers.travelers.com/job-search-results/?pg=5

########### todo: in sivis chrome: #################
# cant get data for job subpage: https://www.sage.com/de-de/unternehmen/karriere/karriere-suche/?page=1
# cant get middle column: https://jobboerse.strabag.at/jobs-overview.php?language=AT_DE
# https://jobs.nike.com/de/?jobSearch=true&jsOffset=0&jsSort=relevance&jsLanguage=de
# resource type --> other - results in png:   https://careers.adidas-group.com/jobs
# gets document if try to select all - https://www.pmi.com/careers/explore-our-job-opportunities?title=&locations=
# https://careers.blackrock.com/job-search-results #### cant select all data
# https://jobs.cvshealth.com/ListJobs?prefilters=none&CloudSearchLocation=none

# WRONG REQUEST in
# https://mastercard.jobs/jobs/#3
# and https://www.att.jobs/search-jobs

#only for one job: https://careers.kronos.com/

# encoding:
# httpscareerslillycomsearchjobs - request ist korrekt aus R heraus. Encoding von sivis chrome ist falsch. seite --> https://careers.lilly.com/search-jobs

############encoding#################
# chinese jobs: https://pfizer.wd1.myworkdayjobs.com/PfizerCareers

###### cant get data:
# cant get data: https://emerson.taleo.net/careersection/ex/jobsearch.ftl?lang=de.
# cant get data for main page but once!!  i think follow pages: https://www.gd.com/en/careers/job-search
# cant get data: https://jobs.axa/careersection/1/moresearch.ftl?lang=de&portal=8105020509
#https://jobs.celgene.com/jobs?page=1
#https://acetalent.taleo.net/careersection/ace_external/jobsearch.ftl?lang=en#
#https://mondelez.avature.net/careers
#https://www.aetnacareers.com/search-jobs
# TODO: DAVID KREISEL _ SCHMALES BUDGET _ RECHTSBEISTAND ONLINE

############ todo in get request keys #################
# https://careers.google.com/jobs/results/?company=Google&company=YouTube&hl=en&jlo=en-US&location=Z%C3%BCrich,%20Switzerland


####### get back a png
####https://careers.slb.com/job-listing

# regularities
### https://careers.bat.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=10
### gsk https://jobs.gsk.com/en-gb/jobs?page=3 --> JIBE


#todo: https://www.tesco-careers.com/search-and-apply/?nodeId=1206&type=1&page=2

cleaning <- TRUE
if(cleaning){
  rm(list = ls())
}

options(stringsAsFactors = FALSE)
library(magrittr)
library(rvest)
library(rstudioapi)
library(stringr)
library(glue)
library(httr)
library(jsonlite)
library(DT)
library(dplyr)

#debugSource("~/sivis/rvestFunctions.R")


sivis <- new.env(parent = emptyenv())
sivis$GETContents <- list()


# ff <- cbData$getUrl %>% GET %>% content %>% showHtmlPage

# cbdataFlat <- readClipboard() %>% fromJSON(simplifyDataFrame = FALSE, flatten = FALSE)
# cbData

# url %>% GET %>% content %>% showHtmlPage
# url <- "C:/Users/User1/Documents/sivis/test.html"


##
# For testing i need the following structure:
# A function i can feed in sivis environment with output: expected result data and
# input --> clipboard data + optional response object from server.
# (Optional, because it could either be objects from internet - that one i want to save because later on i might not
# be able to reproduce them [time variant data] or required, because i simulate a server with e.g. plumber.)

# In order to arrive there i need a preprocessing function that can arrive there. It creates all the necessary inputs
# for the testing

createScraper <- function(){
  sivis$cbData <- readClipboard() %>% fromJSON
  sivis$cbData$request$`_fromCache`
  sivis$cbData
  sivis$cbData$request$request$url

  cbData <- sivis$cbData
  create_sivis(cbData)
  testRun = FALSE
  success <- use_sivis(sivis, testRun = testRun)
  success
}


create_sivis <- function(cbData){
  sivis$headers <- cbData$request$request$headers %>% {setNames(object = .$value, nm = .$name)}

  hdr <- sivis$headers["accept-encoding"]
  if(grepl(pattern = "br", hdr)){
    message("Info: removing br(otli) as accepted encoding as its not supported by curl.")
    sivis$headers["accept-encoding"] <- gsub(pattern = ", br|br, ", replacement = "", x = hdr)
  }

  if(is.null(cbData$clipBoardText)) stop("no clipboard text")
  method <- cbData$request$request$method
  resourceType <- cbData$request$`_resourceType`

  pattern <- "file:///"
  pageUrl <- cbData$request$request$url
  url = cbData$request$request$url # alternative: cbData$clipBoardText$pageUrl
  ##url <- cbData$clipBoardText$pageUrl
  if(grepl(pattern = pattern, x = url)){
    sivis$browserOutput$pageUrl %<>% gsub(pattern = pattern, replacement = "")
    url %<>% gsub(pattern = pattern, replacement = "")
  }


  sivis$browserOutput <- cbData$clipBoardText
  if(resourceType %in% "png"){
    stop(glue("Wrong resource type: {resourceType}. File an issue with: resourceType = {resourceType},
              time: {Sys.time()}, source url = {url} and an indication if this issue would be reproducible at a later time or if your
              selectedText = {sivis$browserOutput$selectedText[1]},... will likely change over time."))
  }

  cbData$request$response

  responseType <- cbData$request$response$content$mimeType
  responseType
  resourceType



  # There are different types of responsetypes: application/JSON, text/html or javascript code  with jsons (or plain javascript
  # which would be more difficult).
  # They can also be nested within each other: Within the JSON could be html and within an html could be jscode / a json (e.g. at
  # //html/body/script).
  # Therefore, it has to be checked which type of response is present. That could be possible with mimeType / responseType, but
  # not for the nested texts (at lower extraction levels - e.g. the html within a json).

  # Since the GET document / page source is also of type GET it will be only differentiated between request methods (mostyl GET, POST, etc.)
  # and the response is treated as unknown.


  assign("url", value = url, envir = sivis)
  # if GET request was already performed in this session dont perform it again to avoid extensive amount of requests for the same server
  contactDetails <- NULL
  agentName <- paste(contactDetails, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36")
  #        user_agent(agentName)
  sivis$GETContents <- list()


  method
  if(method == "POST"){
    body <- sivis$cbData$request$request$postData$text
    if(!(sivis$url %in% names(sivis$GETContents))){
      sivis$GETContents[[sivis$url]] <- POST(
        url = sivis$url,
        httr::add_headers(.headers = sivis$headers),
        body = body
      )
    }
  }

  if(method == "GET"){

    if(!(sivis$url %in% names(sivis$GETContents))){
      sivis$GETContents[[sivis$url]] <- GET(
        url = sivis$url,
        httr::add_headers(.headers = sivis$headers)
      )
    }
  }

  getRes <- sivis$GETContents[[sivis$url]]

  urlShort <- gsub(
    x = sivis$url,
    pattern = "[:]|[.]|[/]|[?]|[&]|[=]|[%]|[-]",
    replacement = "",
    perl = TRUE
  )

  offline <- FALSE
  if(offline){
    fls <- list.files(path = "R/fromWeb/",pattern = "*.RData")
    fls
    fl <- fls[11]
    # to do logging messages for user about progress and assumptions
    # fl <- "httpswwwcapitalonecareerscomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50Radi.RData" for doubleextract json/html
    # fl <- "httpsvalerotaleonetcareersectionrestjobboardsearchjobslangenportal101430233.RData" create first POST request


    # nested JSON then html: "httpsjobsraytheoncomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50RadiusUnitType0KeywordsLocationLatitudeLongitudeShowRadiusFalseCustomFacetNameFacetTermFacetType0SearchResultsModuleNameSearch+ResultsSearchFiltersModuleNameSearch+FiltersSortCriteria0SortDirection1SearchType5CategoryFacetTermCategoryFacetTypeLocationFacetTermLocationFacetTypeKeywordTypeLocationTypeLocationPathOrganizationIdsPostalCodefcflfcfafcaflafcf"
    #DONEDONE#  too many target values: fl <- "httpscareersintuitivecomapijobspage2internalfalseuserIdc584a5e0ba114d19b2e225622b9e90e7sessionId773f7.RData"

    ###### wait for SO
    # multi json fl <- "httpswwwnorthropgrummancomJobsNGFeaturedJobsdefaultaspx.RData"

    #### Problem with find json in string, too many iterations:
    #fl <- "httpsjobsraytheoncomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50RadiusUnitTy.RData"

    ## DONE
    # fl <- "httpssearchcareersgmcomsearchresultsfrom50s1.RData" html/script with multiple jsons?
    # fl <- "httpscareersactivisioncomsearchresults.RData"
    #DONEDONE# fl <- "httpscareersintuitcomjobsearch.RData" too many targetValues

    print(fl)
    load(file = paste0("sivis/testData/fromWeb/", fl))
    targetValues <- sivis$browserOutput$selectedText
    XPathFromBrowser <- sivis$browserOutput$XPath
    pageUrl <- sivis$browserOutput$pageUrl
  }else{
    XPathFromBrowser <- cbData$clipBoardText$XPath
    targetValues <- sivis$cbData$clipBoardText$selectedText
    fileName <- paste0("C:/Users/User1/Documents/mypkg2/R/fromWeb/", substring(text = urlShort, first = 1, last = 101), ".RData")
    print(fileName)
    save(sivis, file = fileName)
  }
}

if(FALSE){
  fl <- "httpsjobsdisneycareerscomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50RadiusU.RData"
  fl <- "httpsjobsapigooglemcloudioapijobsearchcallbackjobsCallbackpageSize10offset0companyNamecompanies2Fc3f8.RData"


  fl <- "httpscareersdupontcomjobsearchajaxcallbacksgetJobsphpcategory5B5Dorganization5B5Dtype5B5Dlocation5B5D.RData"
  fl <- "httpscareersintuitivecomapijobspage2internalfalseuserIdc584a5e0ba114d19b2e225622b9e90e7sessionId773f7.RData"
  fl <- "httpswwwcapitalonecareerscomsearchjobsresultsActiveFacetID0CurrentPage3RecordsPerPage15Distance50Radi.RData"
  load(file = paste0("R/fromWeb/", fl))
  testRun <- FALSE
  ff <- use_sivis(sivis, testRun = testRun)
}

use_sivis <- function(sivis, testRun = TRUE){
  url = sivis$url
  cbdata = sivis$browserOutput
  getRes = sivis$GETContents[[1]]

  status <- getRes %>% status_code()
  if(status != 200) glue("status code of server response is: {status}") %>% warning

  targetValues <- sivis$browserOutput$selectedText
  XPathFromBrowser <- sivis$browserOutput$XPath
  pageUrl <- sivis$browserOutput$pageUrl

  responseString <- getRes %>% content(type = "text")
  if(!nchar(responseString)) stop("response body from server seems to be empty.")

  # split for text/html, because dont want to differentiate between encoding!?
  contentType <- getRes$headers$`content-type`
  # content type can be NULL? see https://www.accenture.com/de-de/careers/jobsearch?jk=&sb=1
  if(!is.null(contentType)){
    docType <- contentType %>%
      strsplit(split = ";") %>%
      unlist %>%
      .[1]
  }else{
    docType <- NULL
  }

  #docType <- findDocType(responseString)

  # a function that
  # - gets a (http response) string (or string of subelement)
  # - decides if its json, html or text containing jsons (or text with js code).
  # - creates a path to extract the target element(s)
  # - saves this path for reproducing the http request in R
  # - decides if target element was found or if its nested in subelement
  # - if its nested in sub element recall this function again
  # - also gets the pageUrl - to create the reproducing code.

  docType
  extractPathes = list()
  iterNr <- 0

  result <- extracts_data(
    responseString = responseString,
    docType = docType,
    cbdata = cbdata,
    XPathFromBrowser = XPathFromBrowser,
    extractPathes = extractPathes,
    pageUrl = pageUrl,
    targetValues = targetValues,
    testRun = testRun,
    iterNr = iterNr
  )
  return(result)
}



extracts_data <- function(responseString, docType = NULL, cbdata, XPathFromBrowser = "", extractPathes = list(), pageUrl = pageUrl,
                          targetValues, iterNr = 0, testRun = FALSE){
  # this function can get called by itself if the targetvalues have to be extracted "across multiple levels". E.g. if within
  # response is a json with an object including html.
  # If it is called a second time, the document type (html) is not known before and has to be identified.
  print(iterNr)
  iterNr = iterNr + 1
  if(iterNr > 6) stop("Too many iterations. Want to avoid getting caught in an infinite loop.")

  if(is.null(docType)) docType <- findDocType(responseString = responseString, targetValues = targetValues)
  docType
  resourceType <- sivis$cbData$request$`_resourceType`
  print(docType)
  print(resourceType)
  if(is.null(resourceType)) resourceType  <- ""
  if(docType == "script/json" | resourceType == "script" & docType == "application/json"){
    str <- responseString
    jsonExtractor <- JSON_from_String(
      str = str,
      targetValues = targetValues
    )
    responseString <- jsonExtractor$jsons
    extractPathes <- c(extractPathes, list(scriptJsonIndex = jsonExtractor$index))

    # have extracted a JSON now, can move on as if i would have gotten a JSON from the server.
    # The necessary extraction step is saved in variable above: extractPathes.
    docType <- "application/json"
  }
  docType
  if(docType == "application/json"){
    jsonStruct <- extract_JSON(
      responseString = responseString,
      targetValues = targetValues,
      extractPathes = extractPathes
    )
    str(jsonStruct)
    jsonStruct$extractPathes$json$isLargeHTML
    extractPathes <- c(extractPathes, jsonStruct$extractPathes)
    print(jsonStruct$allFound)
    if(jsonStruct$allFound){
      if(testRun) return(TRUE)
      createDocumentGET(
        url = pageUrl,
        cbdata = cbdata,
        getRes = getRes,
        extractPathes = extractPathes
      )
    }else{
      responseString = jsonStruct$resultValues
      extractPathes = jsonStruct$extractPathes
      docType <- NULL
      XPathFromBrowser <- XPathFromBrowser
      iterNr <- iterNr
      targetValues <- targetValues
      testRun <- testRun

      extracts_data(
        responseString = responseString,
        extractPathes = extractPathes,
        docType = docType,
        XPathFromBrowser = XPathFromBrowser,
        iterNr = iterNr,
        targetValues = targetValues,
        testRun = testRun
      )
    }
  # nest in else if otherwise json with html is extracted and jumped right into html extraction
  # without adjusting the inputs
  }else if(docType == "text/html"){
    maxCheck = 5
    htmlResult <- extract_HTML(
      responseString = responseString,
      targetValues = targetValues,
      extractPathes = extractPathes,
      XPathFromBrowser = XPathFromBrowser,
      maxCheck = maxCheck
    )
    htmlResult
    htmlResult$extractPathes
    htmlResult$allFound
    if(htmlResult$allFound){
      if(testRun) return(TRUE)
      # have to set xpath to environ/global variable, so that later on xpathes can be added
      sivis$XPathes <- htmlResult$extractPathes$xpath
      extractPathes = htmlResult$extractPathes
      getRes = NULL
      url <- pageUrl
      createDocument(
        pageUrl = url,
        extractPathes = extractPathes
      )
    }else{
      responseString = htmlResult$resultValues
      extractPathes = htmlResult$extractPathes
      docType = NULL
      XPathFromBrowser = ""
      iterNr <- iterNr
      targetValues <- targetValues
      testRun <- testRun
      extracts_data(
        responseString = responseString,
        extractPathes = extractPathes,
        docType = docType,
        XPathFromBrowser = XPathFromBrowser,
        iterNr = iterNr,
        targetValues = targetValues,
        testRun = testRun
      )
    }
  }
}


use_sivis_deprecated <- function(){
  if(resourceType == "document" & method == "GET"){
    #url %>% GET(add_headers(.headers = headers)) %>% toString %>% grep(pattern = "Ausbildung Drogist")
    sivis$doc <- url %>% read_html
    #sivis$doc %>% showHtmlPage
    browserOutput = sivis$browserOutput
    insertData(browserOutput = browserOutput)
  }

  if(method == "GET" & resourceType %in% c("xhr", "fetch")){
    assign("url", value = url, envir = sivis)

    # if GET request was already performed in this session dont perform it again to avoid extensive amount of requests for the same server
    contactDetails <- NULL
    agentName <- paste(contactDetails, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36")
    sivis$GETContents <- list()
    if(!(sivis$url %in% names(sivis$GETContents))){
      sivis$GETContents[[sivis$url]] <- GET(
        url = sivis$url,
        user_agent(agentName)
      )
    }
    getRes <- sivis$GETContents[[sivis$url]]
    urlShort <- gsub(x = sivis$url, pattern = "[:]|[.]|[/]|[?]|[&]|[=]|[%]|[-]", replacement = "", perl = TRUE)
    #load(file = paste("httpsjobsnikecomdejobSearchtruejsOffset0jsSortrelevancejsLanguagede", ".RData"))
    save(sivis, file = paste0(substring(text = urlShort, first = 1, last = 100), ".RData"))

    url = sivis$url
    cbdata = sivis$browserOutput
    getRes = sivis$GETContents[[1]]
    createDocumentGET(
      url = url,
      cbdata = cbdata,
      getRes = getRes
    )
  }else
    if(nchar(cbData$postUrl)){
      createDocumentPOST(url = cbData$postUrl, cbdata = cbData, cbdataFlat = cbdataFlat)
    }

}


extract_HTML <- function(responseString, targetValues, extractPathes, XPathFromBrowser = "", maxCheck = 5){
  XPathNr <- 1
  xpath <- ""
  text <- targetValues[1]
  allText <- targetValues[1:min(length(targetValues), maxCheck)]
  exact <- FALSE
  url <- sivis$url
  sivis$doc = responseString %>% read_html
  doc <- sivis$doc
  #doc %>% showHtmlPage
  attr = NULL #"class"
  byIndex = TRUE

  XPathCandidates <- sapply(
    X = allText,
    FUN = getXPath,
    allText = allText,
    url = url,
    exact = exact,
    doc = doc,
    attr = attr, #"class"
    byIndex = byIndex
  )
  XPathCandidates %<>% unlist %>% unique
  XPathCandidates
  # alternative approach go for frequencies
  #XPathCandidates <- xpathes %>% c %>% table %>% data.frame

  #if(is.null(XPathCandidates))
  xpath <- XPathCandidates %>%
    adist(y = XPathFromBrowser) %>%
    data.frame(dist = ., xpathes = XPathCandidates) %>%
    filter(dist == min(dist)) %>%
    select(2) %>%
    unname %>%
    unique %>%
    unlist %>%
    .[1]

  xpath
  resultValues <- doc %>% html_nodes(xpath = xpath) %>% html_text
  targetValues %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws
  resultValues %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws

  approximate <- TRUE
  if(approximate){
    lengths <- targetValues %>% nchar
    similarityRatio <- adist(x = targetValues, y = resultValues) %>%
      apply(MARGIN = 1, FUN = min) %>%
      divide_by(lengths)
    allFound <- similarityRatio %>%
      magrittr::is_less_than(0.05) %>%
      all
  }else{
    allFound <- all(targetValues %in% resultValues)
  }

  list(
    allFound = allFound,
    extractPathes = c(extractPathes, list(xpath = xpath)),
    resultValues = resultValues
  )
}


# For the scheduled scrape i want to extract by index not by targetvalue, since the target values wll change.
# For the initial scrape i want to extract by target value. The index value i can not know so far.
JSON_from_String <- function(str, targetValues = NULL, indexNr = NULL){
  # httpsjobsapigooglemcloudioapijobsearchcallbackjobsCallbackpageSize10offset0companyNamecompanies2Fc3f8
  #### recursion limit reached in PCRE for element 1

  allJSONS <- gregexpr(
    pattern = "\\{(?:[^{}]+|(?R))*?\\}",
    perl = TRUE,
    text = str
  ) %>%
    regmatches(x = str) %>%
    unlist
  if(!length(allJSONS)) stop("Expected a JSON, but did not find one.")

  if(!is.null(targetValues)){
    allJSONS %>%
      data.frame(jsons = ., match = grepl(pattern = paste(targetValues, collapse = "|"), x = .), index = 1:length(.)) %>%
      dplyr::filter(match == TRUE) %>% return
  }else if(!is.null(indexNr)){
    allJSONS %>%
      data.frame(jsons = ., index = 1:length(.)) %>%
      dplyr::filter(index == indexNr) %$% jsons %>%  return
  }else{
    stop("Please specify either the targetValues or the indexNr parameter.")
  }
}

extract_JSON <- function(responseString, targetValues, extractPathes = list()){
  jsonContent <- responseString %>% jsonlite::fromJSON()
  JSONValues <- allJSONValues(
    jsonContent = jsonContent,
    targetValues = targetValues
  )

  targetValues %<>% gsub(pattern = "\n|\r", replacement = "") %>% trimws
  resultValues <- JSONValues$texts %>% gsub(pattern = "\n|\r", replacement = "") %>% trimws

  distMatrix <- adist(x = targetValues, y = resultValues)
  distances <- apply(distMatrix, 1, min, na.rm = TRUE)

  # config parameter
  allFound <- (distances / nchar(targetValues) < 0.1) %>% {sum(.) / length(.)} > 0.9

  hasNAs <- JSONValues$texts %>% is.na %>% any
  if(allFound & hasNAs) warning("All target values found, but extraction of JSON values yields additional NAs")
  if(hasNAs) warning("Extraction of JSON values yields NAs")

  if(length(resultValues) > length(targetValues)){
    message("Found more values than expected!")
  }


  jsonToExtract <- list(
    reponse = "json",
    isLargeHTML = JSONValues$isLargeHTML,
    neighbours = JSONValues$neighbours,
    texts = JSONValues$texts,
    targetKey = JSONValues$targetKey,
    base = JSONValues$base,
    baseFollow = JSONValues$baseFollow
  )

  return(
    list(
      allFound = allFound,
      extractPathes = c(extractPathes, list(json = jsonToExtract)),
      resultValues = JSONValues$texts
    )
  )
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
# addXPath()
addXPath <- function(){
  XpathNr <- length(sivis$XPathes)
  browserOutput <- readClipboard() %>% fromJSON
  browserOutput$selectedText <- trimws(gsub(pattern = "\n", replacement = "", x = browserOutput$selectedText))


  if(!length(XpathNr)){
    print("Run InsertData() Function first!")
    return()
  }

  xpathRaw <- ""
  XPathIter <- 1
  candidates <- sivis$doc %>%
    html_nodes(xpath = browserOutput$clipBoardText$XPath) %>%
    html_text %>%
    gsub(pattern = "\n|\t", replacement = "") %>%
    trimws
  matches <- candidates %in% browserOutput$clipBoardText$selectedText
  if(sum(matches) / length(matches) > 0.4){
    xpathRaw <- browserOutput$clipBoardText$XPath
  }else{
    while(xpathRaw  == ""){
      xpathRaw <- getXPath(
        url = browserOutput$pageUrl,
        text = browserOutput$selectedText[XPathIter],
        exact = FALSE,
        doc = sivis$doc
      )
      print(XPathIter)
      XPathIter <- XPathIter + 1
      if(XPathIter > length(browserOutput$selectedText)) stop("didnt find a match for xpath")
    }
  }

  #matchIdxRaw <- sapply(targetValues, matchStrings, candidates = candidates)

  XPath <- data.frame(xpathRaw)
  rstudioapi::documentSave(id = "Notebook_scraping.Rmd")
  existingXPathes <- getXPathFromScript()
  sivis$XPathes <- c(existingXPathes, paste0("XPath", length(existingXPathes) + 1, " = \"", unname(XPath), "\""))
  # sivis$XPathes <- cbind(sivis$XPathes, XPath)

  if(is.null(sivis$urlGen)){
    createDocument(browserOutput = browserOutput, OneXPathOnly = FALSE)
  }else{
    GetLink(newLink = FALSE)
  }
}

# responseString <- "asdd <a>"
# responseString <- "asd {a:b, c:d} asdasd {a:b, c:e}"
# responseString <- '{"a":"text","b":"<html>asd</html>"}'
# responseString <- '"/*<!--*/ var phApp = phApp || {\"widgetApiEndpoint\":\"' is identified as html
# ---> if html and html, then html has to have a script tag

# complete list of meme types: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types
findDocType <- function(responseString, targetValues){

  isJSON <- jsonlite:::validate(responseString)

  isHTML <- tryCatch(
    expr = responseString %>%
      xml2::read_html() %>%
      typeof %>%
      magrittr::equals("list"),
    error = function(e) return(FALSE)
  )

  jsons <- gregexpr(
    pattern = "\\{(?:[^{}]|(?R))*\\}",
    text = responseString,
    perl = T
  ) %>%
  regmatches(x = responseString) %>%
  unlist

  #sapply(jsons, grepl, x = jsons, fixed = TRUE, USE.NAMES = FALSE)
  #grep(pattern = jsons, )
  if(length(jsons)){
    # config parameter
    matches <- sapply(targetValues, grepl, fixed = TRUE, x = jsons) %>% as.matrix
    ScriptJSON <- matches %>% rowSums() %>% {. / length(targetValues)} %>% magrittr::is_greater_than(0.95) %>% which
  }else{
    ScriptJSON <- FALSE
  }

  #### todo: refactor this shit
  if(isJSON){
    return("application/json")
  }else if(isHTML){
    if(ScriptJSON){
      hasScriptTag <- responseString %>%
        xml2::read_html() %>%
        html_nodes(xpath = "/script") %>%
        length
      if(hasScriptTag){
        return("text/html")
      }else{
        return("script/json")
      }
    }
    return("text/html")
  }else if(ScriptJSON){
    return("script/json")
  }else{
    return("unknown type")
  }
}


insertData <- function(browserOutput = cbData, maxCheck = 5){
  if(is.null(browserOutput)) browserOutput <- readClipboard() %>% fromJSON

  XPathNr <- 1
  xpath <- ""


  #todo: hella: https://hella-jobs.dvinci.de/cgi-bin/appl/selfservice.pl?action=search;page=2
  # make direct match with xpath from sivis
  # xx <- doc %>% html_nodes(xpath = browserOutput$XPath) %>% html_text
  # xx[1:5]
  # browserOutput$selectedText[1:5]
  if(is.null(browserOutput$selectedText)) stop("no selected text")
  text = browserOutput$selectedText[1]
  allText = browserOutput$selectedText[1:maxCheck]
  url <- browserOutput$pageUrl
  exact = FALSE
  doc = sivis$doc
  attr = NULL #"class"
  byIndex = TRUE

  XPathCandidates <- sapply(
    X = allText,
    FUN = getXPath,
    allText = allText,
    url = url,
    exact = exact,
    doc = doc,
    attr = attr, #"class"
    byIndex = byIndex
  )
  XPathCandidates %<>% unlist %>% unique

  # alternative approach go for frequencies
  #XPathCandidates <- xpathes %>% c %>% table %>% data.frame

  #if(is.null(XPathCandidates))
  xpath <- XPathCandidates %>%
    adist(y = browserOutput$XPath) %>%
    data.frame(dist = ., xpathes = XPathCandidates) %>%
    filter(dist == min(dist)) %>%
    select(2) %>%
    unname %>%
    unique


  sivis$XPathes <- paste0("XPath1 = \"", xpath, "\"")
  print(sivis$XPathes)
  sivis$browserOutput <- browserOutput
  OneXPathOnly <- length(sivis$XPathes) == 1
  sivis$urlGen <- NULL
  createDocument(browserOutput = browserOutput, extractPathes = extractPathes)
  #assign("scrapedData", browserOutput$selectedText, envir = .GlobalEnv)
}



createDocumentPOST <- function(url, cbdata, cbdataFlat){
  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"

  formData <- cbdataFlat$postBody

  body <- paste0(capture.output(dput(formData, control = "keepInteger")), collapse = "")
  postReqString <- paste0('x <- POST(url = "', url,'", encode = "json", body = ', body,')', collapse = "")
  evalPOSTReq <- "do.call(what = rbind, args = unlist(content(x), recursive = FALSE)) %>% datatable"

  fullRequest <- paste(postReqString, evalPOSTReq, sep = "\n")
  eval(parse(text = fullRequest), envir = .GlobalEnv)
  cat(
    paste0('---
           title: "R Notebook"
           output: html_notebook
           ---

           This is a scraping suggestion for the following website: ', cbdata$pageUrl,'.
           The required content was found in a POST request. httr::POST was chosen over RSelenium due to performance reasons .

           ```{r}
           library(DT)
           library(httr)
           ', fullRequest,'

           ```
           '), file = fileName
    )
  file.edit(fileName)
  eval(parse(text = fullRequest), envir = .GlobalEnv)
}


######## FOR JSON in XML/DOC
# ######## tested for:
# url <- "https://careers.loreal.com/global/en/search-results?from=200&s=1"
# xpath <- "/html/body/div[1]/main/section[3]/div/div/div[3]/article[1]/div[1]/div[1]/h3/a"
# data <- read_html(x = url) %>% html_nodes(xpath = as.character(xpath)) %>% html_text()
# target <- "Marketing Koordinator CAD"
# ff <- gregexpr(
#   pattern = "\\{(?:[^{}]|(?R))*?\\}",
#   perl = TRUE,
#   text = data
# ) %>%
#   regmatches(x = data) %>%
#   unlist %>%
#   data.frame(jsons = ., match = grepl(pattern = target, x = .), index = 1:length(.)) %>%
#   dplyr::filter(match == TRUE)
# xx <- ff$jsons %>% jsonlite::fromJSON()

# extractPath <- extractPathes[2:length(extractPathes)]
create_Addit_Extract <- function(extractPathes){
  if(!length(extractPathes)) return(NULL)
  nr <- 1
  for(nr in 1:length(extractPathes)){
    type <- extractPathes[nr] %>% names
    path <- extractPathes[type] %>% unlist %>% unname
    if(type == "xpath"){
      return(
        glue("\tresponse %<>% read_html %>% html_nodes(xpath = '{path}') %>% html_text")
      )
    }
  }
}


# if sivis data disappear try with assign("url", value = .GlobalEnv[["url"]], envir = sivis)
createDocumentGET <- function(url, cbdata, getRes, extractPathes = NULL){
  rstudioapi::documentSave(id = "Notebook_scraping.Rmd")

  #getRes %>% showHtmlPage
  #getRes %>% toString
  sivis$browserOutputRaw <- cbdata
  browserOutputRaw <- cbdata

  sivis$targetKeys <-  list(extractPathes$json$targetKey)

  #todo; make better
  extractType <- extractPathes[1] %>% names %>% toString
  if(extractType == "scriptJsonIndex"){
    idx <- extractPathes[[1]]
    jsonFromString <- TRUE
  }else{
    jsonFromString <- NULL
  }

  additionalExtractions <- create_Addit_Extract(extractPath = extractPathes)

  createDocumentGETW(
    jsonFromString = jsonFromString,
    additionalExtractions = additionalExtractions,
    extractPathes = extractPathes
  )
}




url <- sivis$browserOutputRaw$url
base <- deparse(dput(sivis$initGET$base))
baseFollow <- deparse(dput(sivis$initGET$baseFollow))
targetKeys <- deparse(sivis$targetKeys, width.cutoff = 500L)

# this covers: text2json, json extraction and early exit for huge html.
# does not cover: follow-up process of html or only html
baseGETTemplate <- function(url, base, baseFollow, targetKeys, isLargeHTML = FALSE, jsonFromString = FALSE){
  # todo refactor this
  if(is.null(isLargeHTML)) isLargeHTML <- FALSE
  if(is.null(jsonFromString)) jsonFromString <- FALSE

  paste0(c(
    'library(DT)',
    'library(httr)',
    '# ", 1 + (nr - 1)*maxItems,"',
    '# ", maxItems*nr,"',
    '',
    'scraper <- list(',
    paste0('\turlHTTP = "', url, '",'),
    '\turlGen = function(nr){',
    '\t\tmaxItems <- 100',
    paste0('\t\tpaste0("', sivis$url, '")'),
    '\t},',
    paste0('\tbase = ', base,','),
    paste0('\tbaseFollow = ', baseFollow,','),
    paste0('\ttargetKeys = ', targetKeys, ','),
    paste0('\tisLargeHTML = ', isLargeHTML, ','),
    paste0('\tjsonFromString = ', jsonFromString),
    ')',
    '',
    '',
    'output <- list()',
    'response <- "InitWithValue"',
    'nr <- 1',
    '',
    'while(length(response)){',
    '\tSys.sleep(0.2)',
    '\tprint(nr)',
    '\turl <- scraper$urlGen(nr)',
    '\t',
    '\tresponse <- scheduledGET(url = url, targetKeys = scraper$targetKeys, base = scraper$base, baseFollow = scraper$baseFollow, isLargeHTML = scraper$isLargeHTML, jsonFromString = scraper$jsonFromString)$res'),
    collapse = "\n")
}

createDocumentGETW <- function(jsonFromString = NULL, additionalExtractions = NULL, extractPathes = NULL){
  assign(x = "neighbours", value = unlist(sivis$initGET$neighbours), envir = .GlobalEnv)

  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  url <- sivis$browserOutputRaw$url

  base <- extractPathes$json$base %>% dput %>% deparse
  baseFollow <- extractPathes$json$baseFollow %>% dput %>% deparse

  isLargeHTML <- extractPathes$json$isLargeHTML
  targetKeys <- deparse(sivis$targetKeys, width.cutoff = 500L)
  writeLines(
    text = paste0(c('---',
                    'title: "R Notebook"',
                    'output: html_notebook',
                    '---',
                    '',
                    paste0('This is a scraping suggestion for the following website: ', sivis$browserOutput$url, '.'),
                    'The required content was found in a GET request.',
                    '',
                    '```{r}',
                    baseGETTemplate(
                      url = url,
                      base = base,
                      baseFollow = baseFollow,
                      targetKeys = targetKeys,
                      isLargeHTML = isLargeHTML,
                      jsonFromString = jsonFromString
                    ),

                    additionalExtractions,
                    '\toutput[[nr]] <- response',
                    '\tnr <- nr + 1',
                    '',
                    '\t######## INITIALLY ONLY ONE ROUND',
                    '\tresponse <- c()',
                    '\t######## INITIALLY ONLY ONE ROUND',
                    '}',
                    'tbl <- do.call(what = rbind, args = output) %>% c %>% data.frame(data = .)',
                    'datatable(tbl)',
                    '',
                    '```',
                    '',
                    'Potential new fields to consider:',
                    '',
                    '```{r}',
                    'library(shiny)',
                    '',
                    'choiceNames <- paste(paste("<b>", names(neighbours), "</b>"), as.character(neighbours), sep = ": ")',
                    'choiceNames <- lapply(choiceNames, HTML)',
                    '',
                    'targetValue <- "12"',
                    'ui <- fluidPage(',
                    '\tactionButton(inputId = "updateDocument", label = "Add selected keys to document:"),',
                    '\tbr(),',
                    '\tcheckboxGroupInput(inputId = "additionalKeys", label = "Additional keys to parse: ", choiceValues = as.list(names(neighbours)),',
                    paste0('\tselected = "', sivis$initGET$targetKey,'", choiceNames = choiceNames)'),
                    ')',
                    'server <- function(input, output, session){',
                    '\tobserveEvent(eventExpr = input$updateDocument,{',
                    '\t\tprint(input$additionalKeys)',
                    '\t\tfile.remove("Notebook_Scraping.Rmd")',
                    '\t\tsivis$targetKeys <-  sapply(input$additionalKeys, strsplit, split = "[.]", USE.NAMES = FALSE)',
                    '\t\tcreateDocumentGETW()',
                    '\t\tstopApp(returnValue = invisible())',
                    '\t})',
                    '}',
                    '',
                    'runApp(appDir = shinyApp(ui, server), launch.browser = rstudioapi::viewer)',
                    '```'), collapse = "\n"),
    con = fileName)
  file.edit(fileName)
}






createDocument <- function(pageUrl, extractPathes){

  XPathes <- sivis$XPathes
  OneXPathOnly <- length(XPathes) == 1
  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  print(XPathes)

  is.null(extractPathes$json)
  url <- sivis$browserOutput$pageUrl
  print("url")
  print(url)
  if(is.null(extractPathes$json)){
    getTemplate <- paste0(c('library(httr)',
        'library(DT)',
        paste0('url <- "', url, '"'),
        'response <- url %>% GET %>% content(type = "text")'
      ), collapse = "\n"
    )
    getFinishTemplate <- ""
    indent <- ""
    displayResults <- 'response %>% data.frame %>% DT::datatable()'
  }else{
    indent <- "\t"
    sivis$initGET <- extractPathes$json
    base <- deparse(dput(extractPathes$json$base))
    baseFollow <- deparse(dput(extractPathes$json$baseFollow))
    targetKeys <- deparse(sivis$targetKeys, width.cutoff = 500L)
    isLargeHTML <- extractPathes$json$isLargeHTML
    getTemplate <- baseGETTemplate(url, base, baseFollow, targetKeys, isLargeHTML = isLargeHTML)
    getFinishTemplate <-  paste0(c('\toutput[[nr]] <- response',
    '\tnr <- nr + 1',
    '',
    '\t######## INITIALLY ONLY ONE ROUND',
    '\tresponse <- c()',
    '\t######## INITIALLY ONLY ONE ROUND',
    '}'),
    collapse = "\n"
    )
    displayResults <- "do.call(rbind, output) %>% c %>% data.frame %>% DT::datatable()"
  }

  if(OneXPathOnly){

    rcode_deprecated <- paste(c('options(stringsAsFactors = FALSE)',
                     'library(rvest)',
                     'library(DT)',
                     getTemplate,
                     paste(c('url <- "', pageUrl, '"'), collapse = ""),
                     'xpath <- data.frame("',
                     paste(c('\t', XPathes), collapse = ""),
                     '")',
                     'data <- read_html(x = url) %>% html_nodes(xpath = as.character(xpath)) %>% html_text()',
                     'dt <- datatable(',
                     '\tdata = data.frame(data),',
                     '\toptions = list(pageLength = 10)',
                     ')',
                     'dt'
    ), collapse = "\n")

    rcode_deprecated2 <- paste(c('options(stringsAsFactors = FALSE)',
                     'library(httr)',
                     'library(xml2)',
                     'library(DT)',
                     paste0('url <- "', url, '"'),
                     'response <- url %>% GET %>% content(type = "text")',
                     getTemplate,
                     'xpath <- data.frame(',
                     paste(c('\t"', XPathes, '"'), collapse = ""),
                     ')',
                     'response %<>% read_html %>% html_nodes(xpath = as.character(xpath)) %>% html_text()',
                     getFinishTemplate
    ), collapse = "\n")



    rcode <- paste(c('options(stringsAsFactors = FALSE)',
                     'library(xml2)',
                     getTemplate,
                     paste(c(indent, 'xpath <- data.frame('), collapse = ""),
                     paste(c(indent, '\t"', XPathes, '"'), collapse = ""),
                     paste(c(indent, ')'), collapse = ""),
                     paste(c(indent, 'response %<>% read_html %>% html_nodes(xpath = as.character(xpath)) %>% html_text()'), collapse = ""),
                     getFinishTemplate
    ), collapse = "\n")

    #try(eval(parse(text = rcode), envir = .GlobalEnv))

    writeLines(
      text = paste(c('---',
                     'title: "R Notebook"',
                     'output: html_notebook',
                     '---',
                     '',
                     'This is a scraping suggestion for the following website: ', pageUrl,
                     'The required content was found in the source code. Therefore, rvest was chosen over RSelenium due to performance reasons.',
                     '',
                     '```{r}',
                     rcode,
                     displayResults,
                     '```'),
                   collapse = "\n"),
      con = fileName
    )
    file.edit(fileName)

  }else{
    #xp <- paste0(paste0('XPath', 1:length(dput(sivis$XPathes)),' = "', dput(sivis$XPathes[1, ]), '\"'), collapse = ",\n\t")
    xp <- paste(sivis$XPathes, collapse = ",\n\t")
    rcode <- paste0(c('options(stringsAsFactors = FALSE)',
                      'library(rvest)',
                      'library(DT)',
                      paste0('url <- "', pageUrl, '"'),
                      paste0('xpathes <- ', paste(c(paste(c("data.frame(", xp), collapse = "\n\t"), ")"), collapse = "\n")),
                      'code <- read_html(x = url)',
                      'data <- sapply(xpathes, function(xpath) html_nodes(x = code, xpath = xpath) %>% html_text())',
                      'dt <- datatable(',
                      '\tdata = data,',
                      '\toptions = list(pageLength = 10)',
                      ')',
                      'dt'), collapse = "\n")


    tryCatch(eval(parse(text = rcode), envir = .GlobalEnv),error = function(e) NULL)

    writeLines(
      text = paste0(c('---',
                      'title: "R Notebook"',
                      'output: html_notebook',
                      '---',
                      '',
                      paste0('This is a scraping suggestion for the following website: ', pageUrl, '.'),
                      'The required content was found in the source code. Therefore, a get request will be performed on the target document.',
                      '',
                      '```{r}',
                      rcode
                      ,'```'),
                    collapse = "\n"
      ), con = fileName)
    file.edit(fileName)
  }
}


createDocument_deprecated <- function(pageUrl, XPathes){
  OneXPathOnly <- length(XPathes) == 1
  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  print(XPathes)

  if(OneXPathOnly){

    rcode <- paste(c('options(stringsAsFactors = FALSE)',
                     'library(rvest)',
                     'library(DT)',
                     paste(c('url <- "', pageUrl, '"'), collapse = ""),
                     'xpath <- data.frame("',
                     paste(c('\t', XPathes), collapse = ""),
                     '")',
                     'data <- read_html(x = url) %>% html_nodes(xpath = as.character(xpath)) %>% html_text()',
                     'dt <- datatable(',
                     '\tdata = data.frame(data),',
                     '\toptions = list(pageLength = 10)',
                     ')',
                     'dt'
    ), collapse = "\n")

    try(eval(parse(text = rcode), envir = .GlobalEnv))

    writeLines(
      text = paste(c('---',
                     'title: "R Notebook"',
                     'output: html_notebook',
                     '---',
                     '',
                     'This is a scraping suggestion for the following website: ', pageUrl,
                     'The required content was found in the source code. Therefore, rvest was chosen over RSelenium due to performance reasons.',
                     '',
                     '```{r}',
                     rcode,
                     '```'),
                   collapse = "\n"),
      con = fileName
    )
    file.edit(fileName)

    file.edit(fileName)

  }else{
    #xp <- paste0(paste0('XPath', 1:length(dput(sivis$XPathes)),' = "', dput(sivis$XPathes[1, ]), '\"'), collapse = ",\n\t")
    xp <- paste(sivis$XPathes, collapse = ",\n\t")
    rcode <- paste0(c('options(stringsAsFactors = FALSE)',
                      'library(rvest)',
                      'library(DT)',
                      paste0('url <- "', pageUrl, '"'),
                      paste0('xpathes <- ', paste(c(paste(c("data.frame(", xp), collapse = "\n\t"), ")"), collapse = "\n")),
                      'code <- read_html(x = url)',
                      'data <- sapply(xpathes, function(xpath) html_nodes(x = code, xpath = xpath) %>% html_text())',
                      'dt <- datatable(',
                      '\tdata = data,',
                      '\toptions = list(pageLength = 10)',
                      ')',
                      'dt'), collapse = "\n")


    tryCatch(eval(parse(text = rcode), envir = .GlobalEnv),error = function(e) NULL)

    writeLines(
      text = paste0(c('---',
                      'title: "R Notebook"',
                      'output: html_notebook',
                      '---',
                      '',
                      paste0('This is a scraping suggestion for the following website: ', pageUrl, '.'),
                      'The required content was found in the source code. Therefore, a get request will be performed on the target document.',
                      '',
                      '```{r}',
                      rcode
                      ,'```'),
                    collapse = "\n"
      ), con = fileName)
    file.edit(fileName)
  }
}


OneTimeScrape <- function(){
  splitted <- strsplit(
    x = readClipboard(),
    split = ";"
  )[[1]]

  browserOutput <- list(
    url = tolower(splitted[1]),
    clickType = tolower(splitted[2]),
    expectedOutput = tolower(strsplit(splitted[3], split = "~~~")[[1]]),
    links = tolower(strsplit(splitted[4], split = "~~~")[[1]]),
    XPath = tolower(splitted[5]),
    XPathBlank = gsub(
      pattern = "[[]\\d+[]]",
      replacement = "",
      x = tolower(splitted[5])
    )
  )
  assign("scrapedData", browserOutput$expectedOutput, envir = .GlobalEnv)
}

ChangeOnString1 <- function(string1, string2){
  levenshteinRaw <- adist(string1, string2, count = TRUE)
  actionRaw = strsplit(attr(levenshteinRaw, "trafos"), '')[[1]]
  actionOnString1 <- actionRaw[actionRaw != "I"]
  WhichChangedString1 <- which(actionOnString1 %in% c("S", "D"))
  WhichChangedString1
}

getIndex <- function(string1, strings){
  allModifiedIndex <- sapply(X = strings, FUN = ChangeOnString1, string1 = string1, USE.NAMES = FALSE)
  keep <- lengths(allModifiedIndex) < 5
  allModifiedIndex <- allModifiedIndex[keep]
  Uniqueindexes <- sort(unique(unlist(allModifiedIndex)))
  valRaw <- paste(strsplit(x = string1, split = "")[[1]][Uniqueindexes], collapse = "")

  val <- as.numeric(gsub(pattern = "[/]|#", replacement = "", x = valRaw))
  list(
    val = val,
    idx = Uniqueindexes
  )
}


IterableLinkGenerator <- function(links, envir = sivis){
  dupes <- duplicated(links, fromLast = FALSE)  #| duplicated(links, fromLast = TRUE)
  strings <- links[!dupes]
  lengthsStrings <- nchar(strings)
  keepString <- abs(lengthsStrings - median(lengthsStrings)) < 3
  strings <- strings[keepString]

  getIndex(string1 = strings[1], strings = strings)
  toReplace <- lapply(X = strings, FUN = getIndex, strings = strings)
  num <- sapply(toReplace, "[[", "val")
  idx <- sapply(toReplace, "[[", "idx")
  tbl <- table(abs(diff(num)))
  counter <- names(which.max(tbl))

  if(counter == "1"){
    multiplier <- "nr"
    counter <- ""
  }else{
    multiplier <- "(nr - 1)*"
  }

  str1 <- paste0('function(nr) paste0("', substr(x = strings[1], start = 1, stop = (min(idx[[1]]) - 1)), '", ', multiplier, counter)

  middlePart <- ""
  stringLength <- nchar(strings[1])
  matchIndexMax <- max(idx[[1]])
  HaveToAddStringAtEnd <- stringLength > matchIndexMax
  if(HaveToAddStringAtEnd){
    middlePart <- paste0(', "', substr(x = strings[1], start = max(idx[[1]]) + 1, stop = nchar(strings[1])), '\"')
  }

  str1 <- paste0(str1, middlePart, ")")
  assign(x = "urlGen", value = eval(parse(text = str1)))
  return(urlGen)
}

GetLink <- function(newLink = TRUE){

  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  xp <- getXPathFromScript()

  if(newLink){
    browserOutput <- sivis$browserOutput
    if(is.null(browserOutput)) browserOutput <- readClipboard() %>% fromJSON
    links <- browserOutput$links
    if(all(is.na(links))) stop("Provided clipboard data doesnt have links as href attributes.")
    allEqual <- all(duplicated(links, fromLast = TRUE) | duplicated(links, fromLast = FALSE))
    if(allEqual){
      print("Href Attribute is not iterable")
      return()
    }else{
      urlGen <- IterableLinkGenerator(links = links)
      sivis$urlGen <- urlGen
      # nr <- 1:4
      # print(!sapply(X = urlGen(nr), FUN = http_error))
    }
  }


  writeLines(
    text = paste0('---
                  title: "R Notebook"
                  output: html_notebook
                  ---

                  This is a scraping suggestion for the following website: ', as.character(sivis$browserOutput$url),'.
                  The required content was found in the source code. Therefore, rvest was chosen over RSelenium due to performance reasons .

                  ```{r}
                  options(stringsAsFactors = FALSE)
                  library(rvest)
                  library(DT)
                  scrapeOutput <- "initWithValue"
                  continueScraping <- TRUE
                  output <- list()
                  urlGen <- ', Reduce(paste, deparse(sivis$urlGen)),'
                  nr <- 1

                  while(continueScraping){
                  print(nr)
                  url <- urlGen(nr)
                  xpathes <- ', paste(c(paste0("data.frame(\n", paste(xp, collapse = ",\n\t\t")), ")"), collapse = "\n"), '
                  code <- read_html(x = url)
                  scrapeOutput <- sapply(xpathes, function(xpath) html_nodes(x = code, xpath = xpath) %>% html_text())
                  continueScraping <- length(unlist(scrapeOutput))
                  repeated <- sum(unlist(sapply(output, identical, y = scrapeOutput))) > 0
                  if(repeated) continueScraping <- FALSE
                  output[[nr]] <- scrapeOutput
                  nr <- nr + 1
                  }
                  dt <- datatable(
                  data = do.call(rbind, output),
                  options = list(pageLength = 10)
                  )
                  dt
                  ```
                  '), con = fileName)
  file.edit(fileName)

}


getXPathFromScript <- function(){
  rmdCode <- readLines("Notebook_Scraping.Rmd")

  # todo: clean matching on xpath data.frame
  startIdx <- sapply(rmdCode, grep, pattern = ' <- data.frame(', fixed = TRUE) %>% lengths %>% `>`(0) %>% which %>% unname
  #startIdx <- which(rmdCode == "xpathes <- data.frame(")
  endIndexes <- data.frame(end = which(rmdCode == ")"))
  endRange <- endIndexes %>%
    `-`(startIdx) %>%
    dplyr::filter(end > 0) %>%
    slice(which.min(end)) %>%
    as.numeric

  rng <- (startIdx + 1):(startIdx + endRange - 1)
  sivis$xpathes <- rmdCode[rng] %>%
    gsub(pattern = ',|\t', replacement = "") #%>%
  # strsplit(split = ' = \"') %>%
  # do.call(what = rbind) %>%
  # gsub(pattern = '\"', replacement = "")

  return(sivis$xpathes)
}






library(glue)
library(rvest)

extractJobLink <- function(hrefAttrText, baseUrl){
  firstHrefPart <- strsplit(hrefAttrText, ";")[[1]][1]
  hasJavascript <- grep(pattern = "javascript:", x = firstHrefPart)
  if(length(hasJavascript)){
    extractedRaw <- str_extract(
      string = firstHrefPart,
      pattern = "\\(([^)]+)\\)"
    )
    firstHrefPart <- gsub(pattern = "\\(|\\)|'", replacement = "", x = extractedRaw)
  }
  match <- substr(firstHrefPart, 1, 4) == "http"
  if(sum(match) / length(match) < 1){
    dom <- domain(baseUrl) # scraper[[compName]]$url
    url <- paste0("https://", dom,  firstHrefPart)
  }else{
    url <- firstHrefPart
  }
  return(url)
}


# text = browserOutput$selectedText[XPathNr]
# text = allText[3]
# todo: escaping quotes, see below and example in: httpswwwfocusde.RData
getXPath <- function(url, text = NULL, xpath = NULL, exact = FALSE, doc = NULL, attr = NULL, byIndex = FALSE, allText){

  ########### FOR ESCAPING QUOTES
  # text3 <- "How can select from same Id\\'s different values?"
  # text2 <- text %>%  gsub(pattern = "'", replacement = "\\\\'")
  # text2 <- text

  if(!is.null(text)){
    text <- gsub(pattern = " |\n|\t", replacement = "", tolower(text))
    if(exact){
      xpath <- paste0("//*[text() = '", text,"']")
    }else{
      xpath <- paste0("//text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", text,"')]")
    }
  }
  #doc %>% showHtmlPage
  if(is.null(doc)) doc <- read_html(x = url)
  tags <- doc %>% html_nodes(xpath = xpath)
  if(length(tags) > 1) warning("Found more than one matching element!")

  if(!length(tags)){
    glue("Did not find element for {text}.") %>% warning
    return("")
    xpath = ""

    #showHtmlPage(doc)
  }
  # somehow cant use apply family here. last tag vanishes.
  nr <- 1
  xpathes <- rep(NA, length(tags))
  tag <- tags[nr]
  for(nr in 1:length(tags)){
    xpathes[nr] <- getXPathByTag(
      tag = tags[nr],
      attr = attr,
      byIndex = byIndex,
      allText = allText,
      doc = doc
    )
  }

  xpathes
}

# byIndex = FALSE
# tag <- tag[2]
# attr = "class"

checkXPath <- function(tagNameInsert, tags, AmtElemBefore = 1e6, XPathClass = "", allText){
  tagName <- glue("{tagNameInsert}{XPathClass}")
  xpathBuild <- paste(c("", tagName, tags[length(tags):1]), collapse = "//")
  tagTexts <- sivis$doc %>%
    html_nodes(xpath = xpathBuild) %>%
    html_text
  allTargetsFound <- sapply(allText, grep, x = tagTexts, fixed = TRUE) %>%
    lengths %>% `>`(0) %>% all
  NoExtraTags <- all(tagTexts %in% allText)
  AmtElemFound = length(tagTexts)
  allTargetsFound = allTargetsFound

  if(allTargetsFound & NoExtraTags) XPathFound <- TRUE
  if(!allTargetsFound | !(AmtElemFound < AmtElemBefore)){
    XPathClass <- ""
  }

  data.frame(
    class = XPathClass,
    AmtElemFound = AmtElemFound,
    XPathFound = XPathFound,
    XPath = xpathBuild
  )
}

# vec <- letters[1:4]
# vec <- glue("contains(@class, '{vec}')")
#
# XPClasses <- sapply(seq_along(vec), function(x) combn(vec, x, FUN = paste, collapse = " and ")) %>%
#   unlist %>%
#   paste0("[", ., "]")
#
# # doc <- read_html("test.html")
# # tags <- c()
# # tagNameInsert <- "a"
# lapply(
#   X = XPClasses,
#   FUN = checkXPath,
#   tagNameInsert = tagNameInsert,
#   tags = tags
# )

# text <- "Anlagenmechaniker"
# doc <- contInit %>% read_html
# getXPathByText(text, doc, exact = FALSE, attr = NULL, byIndex = FALSE)
getXPathByText <- function(text, doc, exact = FALSE, attr = NULL, byIndex = FALSE){
  text %<>% tolower
  xpath <- paste0("//*[text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '",
                  text, "')]]")
  tag <- doc %>% html_nodes(xpath = xpath)

  tagName <- ""
  tags <- c()


  if(length(tag) > 1){
    tagNames <- sapply(tag, html_name)
    match <- which(tagNames != "script")
    if(!length(match)) match <- 1
    tag <- tag[match[1]]
  }
  while(tagName != "html"){
    tagName <- tag %>% html_name
    tags <- c(tags, tagName)
    tag <- tag %>% html_nodes(xpath = "..")
  }

  xpath <- paste(c("", tags[length(tags):1]), collapse = "/")
  xpath

}

#tag <- tags[[1]]
getXPathByTag <- function(tag, exact = FALSE, attr = NULL, byIndex = TRUE, allText, doc){
  # print("JUMP OVER BREAKPOINT")

  tagName <- ""
  tags <- ""
  XPathFound <- FALSE

  tag <- as.list(tag)
  iterTag <- tag # in case of debugging nike needed
  while(!XPathFound){
    tagName <- iterTag %>% html_name
    # print(iterTag)
    print(tagName)
    tagNameInsert <- tagName
    if(byIndex){
      childrenAll <- iterTag %>%
        html_nodes(xpath = "..") %>%
        html_nodes(xpath = "child::*")

      childrenMatchTag <- childrenAll[html_name(childrenAll) == tagName]
      childrenMatchText <- childrenAll %>% html_text

      # In case of multiple target strings: Dont set index if its html_text includes
      # other targetStrings. In current implementation this includes also
      # indexes of all parent tags, since their html_text also include
      # the other targetStrings.

      # refactor better grepl?
      hasAllChildren <- sapply(allText, grep, x = childrenMatchText, fixed = TRUE) %>%
        lengths %>% `>`(0) %>% all
      # hasAllChildren <- sapply(allText, grepl, x = childrenMatchText, fixed = TRUE) %>%
      #   matrix(nrow = length(allText)) %>%
      #   rowSums %>%
      #   which.max
      firstRound <- is.null(tags)
      if(!hasAllChildren | firstRound | all(unique(tags) %in% "")){
        matches <- rep(0, length(childrenMatchTag))
        # dont need to set index if there is only one element with same tag type.
        # for consistency with chrome output.
        if(length(matches) > 1){
          for(nr in 1:length(childrenMatchTag)){
            matches[nr] <- identical(childrenMatchTag[nr], iterTag)
          }
          tagNameInsert <- glue("{tagName}[{which(matches == 1)}]")
        }
      }
      XPathFound <- tagName != "html"
    }

    # if(!is.null(attr)){
    #
    #   NoClass <- checkXPath(
    #     tagNameInsert = tagNameInsert,
    #     tags = tags,
    #     allText = allText
    #   )
    #   NoClass
    #   if(NoClass$XPathFound) return(NoClass$XPath)
    #
    #   # check if the tag has a class
    #   classes <- iterTag %>%
    #     html_attr(attr) %>%
    #     strsplit(split = " ") %>%
    #     unlist
    #   classes
    #   if(!is.na(classes)){
    #     #checkXPath(tagNameInsert, AmtElemBefore = NoClass$AmtElemFound, class = classes[1])
    #     WithClasses <- lapply(
    #       X = classes,
    #       FUN = checkXPath,
    #       tags = tags,
    #       tagNameInsert = tagNameInsert,
    #       AmtElemBefore = NoClass$AmtElemFound,
    #       allText = allText
    #     ) %>% do.call(what = rbind)
    #     WithClasses
    #     WithClasses %<>% dplyr::filter(nchar(class) > 0)
    #     WithClasses
    #     if(nrow(WithClasses)){
    #       tagNameInsert <- glue("{tagName}[contains(@class, '{WithClasses$class}')]")
    #     }
    #   }
    # }

    tags <- c(tags, tagNameInsert)
    iterTag <- iterTag %>% html_nodes(xpath = "..")
    XPathFound <- !length(iterTag) & XPathFound
  }

  #todo: sometimes a tag "text" comes up, that i dont want. However, if i debug nike jobsite
  # tag script does not come up, but i want it.
  tags <- tags[magrittr::not(tags %in% c("text", ""))]
  xpath <- paste(c("", tags[length(tags):1]), collapse = "/")
  xpath
}




getXPathDeprecated <- function(url, text = NULL, xpath = NULL, exact = FALSE, doc = NULL){
  tagName <- ""
  tags <- c()
  if(!is.null(text)){
    text <- gsub(pattern = " |\n|\t", replacement = "", tolower(text))
    if(exact){
      xpath <- paste0("//*[text() = '", text,"']")
    }else{
      xpath <- paste0("//*[text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '",
                      text, "')]]")
    }
  }

  if(is.null(doc)) doc <- read_html(x = url)
  tag <- doc %>% html_nodes(xpath = xpath)
  #tag[1] %>% html_text()
  if(!length(tag)){
    xpath = ""
    #showHtmlPage(doc)
  }else{
    if(length(tag) > 1){
      tagNames <- sapply(tag, html_name)
      match <- which(tagNames != "script")
      if(!length(match)) match <- 1
      tag <- tag[match[1]]
    }
    while(tagName != "html"){
      tagName <- tag %>% html_name()
      tags <- c(tags, tagName)
      tag <- tag %>% html_nodes(xpath = "..")
    }

    xpath <- paste(c("", tags[length(tags):1]), collapse ="/")
  }
  xpath
}


getXPathByTagDeprecated <- function(tag, text, exact = FALSE){
  tagName <- ""
  tags <- c()

  if(!length(tag)){
    xpath = ""
  }else{
    if(length(tag) > 1){
      tagNames <- sapply(tag, html_name)
      match <- which(tagNames != "script")
      if(!length(match)) match <- 1
      tag <- tag[match[1]]
    }
    while(tagName != "html"){
      tagName <- tag %>% html_name()
      tags <- c(tags, tagName)
      tag <- tag %>% html_nodes(xpath = "..")
    }

    xpath <- paste(c("", tags[length(tags):1]), collapse ="/")
  }
  xpath
}


getScrapeInfo <- function(browserOutputRaw = readClipboard(), toLower = TRUE){
  splitted <- strsplit(
    x = browserOutputRaw,
    split = ";"
  )[[1]]

  if(splitted[[1]] != "SivisCBDataIdent"){
    print(
      paste0(
        "No valid sivis data on clipboard. Found instead: ", splitted[[1]],
        "No valid sivis data on clipboard. Found instead output above."
      )
    )
  }

  browserOutput <- list(
    url = splitted[2],
    clickType = splitted[3],
    expectedOutput = strsplit(splitted[4], split = "~~~")[[1]],
    links = strsplit(splitted[5], split = "~~~")[[1]],
    XPath = splitted[6],
    XPathBlank = gsub(
      pattern = "[[]\\d+[]]",
      replacement = "",
      x = splitted[6]
    ),
    XPathClasses = splitted[7]
  )
  if(toLower){
    browserOutput <- lapply(X = browserOutput, FUN = tolower)
    # url is case sensitive
    browserOutput$url <- splitted[2]
  }
  browserOutput
}

matching <- function(rvestOut, browserOutput){
  percentage <- sum(rvestOut %in% browserOutput$expectedOutput)/length(rvestOut)
  ident <- identical(x = rvestOut, y = browserOutput$expectedOutput)
  c(percentage, ident)
}


getRvestText <- function(url, XPath){
  #url <- tolower(url)
  read_html(x = url) %>%
    html_nodes(xpath = XPath) %>%
    html_text()
}

getRvestClean <- function(url, XPath){
  rvestOutRaw <- getRvestText(
    url = url,
    XPath = XPath
  )
  if(!length(rvestOutRaw)) rvestOutRaw <- ""

  gsub(
    pattern = "\n",
    replacement = "",
    x = rvestOutRaw
  )
}


getRvestHtmlSource <- function(url){
  textRaw <- read_html(x = url) %>% html_nodes(xpath = "//*[not(*) and not(self::script)]") %>% html_text()
  text <- paste(textRaw, collapse = "\n")
  text
}


getRvestHref <- function(url, XPath){
  read_html(x = url) %>%
    html_nodes(xpath = XPath) %>%
    html_attr(name = "href")
}


rvestScraping <- function(nr){
  url <- scraper[[nr]]$url
  XPath <- scraper[[nr]]$jobNameXpath

  rvestOutRaw <- getRvestText(
    url = url,
    XPath = XPath
  )

  rvestOut <- gsub(
    pattern = "\n",
    replacement = "",
    x = rvestOutRaw
  )

  rvestOut
}


getJsonIndexes = function(x, sep = ".") {
  names(x) = paste0(seq_along(x))
  while(any(sapply(x, class) == "list")) {
    ind = sapply(x, class) == "list"
    temp = unlist(x[ind], recursive = FALSE)
    names(temp) = paste0(rep(names(x)[ind], lengths(x[ind])),
                         sep,
                         sequence(lengths(x[ind])))
    x = c(x[!ind], temp)
  }
  return(x)
}


ChangeOnString1 <- function(string1, string2){
  levenshteinRaw <- adist(string1, string2, count = TRUE)
  actionRaw = strsplit(attr(levenshteinRaw, "trafos"), '')[[1]]
  actionOnString1 <- actionRaw[actionRaw != "I"]
  WhichChangedString1 <- which(actionOnString1 %in% c("S", "D"))
  WhichChangedString1
}

getIndex <- function(string1, strings){
  allModifiedIndex <- sapply(X = strings, FUN = ChangeOnString1, string1 = string1, USE.NAMES = FALSE)
  keep <- lengths(allModifiedIndex) < 5
  allModifiedIndex <- allModifiedIndex[keep]
  Uniqueindexes <- sort(unique(unlist(allModifiedIndex)))
  valRaw <- paste(strsplit(x = string1, split = "")[[1]][Uniqueindexes], collapse = "")

  val <- as.numeric(gsub(pattern = "[/]|#", replacement = "", x = valRaw))
  list(
    val = val,
    idx = Uniqueindexes
  )
}


IterableLinkGenerator <- function(links, envir = .GlobalEnv){
  dupes <- duplicated(links, fromLast = FALSE)  #| duplicated(links, fromLast = TRUE)
  strings <- links[!dupes]
  lengthsStrings <- nchar(strings)
  keepString <- abs(lengthsStrings - median(lengthsStrings)) < 3
  strings <- strings[keepString]

  getIndex(string1 = strings[1], strings = strings)
  toReplace <- lapply(X = strings, FUN = getIndex, strings = strings)
  num <- sapply(toReplace, "[[", "val")
  idx <- sapply(toReplace, "[[", "idx")
  tbl <- table(abs(diff(num)))
  counter <- names(which.max(tbl))

  if(counter == "1"){
    multiplier <- "nr*"
  }else{
    multiplier <- "(nr - 1)*"
  }

  str1 <- paste0('urlGen <- function(nr) paste0("', substr(x = strings[1], start = 1, stop = (min(idx[[1]]) - 1)), '", ', multiplier, counter)

  middlePart <- ""
  stringLength <- nchar(strings[1])
  matchIndexMax <- max(idx[[1]])
  HaveToAddStringAtEnd <- stringLength > matchIndexMax
  if(HaveToAddStringAtEnd){
    middlePart <- paste0(', "', substr(x = strings[1], start = max(idx[[1]]) + 1, stop = nchar(strings[1])), '\"')
  }

  str1 <- paste0(str1, middlePart, ")")
  eval(parse(text = str1), envir = envir)
  urlGen
}


IterableJsonName <- function(links, getResult){
  dupes <- duplicated(links, fromLast = FALSE)  #| duplicated(links, fromLast = TRUE)
  strings <- links[!dupes]
  lengthsStrings <- nchar(strings)
  keepString <- abs(lengthsStrings - median(lengthsStrings)) < 3
  strings <- strings[keepString]

  toReplace <- lapply(X = strings, FUN = getIndex, strings = strings)
  num <- sapply(toReplace, "[[", "val")

  #todo;3;3;serious refactoring - maybe as additional function
  splittedRaw <- list()
  for(nr in 1:length(links)){
    splittedRaw[[nr]] <- strsplit(x = links[nr], split = num[nr])[[1]]
  }
  asString <- sapply(splittedRaw, FUN = paste, collapse = "|")
  tbl <- table(asString)
  winner <- names(tbl)[which.max(tbl)]
  winnerIdx <- which(asString == winner)
  splitted <- splittedRaw[[winnerIdx[1]]]

  subset <- as.numeric(strsplit(x = splitted[1], "[.]")[[1]])
  s <- subsetByArray(lst = getResult, arr = as.list(subset))
  maxIter <- length(s)

  text <- paste0(splitted[1], 1:maxIter, splitted[2])
  eval(parse(text = paste0("func <- function(nr) paste0('",splitted[1], "', 1:nr, '", splitted[2], "')")))

  list(
    text = text,
    func = func,
    subset = subset
  )
}


subsetByArray <- function(lst, arr){
  nr <- 2
  names(lst)
  for(nr in 1:length(arr)){
    lst <-  lst[[arr[[nr]]]]
  }
  lst
}

subsetByStr <- function(lstRaw, arr){
  nrr <- 1
  nr <- 1
  lst <- lstRaw
  base <- NULL
  baseFollow = NULL
  for(nr in 1:length(unlist(arr$str))){
    hasName <- !is.null(names(lst))
    if(hasName){
      subsetBy <- arr$str[nrr]
      nrr <- nrr + 1
    }else{
      subsetBy <- 1 #arr$iter
      base <- arr$str[1:(nrr - 1)]
      baseFollow <- arr$str[nrr:length(arr$str)]
    }
    lst <-  lst[[subsetBy]]
    if(!is.null(baseFollow)) lst <- lst[[baseFollow]]
  }
  if(is.null(base)) base <- arr$str
  list(
    base = base,
    baseFollow = baseFollow,
    lst = lst
  )
}

subsetByStr2 <- function(lstRaw, arr){
  lst <- lstRaw
  nr <- 1
  for(nr in 1:length(arr)){
    lst <-  lst[[arr[nr]]]
  }
  lst
}

subsetByStr3 <- function(lstRaw, arr){
  lst <- lstRaw
  nr <- 1
  for(nr in 1:length(arr)){
    lst <-  lst[[arr[nr]]]
    if(!length(lst)) return("") # missing element return empty string
    if(is.null(names(lst))) lst <- lst[[1]]
  }
  lst
}


# targetValue <- targetValues[4]
# targetValue <- "i Kontrolingu (M/F)"
matchStrings <- function(targetValue, candidates){

  directMatch <- which(candidates == targetValue)
  distances <- adist(x = candidates, y = targetValue) %>% c
  relDistances <- (distances / min(nchar(targetValue), nchar(candidates)) < 0.05) %>% which

  if(length(directMatch)){
    winnerIndex <- directMatch
  }else if(length(relDistances)){
    warning(glue("Only approximate match for targetValue: '{targetValue}'. For string length: {nchar(targetValue)}."))
    winnerIndex <- relDistances
  }else{
    winnerIndex <- grep(pattern = targetValue, x = candidates, fixed = TRUE)
  }

  #winnerIndex <- matchIdx[which.max(candidates[matchIdx] %>% nchar)]
  # targetInCand <- which(lengths(sapply(candidates, grep, pattern = targetValue, fixed = TRUE)) > 0)
  # candInTarget <- which(lengths(sapply(candidates, grep, x = targetValue, fixed = TRUE)) > 0)
  # matchIdx <- c(candInTarget, targetInCand)
  # if(!length(winnerIndex)){
  #   distances <- adist(candidates, targetValue)
  #   minDist <- min(distances, na.rm = TRUE)
  #
  #   winnerIndex <- c("levenDist" = which.min(distances))
  # }
  # todo: check how to use. Currently hardly in use, because of approximate string match above.
  if(!length(winnerIndex)){
    warning(glue("no match for targetValue: {targetValue}"))
    return(NULL)
  }
  names(winnerIndex) <- "directMatch"
  return(winnerIndex)
}

# todo: make better for lufthansa: https://career.be-lufthansa.com/?
# have multiple targetkeys - have to account for numbers and possibly also more values.
allJSONValues <- function(jsonContent, targetValues){
  jsonContentFlat <- unlist(jsonContent)

  hasNames <- jsonContentFlat %>% names %>% is.null
  if(hasNames){
    stop("content can not be parsed - transformation to data.frame failed. Require json that can be parsed to data.frame as input")
  }
  splitNames <- names(jsonContentFlat) %>% strsplit(split = "[.]")
  lastKeys <- sapply(X = splitNames, FUN = tail, n = 1)

  targetValues <- targetValues %>% gsub(pattern = "\n|\t", replacement = "") %>% trimws
  candidates <- jsonContent %>% unlist() %>% trimws

  matchIdx <- sapply(targetValues, matchStrings, candidates = candidates) %>%
    unlist %>%
    unname
  # matchIdxRaw <- sapply(targetValues, FUN = function(targetValue){
  #   which(trimws(unlist(jsonContent)) == trimws(targetValue))
  # }, USE.NAMES = FALSE)
  # matchIdx <- matchIdxRaw %>% unlist %>% unname


  if(!length(matchIdx)){
    stop(
      paste0("No match for targetValues:'", paste(targetValues, collapse = " | "),"' in json.")
    )
  }

  targetKeys <- lastKeys[matchIdx %>% as.numeric()]
  targetKey <- targetKeys %>%
    table %>%
    which.max %>%
    names
  texts <- jsonContentFlat[lastKeys %in% targetKey] %>% unname

  # todo: how do i know if i have an html below or close match
  # check for html is not possible, because plain text is also html.
  # Possible options would include:
  # - check for text length
  # - check if there are plently <>.
  ### --> currently decided for <> count

  ##### Alternative
  # docLength <- nchar(texts) %>% sum
  # textsLength <- nchar(targetValues) %>% sum


  # config parameter
  isLargeHTML <- texts %>% stringr::str_count(pattern = "<") %>% is_greater_than(5)
  if(all(isLargeHTML)){ # all or any?
    return(
      list(
        isLargeHTML = TRUE,
        targetKey = NULL,
        texts = texts,
        neighbours = NULL,
        base = targetKey,
        baseFollow = NULL
      )
    )
  }

  if(length(texts) < length(targetValues)){
    targetKeysSlim <- gsub(x = targetKeys, pattern = "[0-9]*", replacement = "") %>% unique
    numbers <- gsub(pattern = targetKeysSlim, replacement = "", x = targetKeys) %>% as.numeric
    seqNumber <- numbers %>% sort %>% diff %>% table %>% which.max %>% names %>% as.numeric
    if(!length(seqNumber)){
      targetKey <- targetKeys %>% unique
    }else{
      amtSlimKeys <- targetKeysSlim %>% length
      if(amtSlimKeys == 1){
        targetKey <- paste0(targetKeysSlim, "[0-9]*$")
      }else{
        stop("Have too many targetKeys while extracting targetValues in JSON.")
      }
    }
    # if(seqNumber > 1){
    #   #todo: might add regex divisible by 3. ask #SO question about that
    #   # grep(pattern = "^([0369]|[147][0369]*[258]|(([258]|[147][0369]*[147])([0369]|[258][0369]*[147])*([147]|[258][0369]*[258])))+$", x = 3, fixed = TRUE)
    # }
  }

  # to decide: TradeOff here between direct match and fuzzy match.
  # example: https://pfizer.wd1.myworkdayjobs.com/PfizerCareers. hast key: text and text1, text2, text3, where the latter are not of interest.
  idx <- which(targetKey == lastKeys)
  lastKeys[idx]
  idx <- grepl(pattern = targetKey, x = lastKeys) %>% which
  lastKeys[idx]
  texts <- jsonContentFlat[idx] %>% unname
  print(texts)
  #idx <- c(which(texts == targetValue))
  parentElemNmsRaw <- jsonContentFlat %>% names %>% .[matchIdx[1]] %>% strsplit(split = "[.]") %>% unlist
  match <- c(parentElemNmsRaw %>% head(n = -1))

  arr <- list(str = match, iter = matchIdx[1])
  neighbours <- subsetByStr(lstRaw = jsonContent, arr = arr)

  list(
    targetKey = targetKey,
    texts = texts,
    neighbours = neighbours$lst[[1]],
    base = neighbours$base,
    baseFollow = neighbours$baseFollow
  )
}



GetRequest <- function(getRes, browserOutputRaw, extractPathes){

  print(paste0("Status code is: ", http_status(getRes)$message))
  if(getRes$status_code != 200){
    print("Bad request: ")
    return(NULL)
  }
  if(getRes$headers$`content-type` != "application/json; charset=utf-8"){
    warning("Reponse is not of type JSON")
  }

  #content(getRes) %>% showHtmlPage
  bo <- browserOutputRaw #getScrapeInfo(toLower = FALSE)

  contInit <- getRes %>% content(type = "text", encoding = "UTF-8")
  cont <- contInit

  # todo: could not only be the first one, but one of the previous?
  extractionType <- names(extractPathes[1]) %>% toString
  if(extractionType == "scriptJsonIndex"){
    idx <- extractPathes[[1]]
    cont %<>% gregexpr(
      pattern = "\\{(?:[^{}]+|(?R))*?\\}",
      perl = TRUE,
      text = str
    ) %>%
      regmatches(x = str) %>%
      unlist %>%
      .[1]

    isJSON <- TRUE
  }else{
    isJSON <- jsonlite:::validate(cont)
  }

  isJSON
  if(!isJSON){
    # Example: https://www.dmk.de/karriere/stellenangebote/?no_cache=1.
    doc <- tryCatch(xml2::read_html(cont), error = function(e) return(NULL))
    #doc %>% showHtmlPage
    targetString <- gsub(pattern = " |\n|\t", replacement = "", tolower(bo$selectedText))
    if(!is.null(doc)){
      # toDo: could make this a function?
      xpath <- paste0("//text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", targetString,"')]")
      tags <- list(html_nodes(doc, xpath = paste0("//*[contains(text(), '", bo$selectedText[2],"')]")))
      exact = FALSE
      byIndex = TRUE
      allText <- bo$selectedText
      xpath = sapply(
        tags,
        FUN = getXPathByTag,
        byIndex = byIndex,
        exact = exact,
        doc = doc,
        allText = allText
      )
      extractedText <- doc %>% html_nodes(xpath = xpath) %>% html_text
      all(allText %in% extractedText)
      jsonlite:::validate(extractedText)

    }else{
      stop("Did not find JSON nor HTML.")
    }
    # warning("Custom message: Did not get a json. Try to change text to get a json.")
    # extractedJSONRaw <- str_match(cont, "\\(\\{(.*?)\\}\\)")
    # cont <- glue("{{{extractedJSONRaw[, 2]}}}")
    # isJSON <- validate(cont)
    # if(!isJSON){
    #   getXPathByText(text, doc, exact = FALSE, attr = NULL, byIndex = FALSE)
    #   contInit %>% showHtmlPage()
    #
    #   tags <- list(html_nodes(docs, xpath = paste0("//*[contains(text(), '", targetString,"')]")))
    # }else{
    #   m2 <- lapply(docs, html_nodes, xpath = paste0("//*[contains(text(), '", targetString,"')]"))
    #   tags <- m2[lengths(m2) > 0]
    #   docs <- docs[lengths(m2) > 0]
    # }
    # xpath = sapply(tags, getXPathByTag, text = targetString, exact = FALSE)
    #
    # contentGET <- cont %>% fromJSON
    # gg <- allJSONValues(jsonContent = contentGET, targetValues = bo$selectedText)
  }else{
    #contentGET <- getRes %>% content
    # if(typeof(contentGET) == "list"){
    #   index <- sapply(bo$selectedText, grepl, contentGET) %>% rowSums %>% which.max
    #   doc <- tryCatch(xml2::read_html(contentGET[[index]]), error = function(e) return(NULL))
    #   #doc %>% showHtmlPage
    #   if(!is.null(doc)){
    #     # toDo: could make this a function?
    #     xpath <- paste0("//text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", targetString[1],"')]")
    #     tags <- list(html_nodes(doc, xpath = xpath))
    #     xpath = sapply(tags, getXPathByTag, exact = FALSE, doc = doc, allText = targetString)
    #     return(
    #       reponse = c("list", "xpath"),
    #       xpath = xpath
    #     )
    #   }
    # }else{
    #todo: see for better option than checkcing only first value of bo$expectedOutput[1]
    jsonContent = cont %>% jsonlite::fromJSON()
    targetValues = bo$selectedText
    gg <- allJSONValues(
      jsonContent = jsonContent,
      targetValues = targetValues
    )
    return(
      list(
        reponse = "json",
        isLargeHTML = gg$isLargeHTML,
        neighbours = gg$neighbours,
        texts = gg$texts,
        targetKey = gg$targetKey,
        base = gg$base,
        baseFollow = gg$baseFollow
      )
    )
    # }
    # todo: html selection is not clean. GO over depth of structure: Or better over response-header
    # hasHTMLContent <- sum(str_count(string = contentGET, pattern = "<.*>")) > 0
    # if(hasHTMLContent){
    #   assign(x = "returnMessage", value = contentGET, envir = .GlobalEnv)
    #   print("Return message could include HTML - check variable returnMessage (in .globalEnv) for details.")
    #   warning("Return message could include HTML - check variable returnMessage (in .globalEnv) for details.")
    # }
  }
}

##### e.g for httpsjobsapiinternalmcloudioapijobcallbackjobsCallbackoffset49sortfieldopen_datesortorderdescendingfa
# desired functionality for json with html:
# getRes <- GET(url = url)
# if(getRes$status_code != 200) return(NULL)
# contentGET <- content(getRes)
# res <- contentGET[[targetKey]]
# #res %>% showHtmlPage

### does not work because of below:
##baseElems <- contentGET[[1]]
# raw <- sapply(baseElems, function(baseElem){
### --> have to find out why i need that and how to combine it with my other spec.
#contentGET[[base]][[INDEX]][[baseFollow]][[targetKey]]
#contentGET[["jobs"]][[1]][["title"]]

### --> or i make two seperate functions? one for json and one for multiple extractionpathes??

scheduledGET <- function(url, targetKeys, base, baseFollow = NULL, isLargeHTML = FALSE, jsonFromString = NULL){
  if(is.null(base)){
    stop("Parameter base, provided to scheduledGET(), is NULL - please provide a valid subset value.")
  }

  getRes <- GET(url = url)
  if(getRes$status_code != 200) return(NULL)
  contentGET <- content(getRes, type = "text")

  if(!is.null(jsonFromString)){
    contentGET %<>% gregexpr(
      pattern = "\\{(?:[^{}]+|(?R))*?\\}",
      perl = TRUE
    ) %>%
      regmatches(x = contentGET) %>%
      unlist %>%
      .[1]
  }

  contentGET %<>% jsonlite::fromJSON()
  contentGETFlat <- unlist(contentGET)

  splitNames <- names(contentGETFlat) %>% strsplit(split = "[.]")
  lastKeys <- sapply(X = splitNames, FUN = tail, n = 1)

  # todo;do i neeed two of these subsetbystr functions?
  if(is.null(base)) base <- NA
  if(any(is.na(base))){
    baseElems <- contentGET[[1]]
  }else{
    baseElems <- subsetByStr2(contentGET, base)
  }

  if(isLargeHTML) return(
    list(
      res = baseElems,
      base = base, # what do i need these for?
      targetKeys = targetKeys # what do i need these for?
    )

  )
  if(!length(baseElems)) return(NULL)
  targetKey <- targetKeys[[1]]
  texts <- lapply(targetKeys, function(targetKey){
    baseElem <- baseElems[[1]]
    raw <- sapply(baseElems, function(baseElem){
      if(!is.null(baseFollow)){
        baseElem <- subsetByStr3(lstRaw = baseElem, arr = baseFollow)
      }
      subsetByStr3(lstRaw = baseElem, arr = targetKey)
    }, USE.NAMES = FALSE)
    raw2 <- sapply(raw, paste, collapse = " | ") %>% unname
    if(!length(raw2)) return(raw2)
    df <- data.frame(raw2, stringsAsFactors = FALSE)
    df <- setNames(df, paste(targetKey, collapse = "|"))
    df
  })

  res <- do.call(what = cbind, texts)
  colnames(res) <- targetKeys
  rownames(res) <- NULL

  list(
    res = res,
    base = base, # what do i need these for?
    targetKeys = targetKeys # what do i need these for?
  )
}


isHtmlDoc <- function(elem){
  all(names(elem) %in% c("node", "doc"))
}

getHTML <- function(getURL, targetString){
  req <- httr::GET(getURL)
  response <- content(req)
  m <- str_count(string = response, pattern = "<.*>")
  if(length(m) > 1){
    docs <- lapply(response[m > 0], read_html)
  }else{
    docs <- response
  }

  if(isHtmlDoc((response))){
    docs <- response
    tags <- list(html_nodes(docs, xpath = paste0("//*[contains(text(), '", targetString,"')]")))
  }else{
    m2 <- lapply(docs, html_nodes, xpath = paste0("//*[contains(text(), '", targetString,"')]"))
    tags <- m2[lengths(m2) > 0]
    docs <- docs[lengths(m2) > 0]
  }

  list(
    xpath = sapply(tags, getXPathByTag, text = targetString, exact = FALSE),
    docs = docs
  )
}

scrapeGetHtml <- function(getUrl, res){
  htmlDoc <- getURL %>% GET %>% content
  subsetBy <- names(res$xpath)
  if(!is.null(subsetBy)) htmlDoc <- htmlDoc %$% get(subsetBy) %>% read_html
  texts <- htmlDoc %>% html_nodes(xpath = unname(res$xpath)) %>% html_text

  list(
    texts = texts
  )
}

showHtmlPage <- function(doc){
  tmp <- tempfile(fileext = ".html")
  doc %>% toString %>% writeLines(con = tmp)
  tmp %>% browseURL(browser = rstudioapi::viewer)
}

createHeaders <- function(headersRaw){
  headerValueLabel <- (headersRaw) %>%
    strsplit(split = "\n") %>%
    lapply(strsplit, split = ": ") %>%
    unlist(recursive = FALSE)

  header <- sapply(headerValueLabel, "[", 2)
  names(header) <- sapply(headerValueLabel, "[", 1)

  header[is.na(header)] <- ""
  return(header)
}

createBody <- function(bodyRaw){
  bodyValueLabel <- (bodyRaw) %>%
    strsplit(split = "\n") %>%
    lapply(strsplit, split = ": ") %>%
    unlist(recursive = FALSE)

  body <- lapply(bodyValueLabel, "[", 2)
  names(body) <- sapply(bodyValueLabel, "[", 1)

  header[is.na(body)] <- ""
  return(body)
}

createDocumentGET_deprecated <- function(url, cbdata, getRes, extractPathes = NULL){
  rstudioapi::documentSave(id = "Notebook_scraping.Rmd")

  #getRes %>% showHtmlPage
  #getRes %>% toString
  sivis$browserOutputRaw <- cbdata
  browserOutputRaw <- cbdata
  sivis$initGET <- GetRequest(
    getRes = getRes,
    browserOutputRaw = browserOutputRaw,
    extractPathes = extractPathes
  )

  sivis$targetKeys <-  list(sivis$initGET$targetKey)

  #todo; make better
  extractType <- extractPathes[1] %>% names %>% toString
  if(extractType == "scriptJsonIndex"){
    idx <- extractPathes[[1]]
    jsonFromString <- TRUE
  }else{
    jsonFromString <- NULL
  }

  additionalExtractions <- create_Addit_Extract(extractPath = extractPathes)

  createDocumentGETW(jsonFromString = jsonFromString, additionalExtractions = additionalExtractions)
}


anonymise <- function(str){
  responseString2 <- gsub(pattern = 1:9 %>% paste(collapse = "|"), replacement = 3, x = str)
  responseString2 <- gsub(pattern = c("a", "e", "i", "o") %>% paste(collapse = "|"), replacement = "e", x = responseString2)
  gsub(pattern = setdiff(x = letters, y = c("a", "e", "i", "o", "u", "n", "l")) %>% paste(collapse = "|"), replacement = "m", x = responseString2)
}



### FEHLERDATENBANK:
# Cant get the data in clipboard cache. --> Restart Chrome :(
#https://karriere.rewe.de/jobs/suche?term=


