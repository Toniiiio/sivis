
# bitlocker features
# - in .rmd joined all output to result
# - joined all pagechange to one function

#### new potential job source
## scale over company id
##  "https://jobsapi-internal.m-cloud.io/api/job?callback=jobsCallback&offset=11&sortfield=open_date&sortorder=descending&Limit=10&Organization=1909"
#https://gatesfoundation.wd1.myworkdayjobs.com/Gates

### Randbedingungen
# - nur Textdaten
# - wenn mehrere Daten/Seiten, auf 2-X Seite gehen, weil dann alle Parameter da sind.

# multipage errors:
# https://careers.l3harris.com/search-jobs/

# json target keys not added:
# https://careers.wm.com/professional/us/en/search-results?from=50&s=1

# collect links
#https://careers.moodys.com/jobs?jobpage=2

# split in rootpath causes extra rows
#https://careers.moodys.com/jobs?jobpage=2

# Problem in allJSONValues - variable baseCandidates - but did not find a good counter example yet.
# need a json response with mutliple base candidate ("rows")
# https://jobs.aon.com/api/jobs?page=2&sortBy=relevance&descending=false&internal=false

# search_Parent_Nodes takes ages for ralph-lauren - too many subnodes?


# cant find follow pages but have data
# https://paychex.recsolu.com/job_boards/bMMxAXjFgvU0PrJcH0lNgw

#https://www.teleflex.com/emea/de/careers/index.html
#todo:leafpathes
#"https://www.royalcareersatsea.com/jobs/results/page:2"
#https://lifeatexpediagroup.com/jobs/?&page=2


# aufbröseln von c() bei jsons?
#https://pc00.paycomonline.com/v4/ats/web.php/jobs?&clientkey=A38173AIE92874820ALRE20847CDE927PIW76526#
# https://edel.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX/requisitions
#https://kelloggs.taleo.net/careersection/2/jobsearch.ftl?lang=en&alt=1&_ga=2.84979033.1745910250.1583623002-1023856734.1583623002#
# dm !! complicated in request
##https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25678&siteid=5275#keyWordSearch=&locationSearch=

#missing addcols`
#"https://www.essexapartmenthomes.com/careers/jobs" DONE - now sivis fails
#https://careers.questdiagnostics.com/en-US/search?pagenumber=2 400 status code
#https://careers.leidos.com/search/jobs/in?page=2# # done, but have strange rows and have to add (no children for first)
#https://jobs.fcx.com/search/?searchby=location&createNewAlert=false&q=&locationsearch=&geolocation= # done, but additional crap
#https://kcsouthern.silkroad.com/epostings/index.cfm?fuseaction=app.jobsearch # fixed
#https://careers.dovercorporation.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=25 # adpated that grepl doubles is done on leaf not of grandparent, not sure if had to but akes more sense
#https://careers.key.com/en-US/search?pagenumber=2 400 status code: https://careers.key.com/en-US/search?pagenumber=2
# https://careers.firstrepublic.com/index.php?keyword=&search-openings=Search+Openings# klappt jetzt
# http://www.bestbuy-jobs.com/search/?sort=job_title&pg=6 # fixed
# http://jobs.prudential.com/job-listing.php?keyword=&jobType=&location=&jobLabel=&jobLocation= # request fails


# pagechange for getrequest - open
# https://careers.equifax.com/global/en/c/technology-jobs?from=20&s=1
# avoid divide by 2: "https://jobs.nscorp.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=12.5"

######### pagechange in url - works now at least in bitlock version
#https://careers.leidos.com/search/jobs/in?page=2#
#https://careers.questdiagnostics.com/en-US/search?pagenumber=2 sivis fails now
#"https://www.royalcareersatsea.com/jobs/results/page:2" works now
#https://lifeatexpediagroup.com/jobs/?&page=2 works now



### browser + server response differ - check showhtmlpage
# https://jobs.fcx.com/search/?searchby=location&createNewAlert=false&q=&locationsearch=&geolocation=


####access denied


###### 422 Unprocessable entities
# https://jobs.marriott.com/marriott/jobs?page=1&limit=100
# https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/422
# rewe.de captcha

####loads popup text only or wrong request? --> wrong request
#https://www.governmentjobs.com/careers/cincinnati?page=2
#https://careers.equityapartments.com/ListJobs/All/Page-2
# https://careers.regeneron.com/search-results?from=100&s=1

##### false candidates need classes or check children. but cant



# https://www.aldi-nord.de/angebote/aktion-mo-03-02.html
# https://www.rewe.de/angebote/hamburg/540724/rewe-markt-waldweg-43/


## custom seperator
#https://evergy.taleo.net/careersection/evergy_external_career_section/jobsearch.ftl?lang=en&portal=8360483464

# xpath fails on second try
# https://careers-aflac.icims.com/jobs/search?pr=1&searchRelation=keyword_all&schemaId=&o=

# Anforderungen, Beispiele für Funktionen sammeln.
# substr321 entsprechend umbenennen umschreiben
# Automatisches testen für die kreierung von den scrapern bauen.

# invalid access 403: https://careers.westerndigital.com/jobs/search/4107581/page2
#https://www.rewe.de/angebote/hamburg/540724/rewe-markt-waldweg-43/
# empty site: https://recruiting.adp.com/srccar/public/RTI.home?c=1214601&d=ExternalCareerSite

# Notation:
# Initial scrape: Data collection for building the sivis scraping code.
# Scheduled scrape: Automatic scrape, that will be done based on the code created by sivis80.

## General challenges:
# Mixed requests(two) sources: First page html, second page xhr request. Example: https://jobs.disneycareers.com/search-jobs
# https://jobs.gartner.com/search-jobs
#https://newscorp.com/careers/search?s=?
#https://vulcanmat.recsolu.com/job_boards/DSwKIMPhsVjPaaKajSs8hQ

###might not fully work
##peoples united, wrong extraction - can not work - or have to index in neighbour column steht jobtitle, https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25679&siteid=5313#keyWordSearch=&locationSearch=


#### wrong server result - incomplete json???
# https://henryschein.taleo.net/careersection/hsi1/moresearch.ftl?lang=en
#

###to much selected: deselect things
# https://kcsouthern.silkroad.com/epostings/index.cfm?fuseaction=app.jobsearch


# todo: inpage source reparieren und von get und post untersuchungen trennen: {"identifier":"SivisCBDataIdent","pageUrl":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","clickType":"contextmenu","selectedText":["\n            Mcclean, VA, US\n            \n        "],"links":[null],"XPath":"/html/body/div[2]/div[2]/div/div[4]/table/tbody/tr[1]/td[3]/span","XPathClassRaw":"/html[class = 'html5']/body[class = 'coreCSB search-page body']/div[class = 'outerShell']/div[class = 'innerShell']/div[class = 'content']/div[class = 'searchResultsShell']/table[class = 'searchResults full']/tbody/tr[class = 'dbOutputRow2 jobgrid-row']/td[class = 'colLocation']/span[class = 'jobLocation']","getUrl":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","postUrl":"","postBody":{"raw":"[object Object]"},"InPageSource":true}
# todo: https://www.clariant.com/de/Careers/Job-Openings/Global-Job-Openings
# todo: wrong get url for volkswagen
# todo: https://www.dmk.de/karriere/stellenangebote/?no_cache=1 --> mixed sources. First ones in document next in get requests.
# todo: Shiny hat krasse doppelungen, zb auc hbei volkswagen
# todo: Daimler geht net, findet den request net.
# todo: allianz failed: IST EIN GET, aber sivis findet ihn net - yy <- "https://jobs.allianz.com/sap/hcmx/hitlist_na?sap-client=100&sap-language=de" %>% GET - x <- POST(url = "https://careers.allianz.com/etc/clientlibs/onemarketing/career/paths/jobsearch.json", encode = "json", body = list(formData = list(cityID = "", companyID = "", countryID = "32760000",     functionalAreaID = "", hierarchyLevelID = "", language = "de_DE",     orderBy = "desc", resultSet = "9", searchLimit = "9")))
# todo: Linde multi rvest add xpathes und page change
#todo: sivis - cant click specific element
# sivis fails: https://jobboerse.arbeitsagentur.de/vamJB/stellenangeboteFinden.html?d_6827794_p=2&execution=e1s2 request is correct, but result is not the same as in chrome network activities. no server prevention as robotstxt is cleaned.
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
# cant get data also with 10+ tries: https://jobs.harley-davidson.com/search/?createNewAlert=false&q=&locationsearch=
# https://www.edeka.de/eh/angebote.jsp
# https://www.idealo.de/preisvergleich/ProductCategory/19116.html?q=handy&qd=handy
# https://www.preissuchmaschine.de/in-Handy/

########## individual baseFollow names
# fl <- "httpshiremyaviontecomsonarv2jobBoardQ16L3ooLDFQW1VE2wFUOsQ.RData"

##### right click opens new window, but without ctrl it works:
#https://pfizer.wd1.myworkdayjobs.com/PfizerCareers

# WRONG REQUEST in
# falscher request:
##https://www.dm.de/blend-a-med-zahnpasta-classic-p8001090272232.html
# https://mastercard.jobs/jobs/#3
# and https://www.att.jobs/search-jobs

#only for one job: https://careers.kronos.com/

# selects wrong column element in sivis - header value in column header
#https://tractorsupply.jobs/jobs/search#/results?keyword&jobcategory=&jobstate=&jobcity=&zipcode&zipradius=20&positiontype=Both&field24417=


### two seperate requests - two requests - html+get
# https://careers.ihsmarkit.com/search.php


### to learn:
# why does it only work without characters: https://www.zeit.de/index

### feature request for sivisChrome: problem becaues first element is html but not target. exclude elements
# https://www.cimarex.com/careers/current-opportunities/default.aspx --> netxt problem is request$requestUrl empty and does he take an old one then?
# ended up with url for pepsi?
# also up here: https://www.incyte.com/join-us/careers

# encoding:
# httpscareerslillycomsearchjobs - request ist korrekt aus R heraus. Encoding von sivis chrome ist falsch. seite --> https://careers.lilly.com/search-jobs

############encoding################# chinese encoding ???? characters
# chinese jobs: https://pfizer.wd1.myworkdayjobs.com/PfizerCareers
# in response from server they are fine. Just an error in targetValues.


###### cant get data:
# cant get data: https://emerson.taleo.net/careersection/ex/jobsearch.ftl?lang=de.
# cant get data for main page but once!!  i think follow pages: https://www.gd.com/en/careers/job-search
# cant get data: https://jobs.axa/careersection/1/moresearch.ftl?lang=de&portal=8105020509
#https://jobs.celgene.com/jobs?page=1
#https://acetalent.taleo.net/careersection/ace_external/jobsearch.ftl?lang=en#
#https://mondelez.avature.net/careers
#https://www.aetnacareers.com/search-jobs
#cant get for {microsoft jobs. then select only one - then selet multiple
# https://hella-jobs.dvinci.de/cgi-bin/appl/selfservice.pl?action=search;page=2

# TODO: DAVID KREISEL _ SCHMALES BUDGET _ RECHTSBEISTAND ONLINE

##double json
#https://jobs.gartner.com/search-jobs

### json validate fail find \n even if i remove it
#### tried additional json extraction, didnt help: https://stackoverflow.com/questions/59695961/find-json-in-string-with-recursion-limit-in-r-windows
#### tried to change single to double quote.
##https://mylan.taleo.net/careersection/myl_usajobs/jobsearch.ftl?lang=en&radiusType=K&location=26140430146&searchExpanded=true&radius=1&portal=101430233


#### multiple sources und grün wird nicht gut angezeigt:
#https://jobs.unitedrentals.com/search/searchjobs

# encoding issues:
### httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData

# still c stack errors / warnings:
### httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData
# https://careers.globelifeinsurance.com/jobs/jobs-by-category?


## multiple sources:
#https://ekkh.fa.us2.oraclecloud.com/hcmUI/CandidateExperience/en/sites/CX/requisitions

# double extract_json --> means i need multiple targetkeys?

# wrong request from sivisBrowser and data is (wrongly) in element attribute not in text:
# https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25477&siteid=5548#home


###todo: but found/match ratio --> "httpswwwpepsicojobscommainjobspage1.RData"
##--> how to deal with this?

########## seperate requests
#### one in html one in script
# https://n e w scorp.com/careers/search?s=

############ todo in get request keys #################
# https://careers.google.com/jobs/results/?company=Google&company=YouTube&hl=en&jlo=en-US&location=Z%C3%BCrich,%20Switzerland


### to long body string?
#https://www.coty.com/job-search

####### get back a png
####https://careers.slb.com/job-listing

# regularities
### https://careers.bat.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=10
### gsk https://jobs.gsk.com/en-gb/jobs?page=3 --> JIBE

#todo: https://www.tesco-careers.com/search-and-apply/?nodeId=1206&type=1&page=2

### cant select specific object on page:
#   https://www.yelp.de/search?find_desc=hotel&find_loc=Frankfurt+Am+Main%2C+Hessen&ns=1
# https://www.aldi-nord.de/angebote/aktion-mo-17-02.html`
# https://www.aldi-nord.de/produkte/neu-im-sortiment.html
# https://www.aldi-nord.de/produkte/neu-im-sortiment.html
# https://jobs.cardinalhealth.com/search/searchjobs

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

source("QueryParams/createNewUrl.R")
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

create_sivis <- function(cbData){

  sivis$headers <- cbData$request$request$headers %>%
    {setNames(object = .$value, nm = .$name)}

  hdr <- sivis$headers["accept-encoding"]

  if(grepl(pattern = "br", hdr)){

    message("Info: removing br(otli) as accepted encoding as its not supported by curl.")
    sivis$headers["accept-encoding"] <- gsub(pattern = ", br|br, ", replacement = "", x = hdr)

  }

  if(is.null(cbData$clipBoardText)) stop("no clipboard text")

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
  targetValues <- sivis$cbData$clipBoardText$selectedText

  if(resourceType %in% "png"){

    stop(glue("Wrong resource type: {resourceType}. File an issue with: resourceType = {resourceType},
              time: {Sys.time()}, source url = {url} and an indication if this issue would be reproducible at a later time or if your
              selectedText = {sivis$browserOutput$selectedText[1]},... will likely change over time."))

  }

  cbData$request$response

  responseType <- cbData$request$response$content$mimeType

  assign("url", value = url, envir = sivis)
  # if GET request was already performed in this session dont perform it again to avoid extensive amount of requests for the same server
  contactDetails <- NULL
  agentName <- paste(contactDetails, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36")
  #        user_agent(agentName)
  sivis$GETContents <- list()

  reqMethod <- sivis$cbData$request$request$method
  reqMethod

  alreadyScraped <- sivis$url %in% names(sivis$GETContents)


  # cant merge post and get request together even though they just differ in the request method.
  # could set non needed body for get request to NULL.
  # But dont know to set an empty get value yet, see https://stackoverflow.com/questions/59971870/how-to-set-empty-body-in-httr-get-request.

  if(reqMethod == "POST" & !alreadyScraped){

    sivis$headerBody <- sivis$cbData$request$request$postData$text
    # sivis$headerBody <- sivis$cbData$request$request$queryString

    # bdy <- sivis$headerBody
    # list(bdy$value)
    # c(setNames(object = bdy$value, nm = bdy$name))


    verboseLogName <- "verbose3.log"
    con <- file(verboseLogName)
    sink(file = con, append = TRUE)
    sink(con, append = TRUE, type = "message")

    sivis$GETContents[[sivis$url]] <- tryCatch(expr = POST(
      url = sivis$url,
      httr::add_headers(.headers = sivis$headers),
      body = sivis$headerBody,
      verbose(),
      timeout(6)
    ),
    error = function(e){
      warning(glue("GET request with headers failed with {e}. Will attempt request without headers next."))
      return(NULL)
    })

    sink()
    sink(type = "message")
    sivis$verboseLog <- verboseLogName %>% readLines %>% paste(collapse = "\n")
    unlink(verboseLogName)

    # config parameter - timeout = 6
    noHeader <- tryCatch(expr = POST(
      url = sivis$url,
      body = sivis$headerBody,
      verbose(),
      timeout(6)
    ),
    error = function(e){
      warning(glue("GET request without headers failed with {e}."))
      return(NULL)
    })

  }

  if(reqMethod == "GET" & !alreadyScraped){

    #config parameter
    sivis$GETContents[[sivis$url]] <- tryCatch(expr = GET(
      url = sivis$url,
      httr::add_headers(.headers = sivis$headers),
      timeout(6),
      verbose()
    ),
    error = function(e){
      warning(glue("GET request with headers failed with {e}. Will attempt request without headers next."))
      return(NULL)
    })

    # check if i need headers.
    #config parameter
    noHeader <- tryCatch(expr = GET(
      url = sivis$url,
      timeout(6),
      verbose()
    ),
    error = function(e){
      warning(glue("GET request without headers failed with {e}."))
      return(NULL)
    })

  }

  withHeaders <- sivis$GETContents[[sivis$url]]

  # cant compare contents
  # example: https://www.macysjobs.com/search-results?
  # identical(sivis$GETContents[[sivis$url]] %>% content, noHeader %>% content))
  # should not take status code, because status code can be 200 but with empty result
  # in the end i am interested if it contains the target values or not, see solution above

  #  # config parameter
  targetValues <- targetValues[1:min(10, length(targetValues))]


  headerContent <-  ifelse(test = is.null(withHeaders), yes = "", no = withHeaders %>% content(type = "text"))
  noHeaderContent <-  ifelse(test = is.null(noHeader), yes = "", no = noHeader  %>% content(type = "text"))

  # config parameter
  tvRatioTreshold <- 0.4
  withHeaderRatio <- sapply(
    X = targetValues %>% gsub(pattern = "\n|\t", replacement = "") %>% trimws,
    FUN = grepl,
    fixed = TRUE,
    x = headerContent,
    USE.NAMES = FALSE
  ) %>%
    {sum(.) / length(.)}

  withHeaderSuccess <- withHeaderRatio %>%
    magrittr::is_greater_than(tvRatioTreshold)

  # config parameter
  # funny exception that it works without headers only for https://www.zeit.de/index.
  noHeaderRatio <- sapply(
    X = targetValues,
    FUN = grepl,
    fixed = TRUE,
    x = noHeaderContent,
    USE.NAMES = FALSE
  ) %>%
    {sum(.) / length(.)}

  noHeaderSuccess <- noHeaderRatio %>%
    magrittr::is_greater_than(tvRatioTreshold)

  if(!noHeaderSuccess & !noHeaderSuccess){

    message(glue("Status code of a request without headers is: {noHeader$status_code}."))
    message(glue("Status code of a request with headers is: {withHeaders$status_code}."))

    message(glue("Amount of target values found - for request with headers: {withHeaderRatio}. Success treshold is (higher than) {tvRatioTreshold}."))
    message(glue("Amount of target values found - for request without headers: {noHeaderRatio}. Success treshold is (higher than) {tvRatioTreshold}."))

    assign(x = "noHeader", value = noHeader %>% content, envir = .GlobalEnv)
    assign(x = "withHeaders", value = withHeaders %>% content, envir = .GlobalEnv)

    message("Response body for the request with headers is saved to a variable named 'noHeader'. It can be inspected visually (as html) with `showHtmlPage(noHeader)`.")
    message("Response body for the request without headers is saved to a variable named 'withHeader'. It can be inspected visually (as html) with `showHtmlPage(withHeaders)`.")
    message("Executing `showHtmlPage(withHeaders)` now. It will be available in the viewer pane.")

    "showHtmlPage(withHeaders)" %>% parse(text = .) %>% eval
    message(glue("Targetvalues are: {targetValues %>% paste(collapse = '\n ')}."))

    stop("request failed with and without headers.")

  }

  if(!withHeaderSuccess){

    message("cant find target values in response body for request with headers")

  }

  onlyNoHeaderSuccess <- !withHeaderSuccess & noHeaderSuccess

  if(onlyNoHeaderSuccess){

    message("but found them attempting a request without headers,...")
    sivis$GETContents[[sivis$url]] <- noHeader
    sivis$useHeader <- FALSE

  }else{

    sivis$useHeader <- !noHeaderSuccess

  }

  # for saving browser data for reproducibility
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
    fl <-  "tr_j_httpscareersgooglecomapijobsjobsv1searchcompanyGooglecompanyYouTubehlenjloenUSlocationZC3BCrich2C20S.RData"

    load(file = paste0("R/fromWeb/", fl))
    targetValues <- sivis$browserOutput$selectedText
    XPathFromBrowser <- sivis$browserOutput$XPath
    pageUrl <- sivis$browserOutput$pageUrl

  }else{

    XPathFromBrowser <- cbData$clipBoardText$XPath
    targetValues <- sivis$cbData$clipBoardText$selectedText
    fileName <- paste0(file.path(getwd(), "R/fromWeb", substring(text = urlShort, first = 1, last = 101)), ".RData")
    print(fileName)
    save(sivis, file = fileName)
    return(sivis)

  }

}

# Targetvalues:
#   "Exploring the tightened EU CO2 emission standards for cars in 2020 – Why now selling an electric car can be worth an extra 18000€ for producers."
# changed to
# "Exploring the tightened EU CO2 emission standards for cars in 2020 &#8211; Why now selling an electric car can be worth an extra 18000€ for producers.",
# because they where not found with a direct match. But only with a fuzzy match using a fuzzy parameter of 0.1.
# Consider configuring that parameter if that change was inaccurate.

#flhere
####testqqq

if(FALSE){

  nr <<- 9
  # 59 und 8, 9?? raus
  for(nr in 18:27){

    # setwd("..")
    load("tests/testthat/types.RData")
    types <- sapply(types, "[[", "type")
    ww <- types[types == "text/html"] %>% names
    fl <- ww[nr]

    # fl <- list.files(path = "R/fromWeb/", pattern = ".RData")[nr]
    #
    # fl <-  "httpscareerskohlscomsearchresultsfrom12s1.RData"
    #
    nr <<- 99
    fl <- "httpsf5comdecareerssearchjobspage2.RData"

    load(file = paste0("R/fromWeb/", fl))
    if(sivis$useHeader %>% is.null) sivis$useHeader <- FALSE
    save(sivis, file = paste0("R/fromWeb/", fl))


    assign(x = "fileName", value = paste0("test", nr, ".Rmd"), envir = sivis)
    #sivis[["fileName"]] <- 1#paste0("test", nr, ".Rmd")
    testRun <- FALSE
    testEval <- FALSE
    use_sivis(sivis, testRun = testRun, testEval = testEval)
    tryCatch(
      expr = use_sivis(sivis, testRun = testRun, testEval = testEval),
      error = function(e) print(e)
    )



    Sys.sleep(3)
    # ff <- tryCatch(expr = use_sivis(sivis, testRun = testRun, testEval = testEval), error = function(e) print(e))
    # ff
  }
}


## fromChrome from chrome new scraper
createScraper <- function(){

  nr <<- 19

  host <<- "192.168.1.11"
  #host <- "77.182.65.66"
  # host <<- "10.22.20.120"

  sivis$cbData <- readClipboard() %>% jsonlite::fromJSON()
  sivis$cbData$clipBoardText$selectedText %>% head
  sivis$cbData$request$request$url

  cbData <- sivis$cbData
  sivis <- create_sivis(cbData)

  # testRun = TRUE
  # success <- use_sivis(sivis, testRun = testRun)
  # success
  testRun <- FALSE
  testEval <- FALSE
  #newscraper
  success <- use_sivis(sivis, testRun = testRun)

  success
}


#
# sivis$cbData$request
# #, encoding = "UTF-8"
# extract_meta$responseString2 <- getRes %>% content(type = "text/plain")
#
# grep(pattern = "&#8211;", x = extract_meta$responseString2)
# grep(pattern = "-", x = extract_meta$responseString2)

prepare_Extract <- function(sivis){

  print("sivis$browserOutput$pageUrl")
  print(sivis$browserOutput$pageUrl)

  scrapeAllowed <- robotstxt::paths_allowed(
    paths = sivis$browserOutput$pageUrl,
    warn = FALSE
  )

  if(!scrapeAllowed){

    domain <- urltools::domain(sivis$browserOutput$pageUrl)
    robots_URL <- paste0("https://", domain, "/robots.txt")

    base::message(
      glue::glue("robotstxt does not allow scraping this page, see {robots_URL}.")
    )

    user_choice <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to continue anyway?" #the page now?
    )

    if(user_choice == 1){

      #utils::browseURL(url = robots_URL)

    }else{

      return(NULL)

    }

  }





  extract_meta <- list()

  reqMethod = sivis$cbData$request$request$method
  cbdata = sivis$browserOutput
  if(!length(sivis$GETContents)) stop("empty get contents in sivis.")
  getRes = sivis$GETContents[[1]]

  body = sivis$headerbody
  headers = sivis$headers
  if(is.null(sivis$useHeader)) sivis$useHeader <- FALSE
  useHeader = sivis$useHeader

  status <- getRes %>% status_code()
  if(status != 200) glue("status code of server response is: {status}") %>% warning

  #### fl <-  "https://www.rewe.de/angebote/hamburg/540724/rewe-markt-waldweg-43/" example for non replacement of \n
  targetValues <- sivis$browserOutput$selectedText %>%  trimws #gsub(pattern = "\n", replacement = "") %>%

  extract_meta$responseString <- getRes %>% content(type = "text")
  if(is.na(extract_meta$responseString)) stop("response body from server is not available.")
  if(!nchar(extract_meta$responseString)) stop("response body from server seems to be empty.")
  #extract_meta$responseString %>% showHtmlPage()

  ## config parameter - max check
  targetValues <- targetValues[1:min(length(targetValues), 10)]
  targetInResponse <- sapply(targetValues, grepl, x = extract_meta$responseString, fixed = TRUE) %>%
    {sum(.) / length(.)} %>%
    magrittr::is_greater_than(0.4)
  if(!targetInResponse) stop("target values not in filtered server response.")


  XPathFromBrowser <- sivis$browserOutput$XPath
  # for additional on https://www.macysjobs.com/search-results? i need the request url, not the page url.
  # pageUrl <- sivis$browserOutput$pageUrl
  pageUrl <- sivis$cbData$request$request$url


  # config parameter - 0.1
  # adjusting target values for findable values in response body.
  paramFuzzy <- 0.1
  # todo: targetvalues with "&" a problem?
  # targetValue <- targetValues[5]

  # alle sonderzeichen: ? - / \
  # grep("as\asss", pattern = "\\")
  # changed to
  # "SAP Basis-Berater (m\/w\/divers)", "Berater (m\/w\/divers)
  # grep(extract_meta$responseString, pattern = "&amp;", fixed = TRUE)
  # grep(extract_meta$responseString, pattern = "", fixed = TRUE)
  # responseToText <- gsub(extract_meta$responseString, pattern = "&amp;", replacement = "&", fixed = TRUE)

  responseToText <- unescape_html2(extract_meta$responseString)
  # responseToText <- gsub(responseToText, pattern = "\/", replacement = "/", fixed = TRUE)

  if(!nchar(targetValues)) stop("targetValues are empty")

  newTargetValues <- sapply(targetValues, FUN = function(targetValue){
    aregexec(
      pattern = targetValue,
      # todo: check if that has a better alternative.
      text = responseToText,
      max.distance = nchar(targetValue)*paramFuzzy,
      fixed = TRUE
    ) %>%
      regmatches(x = responseToText)
  }, USE.NAMES = FALSE)

  # in case of encoding errors i get funny output like ????-??. No need for aregex to avoid funny matches like "löschen?"
  encodErrors <- grepl("^[?-]+$", targetValues)
  regNotFound <- !(newTargetValues %>% lengths)

  #NotMatchAfter <- newTargetValues[!excluded] %>% unlist %>% substr(start = 1, stop = 20)
  NotMatchBefore <- targetValues[!regNotFound & !encodErrors]
  NotMatchAfter <- newTargetValues[!regNotFound & !encodErrors] %>% unlist
  fuzzyMatches <- which(unlist(NotMatchAfter) != NotMatchBefore)

  if(length(fuzzyMatches)){

    fuzzyBefore <- NotMatchBefore[fuzzyMatches] %>% paste(collapse = '\", \"') %>% c('"', ., '"') %>% paste(collapse = "")
    fuzzyAfter <- NotMatchAfter[fuzzyMatches] %>% paste(collapse = '\", \"') %>% c('"', ., '"') %>% paste(collapse = "")
    warning(glue("Targetvalues:\n {fuzzyBefore} \nchanged to\n {fuzzyAfter}, \nbecause they where not found with a direct match. But only with a fuzzy match using a fuzzy parameter of {paramFuzzy}. \nConsider configuring that parameter if that change was inaccurate."))

  }

  newTargetValues  %<>% unlist
  if(length(newTargetValues)) targetValues <- newTargetValues  %>% unlist

  # split for text/html, because dont want to differentiate between encoding!?
  contentType <- getRes$headers$`content-type`
  # content type can be NULL? see https://www.accenture.com/de-de/careers/jobsearch?jk=&sb=1

  # could also extract encoding here?? utf-8?
  hasContentType <- !is.null(contentType)
  if(hasContentType){

    docType <- contentType %>%
      strsplit(split = ";") %>%
      unlist %>%
      .[1]

  }else{

    docType <- NULL

  }

  # check if json is within string.
  resourceType <- sivis$cbData$request$`_resourceType`
  if(is.null(resourceType)) resourceType  <- ""
  if(resourceType == "script" & docType == "application/json") docType <- "script/json"

  extract_meta$docType <- docType
  extract_meta$extractPathes = list()
  extract_meta$iterNr <- 0

  return(
    list(
      extract_meta = extract_meta,
      cbdata = cbdata,
      reqMethod = reqMethod,
      body = body,
      XPathFromBrowser = XPathFromBrowser,
      pageUrl = pageUrl,
      targetValues = targetValues,
      headers = headers,
      useHeader = useHeader
    )
  )
}


use_sivis <- function(sivis = sivis, testRun = FALSE, testEval = FALSE){

  const_meta <- prepare_Extract(sivis)

  if(is.null(const_meta)) return()

  extract_meta <- const_meta$extract_meta
  continue <- TRUE

  responseString = extract_meta$responseString
  docType = extract_meta$docType
  extractPathes = extract_meta$extractPathes
  iterNr = extract_meta$iterNr

  while(continue){

    cbdata = const_meta$cbdata
    reqMethod = const_meta$reqMethod
    body = const_meta$body
    XPathFromBrowser = const_meta$XPathFromBrowser
    pageUrl = const_meta$pageUrl
    targetValues = const_meta$targetValues
    headers = const_meta$headers
    useHeader = const_meta$useHeader

    extract_meta <- extracts_data(
      responseString = extract_meta$responseString,
      docType = extract_meta$docType,
      extractPathes = extract_meta$extractPathes,
      iterNr = extract_meta$iterNr,

      cbdata = cbdata,
      reqMethod = reqMethod,
      body = body,
      XPathFromBrowser = XPathFromBrowser,
      pageUrl = pageUrl,
      targetValues = targetValues,
      headers = headers,
      useHeader = useHeader,

      testRun = testRun,
      testEval = testEval
    )

    evalTestSuccess <- extract_meta[["testEval"]]

    if(!is.null(evalTestSuccess)){

      if(evalTestSuccess) return(TRUE)

    }

    continue <- !extract_meta[["allFound"]]
    continue
  }

  return(TRUE)

}


unescape_html2 <- function(str){
  html <- paste0("<x>", paste0(str, collapse = "#_|"), "</x>")
  parsed <- xml2::xml_text(xml2::read_html(html, options = "HUGE"))
  strsplit(parsed, "#_|", fixed = TRUE)[[1]]
}


extracts_data <- function(responseString, docType = NULL, cbdata, XPathFromBrowser = "", body = NULL, extractPathes = list(), pageUrl = pageUrl,
                          targetValues, iterNr = 0, testRun = FALSE, testEval = FALSE, reqMethod = "GET", headers = NULL, useHeader = FALSE,
                          reqSingleQuote = NULL){

  # this function can get called by itself if the targetvalues have to be extracted "across multiple levels". E.g. if within
  # response is a json with an object including html.
  # If it is called a second time, the document type (html) is not known before and has to be identified.

  iterNr = iterNr + 1
  if(iterNr > 6) stop("Too many iterations. Want to avoid getting caught in an infinite loop.")

  getAddTextJSONInfo <- identical(docType, "script/json")

  if(is.null(docType) | getAddTextJSONInfo){

    docTypeInfo <- findDocType(
      responseString = responseString,
      targetValues = targetValues
    )
    docType <- docTypeInfo$type

  }


  if(docType == "application/vnd.oracle.adf.resourcecollection+json") docType <- "application/json"
  if(docType == "application/xhtml+xml") docType <- "text/html"

  if(responseString %>% jsonlite::validate()){

    # text/plain -> is jsonobject for https://akamaijobs.referrals.selectminds.com/jobs/search/5145592/page2
    # decided for generic json check
    docType <- "application/json"

  }

  if(docType != "application/json" & grepl(pattern = "application", x = docType) & grepl(pattern = "json", x = docType)){

    warning(glue("DocType: {docType} seems to be of type 'application/json'. Attempting the corresponding extraction method.
                  Please file an issue to add this document type to the list with an indication whether the scrape was successful."))
    docType <- "application/json"

  }

  if(docType != "text/html" & grepl(pattern = "html", x = docType)){

    warning(glue("DocType: {docType} seems to be of type 'text/html'. Attempting the corresponding extraction method.
                  Please file an issue to add this document type to the list with an indication whether the scrape was successful."))
    docType <- "text/html"

  }

  if(magrittr::not(docType %in% c("text/html", "application/json", "script/json"))){

    stop(glue("For docType: '{docType}' there is no extraction method available yet. Please file an issue."))

  }

  docType
  if(docType == "script/json"){

    jsonExtractor <- JSON_from_String(
      str = responseString,
      regexStr = docTypeInfo$jsonRegex,
      reqSingleQuote = docTypeInfo$reqSingleQuote,
      indexNr = docTypeInfo$JSONIdx,
      targetValues = targetValues
    )

    if(!jsonlite::validate(jsonExtractor$jsons$jsons)) stop("Identified a json, but can not parse it with jsonlite.")

    responseString <- jsonExtractor$jsons$jsons
    extractPathes[[length(extractPathes) + 1]] <- list(docTypeInfo)
    names(extractPathes)[length(extractPathes)] <- "scriptJsonIndex"

    # have extracted a JSON now, can move on as if i would have gotten a JSON from the server.
    # The necessary extraction step is saved in variable above: extractPathes.
    docType <- "application/json"

  }

  docType
  if(docType == "application/json"){

    return(
      evaluate_JSON(iterNr, responseString, targetValues, extractPathes, reqSingleQuote, pageUrl, reqMethod, headers, useHeader, body, testRun, testEval, XPathFromBrowser)
    )

  }else if(docType == "text/html"){

    # nest in else if otherwise json with html is extracted and jumped right into html extraction without adjusting the inputs
    return(
      evaluate_HTML(iterNr, responseString, targetValues, extractPathes, XPathFromBrowser, maxCheck, reqMethod, body, testEval, useHeader, XPathes, pageUrl)
    )

  }

}


evaluate_HTML <- function(iterNr, responseString, targetValues, extractPathes, XPathFromBrowser, maxCheck, reqMethod, body, testEval, useHeader, XPathes, pageUrl){

  # config parameter
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

  # have to set xpath to environ/global variable, so that later on xpathes can be added
  XPathes <- htmlResult$extractPathes$xpath
  extractPathes[[length(extractPathes) + 1]] <- htmlResult$extractPathes
  names(extractPathes)[length(extractPathes)] <- "xpath"

  if(all(htmlResult$allFound)){

    if(testRun & !testEval){
      return(
        list(allFound = TRUE)
      )
    }

    reqMethod <- sivis$reqMethod
    testEval <- createDocument(
      pageUrl = pageUrl,
      reqMethod = reqMethod,
      responseString = responseString,
      extractPathes = extractPathes,
      body = body,
      XPathes = XPathes,
      testEval = testEval,
      useHeader = useHeader
    )

    return(
      list(
        testEval = testEval
      )
    )

  }else{

    return(
      list(
        responseString = htmlResult$resultValues,
        extractPathes = extractPathes,
        docType = NULL,
        XPathFromBrowser = "",
        iterNr = iterNr,
        targetValues = targetValues,
        testRun = testRun,
        allFound = FALSE
      )
    )
  }

}


evaluate_JSON <- function(iterNr, responseString, targetValues, extractPathes, reqSingleQuote, pageUrl, reqMethod, headers, useHeader, body, testRun, testEval, XPathFromBrowser){

  jsonStruct <- extract_JSON(
    responseString = responseString,
    targetValues = targetValues,
    extractPathes = extractPathes,
    reqSingleQuote = reqSingleQuote
  )
  extractPathes <- jsonStruct$extractPathes

  jsonStruct$allFound

  if(jsonStruct$allFound){

    if(testRun & !testEval){

      return(list(allFound = TRUE))

    }

    testEval <- createDocumentGET(
      pageUrl = pageUrl,
      extractPathes = extractPathes,
      testEval = testEval,
      targetKeys = NULL, # need this for call from shiny
      reqMethod = reqMethod,
      headers = headers,
      useHeader = useHeader,
      body = body
    )

    return(
      list(testEval = testEval, allFound = TRUE)
    )

  }else{

    return(
      list(
        responseString = jsonStruct$resultValues,
        extractPathes = jsonStruct$extractPathes,
        docType = NULL,
        XPathFromBrowser = XPathFromBrowser,
        iterNr = iterNr,
        targetValues = targetValues,
        testRun = testRun,
        allFound = FALSE
      )
    )

  }

}


createDocumentGETWrap <- function(targetKeys){

  pageUrl <- sivis$pageUrl
  extractPathes <- sivis$extractPathes
  testEval <- sivis$testEval
  reqMethod <- sivis$reqMethod
  headers <- sivis$headers
  useHeader <- sivis$useHeader
  body <- sivis$body

  createDocumentGET(
    pageUrl = pageUrl,
    extractPathes = extractPathes,
    testEval = testEval,
    targetKeys = targetKeys,
    reqMethod = reqMethod,
    headers = headers,
    useHeader = useHeader,
    body = body,
    searchMultiPage = FALSE
  )

}




extract_HTML <- function(responseString, targetValues, extractPathes, XPathFromBrowser = "", maxCheck = 5){

  XPathNr <- 1
  xpath <- ""
  text <- targetValues[1]
  allText <- targetValues[1:min(length(targetValues), maxCheck)] %>% gsub(pattern = "\n", replacement = "") %>% trimws
  exact <- FALSE
  url <- sivis$url
  sivis$doc = responseString %>% read_html
  doc <- sivis$doc
  #doc %>% showHtmlPage
  attr = NULL #"class"
  byIndex = TRUE

  # text = allText[5]
  XPathAllCols <- lapply(
    FUN = getXPath,
    X = allText,
    allText = allText,
    url = url,
    exact = exact,
    doc = doc,
    attr = attr, #"class"
    byIndex = byIndex
  )

  # need single accces "[" for:  "httpswwwfocusde.RData"
  XPathCandidates <- lapply(XPathAllCols, "[", "xpathes") %>% unlist %>% unique
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
    .[1] %>%
    paste0(., "[not(descendant::*)]")

  xpath

  # need single accces "[" for:  "httpswwwfocusde.RData"
  ColAltern <- lapply(XPathAllCols, "[", "ColsAltern") %>% unlist %>% unique


  resultValues <- doc %>% html_nodes(xpath = xpath) %>% html_text
  targetValues %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws
  resultValues %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws

  # config parameter
  approximate <- TRUE

  if(approximate){

    lengths <- targetValues %>% nchar
    similarityRatio <- adist(x = targetValues, y = resultValues) %>%
      apply(MARGIN = 1, FUN = min) %>%
      divide_by(lengths)
    allFound <- similarityRatio %>%
      magrittr::is_less_than(0.1) %>% all

  }else{

    allFound <- all(targetValues %in% resultValues)

  }

  if(!all(allFound)) warning("not all targetvalues found!")

  responseString = resultValues
  docType <- findDocType(
    responseString = responseString,
    targetValues = targetValues
  )

  if(docType == "unknown type"){

    allFound <- TRUE

  }

  # todo: What do i want to do if not all values are found
  list(
    allFound = allFound,
    extractPathes = list(xpath = xpath, ColAltern = ColAltern),
    resultValues = resultValues
  )
}

# For the scheduled scrape i want to extract by index not by targetvalue, since the target values wll change.
# For the initial scrape i want to extract by target value. The index value i can not know so far.
JSON_from_String <- function(str, regexStr = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), reqSingleQuote = FALSE, targetValues = NULL, indexNr = NULL){
  # httpsjobsapigooglemcloudioapijobsearchcallbackjobsCallbackpageSize10offset0companyNamecompanies2Fc3f8
  #### recursion limit reached in PCRE for element 1

  allJSONS <- gregexpr(
    pattern = regexStr,
    perl = TRUE,
    text = str
  ) %>%
    regmatches(x = str) %>%
    unlist

  if(!length(allJSONS)) stop("Expected a JSON, but did not find one.")

  hasTargetValues <- !is.null(targetValues)
  if(hasTargetValues){
    jsons <- allJSONS %>%
      data.frame(jsons = ., match = grepl(pattern = paste(targetValues, collapse = "|"), x = .), index = 1:length(.)) %>%
      dplyr::filter(match == TRUE)
  }else if(!is.null(indexNr)){
    jsons <- allJSONS %<>%
      data.frame(jsons = ., index = 1:length(.)) %>%
      dplyr::filter(index == indexNr) %$% jsons
  }else{
    stop("Please specify either the targetValues or the indexNr parameter.")
  }

  if(reqSingleQuote){
    jsons$jsons <- gsub(
      x = jsons$jsons,
      pattern = "'",
      replacement = '"'
    )
  }

  return(
    list(jsons = jsons)
  )

}


extract_JSON <- function(responseString, targetValues, extractPathes = list(), reqSingleQuote = NULL){

  jsonContent <- lapply(responseString,  FUN = jsonlite::fromJSON)

  # handle json arrays (wihout names)
  isJSONArray <- jsonContent %>% unlist %>% names %>% is.null
  isJSONArray

  if(isJSONArray){

    resultValues <- jsonContent[[1]]
    needAddExtraction <- FALSE

  }else{

    # config parameter  counter argument - need values for finding pattern?
    maxValues <- 10
    targetValues <- targetValues[1:min(length(targetValues), maxValues)]

    print("go")
    JSONValues <- allJSONValues(
      jsonContent = jsonContent,
      targetValues = targetValues
    )

    # example: https://cboe.wd1.myworkdayjobs.com/External_Career_CBOE
    # todo: very dirty: collecting more examples
    # another example would be having title5 as tail(match, 1)
    # --> therefore need slim variant (without the numbers).
    # test for both. tes5t==tes5t and test5==test (want both to succeed)

    lastVal <- tail(JSONValues$base, 1)
    lastValSlim <- gsub(
      x = lastVal,
      pattern = "[0-9]",
      replacement = ""
    )

    print(lastVal)
    if(!is.null(lastVal) & !is.null(JSONValues$targetKey)){
      if(lastVal == JSONValues$targetKey | lastValSlim == JSONValues$targetKey){
        JSONValues$base %<>% head(n = -1)
      }
    }
    print(JSONValues$base)

    sivis$neighbours <- JSONValues$neighbours

    isJSON <- jsonlite:::validate(JSONValues$texts)

    targetValues %<>% gsub(pattern = "\n|\r", replacement = "") %>% trimws
    resultValues <- JSONValues$texts %>% gsub(pattern = "\n|\r", replacement = "") %>% trimws

    needAddExtraction <- isJSON | JSONValues$isLargeHTML
    if(!needAddExtraction){
      distMatrix <- adist(x = targetValues, y = resultValues)
      distances <- apply(distMatrix, 1, min, na.rm = TRUE)

      # config parameter
      # todo how do i decide whether everything was found or not??????
      foundRatio <- (distances / nchar(targetValues) < 0.1) %>% {sum(.) / length(.)}

      # check if it is another json: example: httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData
    }

    if(!is.null(JSONValues$targetKey) & !is.null(JSONValues$base)){

      if(tail(JSONValues$base, 1) == JSONValues$targetKey) JSONValues$base %<>% head(-1)

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

    extractPathes[[length(extractPathes) + 1]] <- jsonToExtract
    names(extractPathes)[length(extractPathes)] <- "json"
  }

  if(needAddExtraction){
    return(
      list(
        allFound = FALSE,
        extractPathes = extractPathes,
        resultValues = JSONValues$texts
      )
    )
  }

  ####################### INDEXING ########################
  # Indexing can be needed in both JSON arrays and JSON objects
  ### limits of indexing: httpssjobsbrassringcomTgNewUISearchAjaxMatchedJobs.RData"
  ### cant even find right pattern manually there?

  # add potential indexing of resultValues

  # take all selected text not the limited amount (limitation is done due to performance issues) - here are no significant performance
  # issues to be expected
  targetSeq <- resultValues %in% sivis$cbData$clipBoardText$selectedText %>% which
  amtMatches <- colSums(data.frame(sapply(sivis$cbData$clipBoardText$selectedText, FUN = "==", resultValues)), na.rm = TRUE)

  if(all(amtMatches > 1)){
    foundRatio <- (targetValues %in% resultValues) %>% {sum(.) / length(.)}

    targetSeq <- targetSeq[seq(from = 1, to = length(targetSeq), by = amtMatches[1])] # assuming all amtMatches are the same - alternative go for table()

    # config parameter
    if(foundRatio > 0.4) allFound <- TRUE

    pattern <- targetSeq %>%
      diff %>%
      table %>%
      {.[which.max(.)] / (length(targetSeq) - 1)}

    if(length(pattern)){

      # config parameter
      if(pattern > 0.45){

        seqBy <- pattern %>%
          names %>%
          as.numeric

        extractPathes[[length(extractPathes) + 1]] <- list(
          start = targetSeq[which(diff(targetSeq) == seqBy)[1]],       #min(targetSeq),
          end = targetSeq[tail(which(diff(targetSeq) == seqBy), 1) + 1],         #max(targetSeq),
          by = seqBy,
          isString = " \n\tjsonlite::fromJSON() %>%",
          reqSingleQuote =  " \n\tgsub(pattern = \"'\", replacement = '\"') %>%"
        )

        names(extractPathes)[length(extractPathes)] <- "ArrayIndex"

      }else{

        stop("Finding a pattern in JSON array failed!")

      }

    }

    return(
      list(
        allFound = allFound,
        extractPathes = extractPathes,
        resultValues = resultValues[targetSeq]
      )
    )

  }else{

    pattern <- targetSeq %>%
      diff %>%
      table %>%
      {.[which.max(.)] / (length(targetSeq) - 1)}

    if(length(pattern)){
      if(pattern > 0.45){
        seqBy <- pattern %>% names %>% as.numeric

        if(seqBy > 1){
          extractPathes[[length(extractPathes) + 1]] <- list(
            start = targetSeq[which(diff(targetSeq) == seqBy)[1]],       #min(targetSeq),
            end = targetSeq[tail(which(diff(targetSeq) == seqBy), 1) + 1],         #max(targetSeq),
            by = seqBy,
            reqSingleQuote =   reqSingleQuote
          )
          names(extractPathes)[length(extractPathes)] <- "ArrayIndex"
        }
      }
    }
  }

  # config parameter
  if(foundRatio < 0.9){

    glue("Only found {foundRatio*100} per cent of target values, while extracting from json.") %>% warning
    allFound <- FALSE
    if(foundRatio > 0.4) allFound <- TRUE

  }else{

    allFound <- TRUE

  }

  hasNAs <- JSONValues$texts %>% is.na %>% any
  if(allFound & hasNAs) message("All target values found, but extraction of JSON values yields additional NAs")
  if(hasNAs) message("Extraction of JSON values yields NAs")

  if(length(resultValues) > length(targetValues)){

    message("Found more values than expected!")

  }

  return(
    list(
      allFound = allFound,
      extractPathes = extractPathes,
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

addXPathFromShiny <- function(XPathes, source = "shiny"){
  rstudioapi::documentSave(id = "Notebook_scraping.Rmd") # do i need this? does it work?

  existingXPathes <- getXPathFromScript()
  c(existingXPathes, XPathes) %>% unique

  createDocument(browserOutput = browserOutput, OneXPathOnly = FALSE)

}



# responseString <- "asdd <a>"
# responseString <- "asd {a:b, c:d} asdasd {a:b, c:e}"
# responseString <- '{"a":"text","b":"<html>asd</html>"}'
# responseString <- '"/*<!--*/ var phApp = phApp || {\"widgetApiEndpoint\":\"' is identified as html
# ---> if html and html, then html has to have a script tag

# complete list of meme types: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types

test_html <- function(responseString, targetValues){
  doc <- responseString %>%
    xml2::read_html()

  isList <- doc %>%
    typeof %>%
    magrittr::equals("list")
  isList

  # get false alarm for html if its text json instead, check if i find the tags
  # todo: do i really want to check if over sapply and df/matrix?
  isUsefulXPath <- sapply(
    X = targetValues,
    FUN = getXPathByText,
    doc = doc,
    onlyTags = TRUE
  ) %>%
    unlist() %>%
    setdiff(y = c("html", "body", "p")) %>%
    length %>%
    magrittr::is_greater_than(0)

  # tags <- doc %>%
  #   html_nodes(xpath = "//*") %>%
  #   html_name %>%
  #   unique %>%
  #   setdiff(y = c("html", "body"))
  #
  # # todo: make better
  # # cant check for html and body, in case it is a html from a json, see: "httpscareersunderarmourcomsearchjobsresultsActiveFacetID0CurrentPage2RecordsPerPage10Distance50Radius.RData"
  # # cant check for setdiff(tags, html/body),  am 01.02
  # foundTags <- tags %>%
  #   sapply(FUN = grep, x = responseString) %>%
  #   lengths %>%
  #   all

  #if(isList & foundTags & length(tags) == 1) stop ("HEEEEEEEEEREEE")
  htmlFound <- isList & isUsefulXPath
  return(htmlFound)
}

# a lot todo
findDocType <- function(responseString, targetValues){

  isJSON <- jsonlite:::validate(responseString)

  if(isJSON){
    return(list(type = "application/json"))
  }

  ####if(length(responseString) > 1) stop("responseString has to be of length one.") # cant use, because i also want to check the final resultvalues
  isHTML <- tryCatch(
    expr = test_html(responseString = responseString, targetValues = targetValues),
    error = function(e) return(FALSE)
  )

  # more complete solution if necessary: https://stackoverflow.com/questions/59695961/find-json-in-string-with-recursion-limit-in-r-windows.


  regexs <- c(
    jsonObject = "\\{(?:[^{}]+|(?R))*?\\}",
    jsonArray = "\\[.*?\\]"
  )
  jsonRegex <- regexs[1]

  ScriptJSON <- lapply(
    X = regexs,
    FUN = checkForJSON,
    targetValues = targetValues,
    responseString = responseString
  )

  #todo: refactor
  matches <- ScriptJSON %>% sapply(FUN = "[", "isMatch") %>% unlist

  if(sum(matches)){

    targetValMatchRatios <- ScriptJSON %>% sapply(FUN = "[", "matchRatio") %>% unlist
    matchWinner <- targetValMatchRatios %>% which.max

    # it can be the case, that both jsonArray and jsonObjects match and they contain the same
    # amount of matches as the structure is [{...}, {...}, {...},]. In that case
    # i would prefer having a single json array, instead of multiple jsonObjects.
    allEqual <- length(unique(targetValMatchRatios)) == 1
    if(allEqual){
      LenJsonObject <- ScriptJSON$jsonObject$jsons %>% unlist %>% nchar %>% sum
      LenJsonArray <-  ScriptJSON$jsonArray$jsons %>% unlist %>%  nchar %>% sum
      matchWinner <- c(LenJsonObject, LenJsonArray) %>% which.max
    }


    ScriptJSON <- ScriptJSON[[matchWinner]]

  }else{

    ScriptJSON <- list(isMatch = FALSE)

  }

  #### todo: refactor this shit
  if(isHTML){

    if(!ScriptJSON$isMatch) return(list(type = "text/html"))

    if(ScriptJSON$isMatch){

      hasScriptTag <- responseString %>%
        xml2::read_html() %>%
        html_nodes(xpath = "/script") %>%
        length

      if(hasScriptTag){

        return(list(type = "text/html"))

      }else{

        ## example: "httpswwwpepsicojobscommainjobspage1.RData"
        return(ScriptJSON)

      }

    }

  }else if(ScriptJSON$isMatch){

    return(ScriptJSON)

  }else{

    return(list(type = "unknown type"))

  }
}

# regexs <- c("\\{(?:[^{}]|(?R))*\\}", "\\[.*?\\]")
# jsonRegex <- regexs[1]
# jsonRegex <- regexs[2]

# more complete solution if necessary: https://stackoverflow.com/questions/59695961/find-json-in-string-with-recursion-limit-in-r-windows.

checkForJSON <- function(jsonRegex, responseString, targetValues, reqSingleQuote = FALSE){
  if(is.null(targetValues) | !length(targetValues)) stop("targetValues are empty")

  jsons <- gregexpr(
    pattern = jsonRegex,
    text = responseString,
    perl = T
  ) %>%
    regmatches(x = responseString) %>%
    unlist

  # now check for occurence of target values
  if(length(jsons)){
    # config parameter
    matches <- sapply(targetValues, grepl, fixed = TRUE, x = jsons) %>% as.matrix

    noMatch <- !length(matches)
    if(noMatch) stop("no targetvalues match found in responsestring.")

    # example for rowSums: "x_r_j_httpscareersactivisioncomsearchresults.RData"
    matchRatio <- matches %>% rowSums %>% {. / length(targetValues)}
    #if(matchRatio < 0.9) warning(paste0("only ", matchRatio, " per cent of targets found in json wrapped in html text."))

    # config parameter
    if(sum(matchRatio) < 0.5){
      JSONIdx <- 0
      isMatch <- FALSE
    }else{
      JSONIdx <- matchRatio %>% which.max
      isMatch <- TRUE
    }
  }else{
    isMatch <- FALSE
    JSONIdx <- 0
    matchRatio <- 0
  }

  if(isMatch){
    # todo: modify in case only one of jsons uses single quotes
    isJSON <- jsonlite::validate(jsons[JSONIdx])
    if(!isJSON & jsons %>% length){
      jsons <- jsons %>% gsub(pattern = "'", replacement = '"')
      isJSON <- jsonlite::validate(jsons[JSONIdx])
      reqSingleQuote <- isJSON
      if(!isJSON) ScriptJSON <- FALSE
    }
  }

  return(
    list(
      type = "script/json",
      isMatch = isMatch,
      matchRatio = max(matchRatio),
      jsons = jsons,
      jsonRegex = jsonRegex,
      JSONIdx = JSONIdx,
      reqSingleQuote = reqSingleQuote
    )
  )
}

# extractPath <- extractPathes[2:length(extractPathes)]
create_Addit_Extract <- function(extractPathes){
  if(!length(extractPathes)) return(NULL)
  nr <- 1
  for(nr in 1:length(extractPathes)){
    type <- extractPathes[nr] %>% names
    path <- extractPathes[type] %>% unlist %>% unname
    if(type == "xpath"){
      return(
        glue("\tresponse <- tryCatch(expr = response %>% read_html %>% html_nodes(xpath = '{path}') %>% html_text, error = function(e) NULL)")
      )
    }
  }
}


# url <- sivis$browserOutputRaw$url
# base <- deparse(dput(sivis$initGET$base), width.cutoff = 500L)
# baseFollow <- deparse(dput(sivis$initGET$baseFollow), width.cutoff = 500L)
# targetKeys <- deparse(sivis$targetKeys, width.cutoff = 500L)

# this covers: text2json, json extraction and early exit for huge html.
# does not cover: follow-up process of html or only html
baseGETTemplate <- function(pageUrl, base, baseFollow, targetKeys = NULL, extractPathes, reqMethod = "GET", body = NULL, headers = NULL, useHeader = FALSE){


  xpath <- paste0("\tresponse <- tryCatch(expr = response %>% read_html %>% html_nodes(xpath = '", extractPathes$xpath$xpath, "') %>% html_text, error = function(e) NULL)")

  # todo: what if they are multiple extractions. then index differently
  indexes <- extractPathes$scriptJsonIndex[[1]]$JSONIdx
  if(length(indexes) > 1){
    indexes <- extractPathes$scriptJsonIndex %>% paste(collapse = ", ") %>% c("c(", ., ")") %>% paste(collapse = "")
  }

  regex <- extractPathes$scriptJsonIndex[[1]]$jsonRegex %>%
    dput %>%
    safeDeparse()

  # todo: refactor, that no coercing to string
  handleQuotes <- ""
  if(identical(extractPathes$scriptJsonIndex$reqSingleQuote, "TRUE")){
    handleQuotes <- "response %<>% gsub(pattern = \"'\", '\"')"
  }

  scriptJsonIndex <- paste0(c(
    '\tresponse %<>% gregexpr(',
    paste0(c('\t\tpattern = ', regex,','), collapse = ""),
    '\t\tperl = TRUE',
    '\t) %>%',
    '\t\tregmatches(x = response) %>%',
    '\t\tunlist %>%',
    paste0(c('\t\t.[', indexes,']'), collapse = ""),
    handleQuotes
  ), collapse = "\n")


  bdyQuote <- ifelse(test = grepl(x = body, pattern = '"'), yes = "'", no = '"')
  scrBdy <- switch(is.null(body) + 1, paste0(',\n\tbody = ', bdyQuote, body, bdyQuote), "")
  scrHdr <- switch(is.null(headers) + 1, paste0(',\n\theaders = ', headers), "")

  # todo: what if they are multiple extractions. then index differently
  json <- function(targetKeys, base, baseFollow){
    paste0('\tresponse <- unpack_JSON(
    \tresponse = response,
    \ttargetKeys = ', targetKeys %>% unique %>% safeDeparse,',
    \tbase = ', base,',
    \tbaseFollow = ', baseFollow,'
    )$res')
  }

  #\treqMethod = ', reqMethod, scrHdr, scrBdy, ',

  if("ArrayIndex" %in% names(extractPathes)){
    from <- extractPathes$ArrayIndex$start
    to <- extractPathes$ArrayIndex$end
    by <- extractPathes$ArrayIndex$by
    singleQuote <- extractPathes$ArrayIndex$reqSingleQuote
    isString = extractPathes$ArrayIndex$isString

    ArrayIndex <- glue("\tseq <- seq(from = {from}, to = {to}, by = {by})")
    display <- paste0("tbl <- do.call(what = rbind, args = output) %>% \n\tunlist %>% \n\tunname %>%", singleQuote, isString," \n\t.[seq] %>% \n\tdata.frame(data = .)")
  }else{
    # dont unlist for sivis additional keys: "tr_j_httpscareersgooglecomapijobsjobsv1searchcompanyGooglecompanyYouTubehlenjloenUSlocationZC3BCrich2C20S.RData"
    # %>% unlist %>% unname %>% data.frame(data = .)
    display <- "tbl <- do.call(what = rbind, args = output) %>% .[complete.cases(.), ] %>% data.frame"
  }
  # indexes <- extractPathes$scriptJsonIndex[[1]]$JSONIdx
  # if(length(indexes) > 1){
  #   indexes <- extractPathes$scriptJsonIndex %>% paste(collapse = ", ") %>% c("c(", ., ")") %>% paste(collapse = "")
  # }

  hdr <- switch(is.null(headers) + 1, paste0(', add_headers(.headers = scraper$headers)'), "")
  bdy <- switch(is.null(body) + 1, paste0(', body = scraper$body'), "")

  # todo info in #so
  # https://www.nisource.com/careers/job-search benötigt double quotes im body
  # bdyQuote <- ifelse(test = grepl(x = body, pattern = '"'), yes = "'", no = '"')
  # scrBdy <- switch(is.null(body) + 1, paste0(',\n\tbody = ', bdyQuote, body, bdyQuote), "")
  # scrHdr <- switch(is.null(headers) + 1, paste0(',\n\theaders = ', headers), "")


  # create code from extractPathes by using coding templates.
  extractions <- extractPathes %>%
    names %>%
    mget(envir = environment(), inherits = TRUE)

  jsonsRaw <- extractions[names(extractions) == "json"]

  nr <- 1
  jsons <- c()
  if(length(jsonsRaw)){
    for(nr in 1:length(jsonsRaw)){
      vars <- extractPathes[names(extractPathes) == "json"]
      base <- vars[[nr]]$base %>% safeDeparse()
      baseFollow <- vars[[nr]]$baseFollow %>% safeDeparse()

      # if targetkeys are added by shiny, take them, else take them from extractpathes
      if(is.null(targetKeys)){
        targetKeys <- vars[[nr]]$targetKey
        sivis$targetKeys <- targetKeys
      }
      extractions["json"][nr] <- jsonsRaw[[1]](targetKeys = targetKeys, base = base, baseFollow = baseFollow)
    }
  }

  extractionAll <- extractions %>% unlist %>%
    paste(collapse = "\n")

  # require a function here, because it should be optional to replace pageUrl with a pageSize or itemSize dynamic parameter
  xhrHeader <- function(urlFunc){
    paste0(c(
    'library(DT)',
    'library(httr)',
    'library(glue)',
    '# ", 1 + (nr - 1)*maxItems,"',
    '# ", maxItems*nr,"',
    '',
    'scraper <- list(',
    # paste0('\turlHTTP = "', url, '",'),
    paste0('\turlGen = ', urlFunc, ','),

    #'\ t urlGen = function(nr){', paste0('\ t \ t paste0("', pageUrl, '")'), '\ t},',
    # paste0('\tbase = ', base,','),
    # paste0('\tbaseFollow = ', baseFollow,','),
    # paste0('\ttargetKeys = ', targetKeys,','),
    paste0('\treqMethod = "', reqMethod, '"', scrBdy, scrHdr, ""),
    ')',
    '',
    '',
    'output <- list()',
    'hasResult <- TRUE',
    'nr <- 1',
    ''
    ), collapse = "\n")
  }

  sivis$xhrHeader <- xhrHeader

  request <- paste0(
    c('while(hasResult){',
      '\tSys.sleep(0.2)',
      '\tprint(nr)',
      '\turl <- scraper$urlGen(nr)',
      '\t',
      paste0(c('\tgetRes <- ', reqMethod,'(url = url', bdy, hdr, ')'), collapse = ""),
      '\tif(getRes$status_code != 200) return(NULL)',
      '\tresponse <- content(getRes, type = "text")',
      extractionAll
    ),
    collapse = "\n"
  )

  list(
    headers = xhrHeader,
    request = request,
    display = display
  )
}
# getRes <- get(scraper$reqMethod)(url = url, body = scraper$body, add_headers(.headers = scraper$headers)) %>%

safeDeparse <- function(expr){
  ret <- paste(deparse(expr), collapse="")
  #rm whitespace
  gsub("[[:space:]][[:space:]]+", " ", ret)
}

createDocumentGET <- function(pageUrl = pageUrl, targetKeys = NULL, extractPathes = extractPathes, testEval = FALSE,
                              reqMethod = "GET", headers = NULL, useHeader = FALSE, body = NULL, searchMultiPage = TRUE){


  assign(x = "neighbours", value = sivis$neighbours, envir = .GlobalEnv)
  assign(x = "extractPathes", value = extractPathes, envir = .GlobalEnv)

  sivis$pageUrl = pageUrl
  sivis$extractPathes = extractPathes
  sivis$testEval = testEval
  sivis$reqMethod = reqMethod
  sivis$headers = headers
  sivis$useHeader = useHeader
  sivis$body = body
  sivis$targetKeys <- extractPathes$json$targetKey %>% safeDeparse


  # need this later for the .rmd file, to get additional fields from the get/post request
  print("createDocumentGET")
  # fileName <- sivis[["fileName"]]
  #if(is.null(fileName))
  fileName <- "Notebook_Scraping.Rmd"
  # url <- sivis$browserOutputRaw$url

  base <- extractPathes$json$base %>% dput %>% safeDeparse
  baseFollow <- extractPathes$json$baseFollow %>% dput %>% safeDeparse


  body <- sivis$headerBody
  if(is.null(useHeader)) useHeader <- FALSE
  headers <- switch(useHeader + 1, NULL, headers %>% dput %>% safeDeparse)
  #isLargeHTML <- extractPathes$json$isLargeHTML

  baseGet <- baseGETTemplate(
    pageUrl = pageUrl,
    base = base,
    baseFollow = baseFollow,
    targetKeys = targetKeys,
    extractPathes = extractPathes,
    reqMethod = reqMethod,
    body = body,
    headers = headers
  )

  # todo refactor
  sivis$xhrRequest <- paste0(
    c(
      baseGet$request,
      '\toutput[[nr]] <- response',
      '\tnr <- nr + 1',
      '',
      '\t#initially only one round',
      '\thasResult <- length(response) & nr < 2',
      '}'
    ), collapse = "\n"
  )

  # now i have all info available to test for potential page change / item size parameter
  # i need sivis$url, sivis$xhrRequest, sivis$xhrHeader

  if(searchMultiPage){
    urlFunc <- getUrlJSONPageChange(
      url = sivis$url,
      headerCode = sivis$xhrHeader(urlFunc = glue("function(nr){{'{pageUrl}'}}")),
      requestCode = sivis$xhrRequest
    )

    newUrlFunc <- urlFunc %>%
      deparse %>%
      trimws %>%
      paste(collapse = "")
    sivis$pageUrl <- newUrlFunc
  }else{
    newUrlFunc <- sivis$pageUrl
  }

  sivis$reproduceForPageChange <- paste(c(
    baseGet$headers(urlFunc = newUrlFunc),
    sivis$xhrRequest,
    baseGet$display
  ), collapse = "\n")


  if(testEval){
    success <- tryCatch(
      eval(parse(text = sivis$reproduceForPageChange), envir = .GlobalEnv),
      error = function(e) return(FALSE) ##do.call(return, list(e), envir = sys.frame(2 - sys.nframe())) # 2 - sys.nframe()
    )

    if(identical(success, FALSE)){
      return(FALSE)
    }else{
      return(TRUE)
    }

    # do.call(return, list(TRUE), envir = sys.frame(2 - sys.nframe())) # 2 - sys.nframe()
  }


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
                    sivis$reproduceForPageChange,
                    'datatable(tbl)',
                    '',
                    '```',
                    '',
                    'Put scraper to production:',
                    '',
                    '```{r}',
                    paste0(c('post_To_Production(host = "http://', host,'")'), collapse = ""),
                    '```',
                    '',
                    'Potential new fields to consider:',
                    '',
                    '```{r}',
                    'library(shiny)',
                    '',
                    'choiceNames <- paste(paste("<b>", names(neighbours), "</b>"), head(neighbours, 1), sep = ": ")',
                    'choiceNames <- lapply(choiceNames, HTML)',
                    '',
                    'ui <- fluidPage(',
                    '\tactionButton(\n\t\tinputId = "updateDocument", \n\t\tlabel = "Add selected keys to document:"\n\t),',
                    '\tbr(),',
                    '\tcheckboxGroupInput(\n\t\tinputId = "additionalKeys", \n\t\tlabel = "Additional keys to parse: ", choiceValues = \n\t\tas.list(names(neighbours)\n\t),',
                    paste0('\tselected = "', sivis$initGET$targetKey,'", choiceNames = choiceNames)'),
                    ')',
                    'server <- function(input, output, session){',
                    '\tobserveEvent(eventExpr = input$updateDocument,{',
                    '#\t\tfile.remove("Notebook_Scraping.Rmd")',
                    '\t\tsivis$targetKeys <-  c(sivis$targetKeys, input$additionalKeys)',
                    '\t\tcreateDocumentGETWrap(targetKeys = sivis$targetKeys)',
                    '\t\tstopApp(returnValue = invisible())',
                    '\t})',
                    '}',
                    '',
                    'runApp(\n\tappDir = shinyApp(ui, server), \n\tlaunch.browser = rstudioapi::viewer\n)',
                    '```'), collapse = "\n"),
    con = fileName)
  file.edit(fileName)
}

createDocument <- function(pageUrl, extractPathes, responseString, testEval = FALSE, reqMethod = "GET", Code_3_Extract = NULL,
                           useHeader = FALSE, body = NULL, XPathes = "", searchMultiPage = TRUE, searchMultiCols = TRUE, rCode = NULL){

  sivis$reqMethod <- reqMethod
  sivis$responseString <- responseString
  sivis$extractPathes <- extractPathes
  sivis$body <- body
  sivis$XPathes <- XPathes
  sivis$testEval <- testEval

  print("createDocument")
  # XPathes <- sivis$XPathes
  OneXPathOnly <- TRUE #length(XPathes) == 1
  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  if(is.null(useHeader)) useHeader <- FALSE
  headers <- base::switch(useHeader + 1, NULL, headers %>% dput %>% safeDeparse)
  hdr <- paste0('(add_headers(.headers = ', headers,'))')
  if(is.null(headers)) hdr <- ""

  hasJSON <- !is.null(extractPathes$json)

  if(hasJSON){

    libCall <- ""
    indent <- "\t"
    sivis$initGET <- extractPathes$json
    base <- safeDeparse(dput(extractPathes$json$base))
    baseFollow <- safeDeparse(dput(extractPathes$json$baseFollow))
    targetKeys <- safeDeparse(sivis$targetKeys)

    getTemplate <- baseGETTemplate(
      pageUrl = pageUrl,
      base = base,
      baseFollow = baseFollow,
      targetKeys = targetKeys,
      extractPathes = extractPathes,
      reqMethod = reqMethod,
      body = body,
      useHeader = useHeader,
      headers = headers
    )

    getFinishTemplate <-  paste0(c('\toutput[[nr]] <- response',
                                   '\tnr <- nr + 1',
                                   '',
                                   '\t######## INITIALLY ONLY ONE ROUND',
                                   '\tresponse <- c()',
                                   '\t######## INITIALLY ONLY ONE ROUND',
                                   '}'),
                                 collapse = "\n"
    )

    sivis$xhrRequest <- paste(c(getTemplate$request, getFinishTemplate), collapse = "\n")

    urlFunc  <- glue("function(nr){{'{sivis$url}'}}")
    urlFunc <- getUrlJSONPageChange(
      url = sivis$url,
      headerCode = sivis$xhrHeader(urlFunc = urlFunc),
      requestCode = sivis$xhrRequest
    )

    if(searchMultiPage){
      newUrlFunc <- urlFunc %>%
        deparse %>%
        trimws %>%
        paste(collapse = "")
    }else{
      newUrlFunc <- sivis$pageUrl
    }

    sivis$reproduceForPageChange <- paste(
      sivis$xhrHeader(urlFunc = newUrlFunc),
      sivis$xhrRequest,
      collapse = "\n"
    )
    Code_4_Display <- "do.call(rbind, output) %>% c %>% data.frame %>% DT::datatable()"

  }else{

    ####### go here for html
    ### initial request for single page and base code for multi page to give into getUrlJSONPageChange

    # config parameter should i include empty header to make it more easy to add some?
    Code_2_Request <- paste0(c(
      '',
      'response <- url %>% GET', hdr,' %>% content(type = "text")'
    ), collapse = "")

    if(is.null(Code_3_Extract)){
      Code_3_Extract <- paste0(c(
        'xpath <- data.frame(',
        paste(c('\t"', XPathes, '"'), collapse = ""),
        ')',
        'response <- tryCatch(\n\texpr = response %>% read_html %>% html_nodes(xpath = as.character(xpath)) %>% html_text(), \n\terror = function(e) NULL\n)'
      ), collapse = "\n")
    }

    Request_Extract <- paste(c(Code_2_Request, Code_3_Extract), collapse = "\n")

    if(searchMultiPage){

      urlFunc <- dynamicUrl(
        url = sivis$url,
        requestCode = Request_Extract
      )$func

      if(is.null(urlFunc)) urlFunc <- glue("function(nr) {sivis$url}") %>% toString

      # need this for updatedocument function
      sivis$isMultiPage <- !is.null(urlFunc)
      sivis$pageUrl <- urlFunc %>% safeDeparse()
    }else{
      urlFunc <- sivis$pageUrl
    }

    getFinishTemplate <- ""
    indent <- ""

    if(sivis$isMultiPage){

      Code_1_LibUrl <-paste0(c(
        'library(httr)',
        'library(DT)',
        paste0(c('urlGen <- ', sivis$pageUrl), collapse = ""),
        'nr <- 1',
        'hasResult <- TRUE',
        'output <- list()',
        '',
        'while(hasResult){',
        '\tprint(nr)',
        '\turl <- urlGen(nr)'
        ),
        collapse = "\n"
      )

      # todo: very dirty
      #getTemplate %<>% gsub(pattern = "\n", replacement = "\n\t")

      # config parameter: limit loop
      Code_4_Display <- paste0(c(
        '\thasResult <- length(response) & nr < 3',
        '\toutput[[nr]] <- response',
        'nr <- nr + 1',
        '}',
        '',
        'output %>% lapply(FUN = data.frame) %>% do.call(what = rbind) %>% DT::datatable()'
      ), collapse = "\n")

    }else{

      Code_1_LibUrl <-paste0(c(
        'library(httr)',
        'library(DT)',
        paste0('url <- "', urlFunc, '"')),
        collapse = "\n"
      )
      Code_4_Display <- 'response %>% data.frame %>% DT::datatable()'

    }

    # todo: do i still need this reproduce? already did it here?
    sivis$reproduceForPageChange <- Request_Extract

  }

  # sivis$cbData$request$request$url
  # url <- sivis$cbData$request$request$url
  # requestCode <- sivis$reproduceForPageChange
  # dynamicUrl(url, requestCode)


  if(testEval){
    success <- tryCatch(
      eval(parse(text = request_code)),
      error = function(e) return(FALSE)
    )
    return(TRUE)
  }


  # result of the function is sivis$moreCols
  # and addCols in global environment. So this can be placed in a seperate function
  if(searchMultiCols){

    addMultiCols(
      XPathes = XPathes,
      responseString = responseString,
      extractPathes = extractPathes,
      searchMultiCols = searchMultiCols,
      pageUrl = pageUrl
    )

  }else{

    addCols <- NULL

  }

  if(is.null(rCode)){
    rCode <- paste0(c(
      'options(stringsAsFactors = FALSE)',
      'library(xml2)',
      Code_1_LibUrl,
      Code_2_Request,
      Code_3_Extract,
      Code_4_Display
    ), collapse ="\n")
  }

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
                   rCode,
                   '```',

                   '',
                   '```{r}',
                   paste0(c('post_To_Production(host = "http://', host,'")'), collapse = ""),
                   '```',
                   '',

                   sivis$moreCols),


                 collapse = "\n"),
    con = fileName
  )
  file.edit(fileName)
}


addMultiCols <- function(XPathes, responseString, extractPathes, searchMultiCols, pageUrl){

  xp1 = XPathes
  sivis$XPathes <- XPathes
  # xp2 = extractPathes$xpath$ColAltern

  commonXPathRes <- Common_XPath_Data(
    responseString = responseString,
    xp1 = xp1,
    extractPathes = extractPathes
  )

  addCols <- commonXPathRes$addColsOutput %>%
    do.call(what = cbind) %>%
    apply(MARGIN = 2, FUN = gsub, pattern = "  |\t|\n|\r|View More|[|]", replacement = "") %>% # to better identify duplicates - could be made as config parameter
    .[, colSums(is.na(.) | !nchar(.) | . == " ") != nrow(.), drop = FALSE] %>% # remove NA cols
    .[, !duplicated(., MARGIN = 2), drop = FALSE] %>%  # remove duplicate cols
    {.[colSums(apply(., 1, is.na) %>% data.frame) != ncol(.), , drop = FALSE]}  # and remove complete NA rows
  #as.data.frame(col.names = colnames(.)) %>% # ensure two dimensions - not needed anymore due to drop = FALSE?

  if(length(addCols)){
    # make the order that columns which have only same values appear at last, because they might contain only title
    # of other columns or other irrelevant data
    onlyDupes <- apply(addCols, 2, FUN = function(col) col %>% table %>% {max(.) == length(col)}) %>% which
    order <- c(setdiff(1:ncol(addCols), onlyDupes), onlyDupes)
    addCols <- addCols[, order, drop = FALSE]

    # config parameter - remove empty character columns
    nonEmptyCol <- addCols %>%
      apply(MARGIN = 2, FUN = function(col) col %>% gsub(pattern = "\n|\t", replacement = "") %>% nchar %>% sum(na.rm = TRUE) %>% magrittr::is_greater_than(0))
    addCols <- addCols[, nonEmptyCol, drop = FALSE]

    if(!exists("nr")) nr <- 999
    save(addCols, file = paste0("addCols_", nr, ".RData")) # for batch testing of notebook_scraping.rmd files
    rootXpath <- commonXPathRes$rootXPath

    additionColExist <- ncol(addCols)

  }else{

    additionColExist <- FALSE

  }

  if(additionColExist){
    mat <- apply(addCols, 1, function(row) !is.na(row) & nchar(row)) %>% data.frame
    idx1 <- apply(mat, 2, sum) %>% order(decreasing = TRUE)

    missing <- mat[, idx1[1], drop = FALSE] %>% magrittr::not() %>% which
    idx2 <- apply(mat[missing, , drop = FALSE], 2, sum) %>% order(decreasing = TRUE)

    idx <- c(idx1[1], setdiff(idx2, idx1[1]))
    addCols <- addCols[idx, , drop = FALSE]
    assign("addCols", addCols, envir = .GlobalEnv) # need the variable accessible in scraping script, to make selections from shiny

    # config parameter
    selectedCol <- 1 # initially only the column is selected, that was selected in the browser.

    # mostly more columns will be selected than not selected by the user-
    # However, if all columns will be selected at the beginning the user, initially,
    # might not realise that columns cann be selected
    selectedCol <- 1:ncol(addCols)

    sivis$moreCols <- addColsOption(
      addCols = addCols,
      rootXpath = rootXpath,
      pageUrl = pageUrl,
      selectedCol = 0 # initially only the column is selected, that was selected in the browser.
    )
    sivis$rootXpath <- rootXpath
  }

  if(!additionColExist & searchMultiCols){
    sivis$moreCols <- paste0(
      c(
        "",
        "",
        "Did not detect any candidates for additional columns/variables."
      ), collapse = "\n"
    )
  }
}


addColsOption <- function(addCols, rootXpath, pageUrl, selectedCol){
  paste0(
    c(
      '',
      '',
      'Potential new fields to consider:',
      '',
      '```{r}',
      paste0(c("load('addCols_", nr, ".RData')"), collapse = ""),
      '',
      'library(shiny)',
      '',
      'ui <- fluidPage(',
      '\th5(shiny::tags$b("Click on columns to add/remove an xpath from the scraper.")),',
      '\tbr(),',
      '\tDT::dataTableOutput("tbl", height = "30em"),',
      '\tbr(),',
      '\th5(shiny::tags$i("Rows are ordered so that empty columns are attempted to be avoided on the first page.")),',
      '\tbr(),',
      '\tactionButton(',
      '\t\tinputId = "updateDocument",',
      '\t\tlabel = "Add selected columns to document:",',
      '\t\ticon("paper-plane"),',
      '\t\tstyle="color: #fff; background-color: #337ab7; border-color: #2e6da4"',
      '\t)',
      ')',

      'server <- function(input, output, session){',
      '',
      '\tobserveEvent(eventExpr = input$updateDocument,{',
      '\t\t#file.remove("Notebook_Scraping.Rmd")',
      paste0('\t\trootXPath <- ', rootXpath %>% safeDeparse,' '),
      paste0('\t\tpageUrl <- ', pageUrl %>% safeDeparse,' '),
      '\t\tXPathes <- colnames(addCols)[input$tbl_columns_selected + 1]',
      '\t\tupdateDocument(',
      '\t\t\tXPathes = XPathes,',
      '\t\t\trootXPath = rootXPath,',
      '\t\t\tpageUrl = pageUrl,',
      '\t\t\tselectedCol = input$tbl_columns_selected %>% sort %>% safeDeparse',
      ')',
      '\t\tstopApp(returnValue = invisible())',
      '\t})',
      '',
      '\toutput$tbl = DT::renderDataTable(',
      '\t\taddCols,',
      '\t\tserver = FALSE,',
      paste0('\t\tselection = list(mode = "multiple", target = "column", selected = ', selectedCol,'),'),
      '\t\toptions = list(pageLength = 10, autoWidth = TRUE)',
      '\t)',
      '}',
      '',
      'runApp(\n\tappDir = shinyApp(ui, server), \n\tlaunch.browser = rstudioapi::viewer\n)',
      '```'
    )
  )
}

updateDocument <- function(XPathes, rootXPath, pageUrl, selectedCol, doc = getActiveDocumentContext()){

  pageUrl <- sivis$pageUrl
  xp <- paste(XPathes, collapse = ",\n\t")

  amtXP <- length(XPathes)

  Code_1_LibUrl <- paste0(c(
    'options(stringsAsFactors = FALSE)',
    'library(rvest)',
    'library(DT)',
    '',
    paste0('url <- "', pageUrl, '"')
  ), collapse = "\n")

  Code_2_Request <- 'response <- read_html(x = url)'

  # need this part to test for additional pages where similar data could be found.
  Code_3_Extract <- paste0(
    c('xpathes <- ', paste(c(paste(c("data.frame(", glue("XPath{1:amtXP} = '{XPathes}'{c(rep(',', amtXP - 1), '')}\n\t")), collapse = "\n\t"), ")"), collapse = "\n"),
      'nodes <- response %>% read_html %>% html_nodes(xpath = ', rootXPath %>% safeDeparse,')',
      'response <- lapply(xpathes, function(xpath){',
      '\tlapply(nodes, function(node) html_nodes(x = node, xpath = xpath) %>% {ifelse(length(.), yes = html_text(.), no = NA)}) %>% unlist',
      '})'), collapse = "\n")

  sivis$reproduceForPageChange <- Code_3_Extract


  Code_4_Display <- paste0(c(
    'response %<>% do.call(what = cbind) %>% .[complete.cases(.), ]',
    'dt <- datatable(',
    '\tdata = response,',
    '\toptions = list(pageLength = 10)',
    ')',
    'dt'
  ), collapse = "\n")

  rCode <- paste0(
    c(Code_1_LibUrl,
      Code_2_Request,
      Code_3_Extract,
      Code_4_Display),
    collapse = "\n"
  )

  createDocument(
    pageUrl = sivis$pageUrl,
    reqMethod = sivis$reqMethod,
    responseString = sivis$responseString,
    extractPathes = sivis$extractPathes,
    body = sivis$body,
    XPathes = XPathes,
    testEval = sivis$testEval,
    useHeader = sivis$useHeader,
    searchMultiCols = FALSE,
    Code_3_Extract = Code_3_Extract,
    rCode = NULL
  )

}


# commonXPathes(responseString = responseString, xp1 = XPathes, xp2 = extractPathes$xpath$ColAltern[2])

commonXPathes <- function(doc, xp1, xp2){
  nodes1 <- list()
  nodes2 <- list()

  nodes1[[1]] <- html_node(x = doc, xpath = xp1)
  nodes2[[1]] <- html_node(x = doc, xpath = xp2)

  tagsSame <- identical(nodes1[[1]], nodes2[[1]])
  while(!tagsSame){

    nodes1 <- c(nodes1, tail(nodes1, 1)[[1]] %>% html_nodes(xpath = ".."))
    nodes2 <- c(nodes2, tail(nodes2, 1)[[1]] %>% html_nodes(xpath = ".."))
    mat <- matrix(NA, nrow = length(nodes1), ncol = length(nodes2))
    for(nr1 in 1:length(nodes1)){
      for(nr2 in 1:length(nodes2)){
        mat[nr1, nr2] <- identical(nodes1[nr1], nodes2[nr2])
      }
    }
    ######outer(nodes1[1:2], nodes2[1], FUN = identical) doesnt work
    tagsSame <- mat %>% sum
  }
  idx <- apply(mat, 1, which) %>% unlist %>% min
  allText <- html_nodes(x = doc, xpath = xp2) %>% html_text()
  commonXPath <- nodes1[[idx]] %>%
    getXPathByTag(doc = doc, allText = allText) %$%
    xpath
  commonXPath
}

Common_XPath_Data <- function(responseString, xp1, extractPathes){

  doc <- responseString %>% read_html
  tags <- doc %>% html_nodes(xpath = xp1)

  if(!length(tags)){

    stop(glue("Found an invalid XPath: {xp1} while attempting to find a commonXPath."))

  }


  leafPathes <- get_Leaf_Pathes(doc, tags)

  xpathes <- c(xp1, extractPathes$xpath$ColAltern)
  if(nchar(leafPathes$link_Path)){

    xpathes %<>% c(., leafPathes$link_Path)

  }


  # xpath <- xpathes[2]
  addXP <- sapply(xpathes, FUN = function(xpath){
    tags <- html_nodes(x = doc, xpath = xpath)
    allText <- tags %>% html_text
    xp <- getXPathByTag(
      tag = tags[1],
      doc = doc,
      rootPath = leafPathes$rootPath,
      allText = allText,
      byIndex = TRUE,
      getOtherCols = FALSE,
      justOneTag = TRUE
    )

    if(xp$xpath %>% substr(start = 1, stop = 5) != "/html"){
      xp$xpath %>% substring(first = 2)
    }

    return(xp$xpath)

  })

  leafPathes$subPathes %<>% c(addXP, .) %>%
    unlist %>%
    unique

  # XPath <- leafPathes$subPathes[1]
  addCols <- lapply(
    X = leafPathes$subPathes,
    FUN = findTextGivenRoot,
    rootPath = leafPathes$rootPath,
    doc = doc
  )

  addCols %<>% .[!is.null(.)]

  addCols %<>% .[!sapply(., is.null)]

  addColsOutput <- sapply(addCols, "[", "texts") %>%
    do.call(what = cbind) %>%
    data.frame

  names(addColsOutput) <- sapply(addCols, "[", "xpCand") %>%
    as.character

  list(
    addColsOutput = addColsOutput,
    rootXPath = leafPathes$rootPath
  )
}

findTextGivenRoot <- function(rootPath, XPath, doc){

  if(is.null(rootPath)) rootPath <- "/*"

  nodes <- html_nodes(x = doc, xpath = rootPath)
  texts <- lapply(
    X = nodes,
    FUN = html_nodes,
    xpath = XPath
  ) %>% lapply(
    FUN = function(node){
      ifelse(test = length(node), yes = html_text(node), no = NA)
    }
  )

  list(
    xpCand = XPath,
    texts = texts %>% unlist %>% gsub(pattern = "\n|\t", replacement = "")
  )
}


findTextGivenRoot_Depr <- function(rootPath, XPath, doc){
  allText <- html_nodes(x = doc, xpath = XPath) %>% html_text()
  if(!length(allText)) return()

  xpCand <- getXPathByTag(
    tag = html_nodes(x = doc, xpath = XPath)[1],
    rootPath = rootPath,
    allText = allText,
    doc = doc
  )$xpath

  if(substr(xpCand, 1, 1) == "/") xpCand %<>% substr(start = 2, stop = nchar(.))

  nodes <- html_nodes(x = doc, xpath = rootPath)
  texts <- lapply(
    X = nodes,
    FUN = html_nodes,
    xpath = xpCand
  ) %>% lapply(
    FUN = function(node){
      ifelse(test = length(node), yes = html_text(node), no = NA)
    }
  )

  list(
    xpCand = xpCand,
    texts = texts
  )
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

      warning("Href Attribute is not iterable")
      return()

    }else{

      urlGen <- IterableLinkGenerator(links = links)
      sivis$urlGen <- urlGen

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
# text = allText[1]
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

  # beispiel für nicht \n\t --> httpswwwrewedeangebotehamburg540724rewemarktwaldweg43.RData, weil ich nicht weiss wie in xpath replacen.
  # aber kann es im text nicht rausnehmen, weil dann andere beispiele failen, dann im dokument.
  doc %<>% gsub(pattern = "\n|\t", replacement = "") %>% read_html
  tags <- doc %>% html_nodes(xpath = xpath)
  if(length(tags) > 1) warning("Found more than one matching element!")

  if(!length(tags)){
    message("Did not find element as text, checking within attributes,...")

    xpathForAttrib <- paste0("//@*[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'),'", text,"')]")
    tags <- doc %>% html_nodes(xpath = xpathForAttrib)

    if(length(tags)){
      xpathOfAttrib <- getXPathByTag(tag = tags[1] %>% html_nodes(xpath = ".."), allText = targetValues)
      message(glue("targetvalues found in attribute: '{tags %>% html_name}' in element with xpath: {xpathOfAttrib}."))
    }else{
      message("targetValues also not found in attributes.")
    }

    glue("Did not find element for {text}.") %>% warning
    return(NULL)
    xpath = ""
  }

  # somehow cant use apply family here. last tag vanishes.

  xpathes <- rep(NA, length(tags))
  ColsAltern <- list()
  # nr <- 2
  # tag <- tags[nr]
  for(nr in 1:length(tags)){
    allXP <- getXPathByTag(
      tag = tags[nr],
      attr = attr,
      byIndex = byIndex,
      allText = allText,
      doc = doc
    )
    xpathes[nr] <- allXP$xpath
    # ColsAltern[[nr]] <- unlist(allXP$xpathOtherCols)
  }

  ColsAltern %<>% unlist %>% unique
  ###########get_Leaf_Pathes(doc, tag)
  list(
    xpathes = xpathes,
    ColsAltern = ColsAltern
  )

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


# text <- "Anlagenmechaniker"
# doc <- contInit %>% read_html
# getXPathByText(text, doc, exact = FALSE, attr = NULL, byIndex = FALSE)
getXPathByText <- function(text, doc, exact = FALSE, attr = NULL, byIndex = FALSE, onlyTags = FALSE){

  if(is.character(doc)){

    warning("doc is of type character - trying to convert to xml doc with xml2::read_html")
    doc %<>% xml2::read_html()

  }

  text %<>% tolower %>% gsub(pattern = " ", replacement = "")
  xpath <- paste0("//*[text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", text, "')]]")
  tag <- doc %>% html_nodes(xpath = xpath)

  tagName <- ""
  tags <- c()

  if(length(tag) > 1){
    tagNames <- sapply(tag, html_name)
    match <- which(tagNames != "script")
    if(!length(match)) match <- 1
    tag <- tag[match[1]]
  }else if(length(tag) == 0){
    message(glue("Did not find an xpath element that matches target Text: {text}!"))
    return(NULL)
  }

  while(tagName != "html"){
    tagName <- tag %>% html_name
    tags <- c(tags, tagName)
    tag <- tag %>% html_nodes(xpath = "..")
  }

  if(onlyTags){
    return(tags %>% unique)
  }

  xpath <- paste(c("", tags[length(tags):1]), collapse = "/")
  xpath

}

#tag <- tags[[1]]
byIndex = TRUE
rootPath = "/*"
allText = NULL
getOtherCols = TRUE
getXPathByTag <- function(tag, exact = FALSE, attr = NULL, byIndex = TRUE, allText = NULL, doc, rootPath = "/*", getOtherCols = TRUE, justOneTag = FALSE){

  if(!length(tag)) return(list(xpath = NULL))

  tag <- as.list(tag)
  if(html_name(tag) == "html") stop("iterTag is html root")

  if(is.null(rootPath)) rootPath <- "/*"

  rootTags <- doc %>% html_nodes(xpath = rootPath)
  tagName <- ""
  xpElems <- ""
  tagsOtherCols <- NULL
  iterTag <- tag # in case of debugging nike needed
  XPathFound <- iterTag %in% rootTags

  iterNr <- 1
  while(!XPathFound){

    tagName <- iterTag %>% html_name
    tagNameInsert <- tagName

    if(byIndex){

      childrenAll <- iterTag %>%
        html_nodes(xpath = "..") %>%
        html_nodes(xpath = "child::*")

      childrenMatchTag <- childrenAll[html_name(childrenAll) == tagName]
      childrenMatchText <- childrenMatchTag %>% html_text

      # In case of multiple target strings: Dont set index if its html_text includes
      # other targetStrings. In current implementation this includes also
      # indexes of all parent tags, since their html_text also include
      # the other targetStrings.

      # refactor better grepl?

      # config parameter
      percentageFound <- sapply(allText, grep, x = childrenMatchText, fixed = TRUE) %>%
        lengths %>%
        {sum(.) / length(.)}

      hasAllChildren <- percentageFound > 0.7

      # todo: where do i need - hasAllChildren <- TRUE??
      # counterexample: addcols for https://jobs.fcx.com/search/?searchby=location&createNewAlert=false&q=&locationsearch=&geolocation=
      if(is.null(allText)) hasAllChildren <- FALSE

      #  | notInAllTags
      # todo: do i really measure this correctly? Example: Blackrock
      # is a tradeoff - cant find all. but things might have changed since i removed
      # addcols calculation from here.
      tagMatch <- sapply(allText, FUN = grepl, x = childrenMatchText, fixed = TRUE)

      if(!length(tagMatch %>% unlist)){

        inJustOneTag <- FALSE

      }else{

        inJustOneTag <- tagMatch %>%
          as.data.frame %>%
          rowSums() %>%
          magrittr::equals(0) %>%
          {sum(.) == length(.) - 1}

      }

      needIndex <- !hasAllChildren | iterNr == 1 | all(unique(xpElems) %in% "") #| inJustOneTag

      if(justOneTag){

        needIndex <- !hasAllChildren | iterNr == 1 | all(unique(xpElems) %in% "") | inJustOneTag

      }

      needIndex
      matches <- c()
      hasTarget <- FALSE

      if(needIndex){

        matches <- rep(0, length(childrenMatchTag))
        # dont need to set index if there is only one element with same tag type.
        # for consistency with chrome output.

        #if(length(iterTag) > 1) iterTag <- list(iterTag) # need to wrap in list so that i can find it children of its parents, see below.
        if(length(matches) > 1){

          for(nr in 1:length(childrenMatchTag)){

            matches[nr] <- identical(childrenMatchTag[nr], iterTag)

          }

          hasTarget <- allText %in% html_text(childrenMatchTag) %>% sum

          # Example: "httpsemploymentwellsfargocompscPSEAAPPLICANT_NWHRMScHRS_HRAM_FLHRS_CG_SEARCH_FLGBLPageHRS_APP_SCHJOB_.RData"
          # "Product Manager 2 - ETP, CEF and UIT" in "Job TitleProduct Manager 2 - ETP, CEF and UIT",
          # because children are div: JobTitle and span: Product Manager,....
          # but i need the indexing on that div level, so i have to accept this text merge.

          if(is.null(allText)){

            hasTarget <- 1

          }else{

            hasTarget <- sapply(allText, grepl, x = html_text(childrenMatchTag), fixed = TRUE) %>% sum

          }

          tagNameInsert <- glue("{tagName}[{which(matches == 1)}]")
          # now finding candidates for alternative columns that are displayed in the shiny overview.
          if(hasTarget) tagNameAltern <- glue("{tagName}[{which(matches != 1)}]")

        }

      }

      iterNr <- iterNr + 1
      # config parameter
      if(iterNr > 70) stop("Too many iterations (70+) in getxpathbytag()")

    }

    # todo: refactor
    if(!length(iterTag)){

      # need this if iterTag is empty: xml_nodeset of 0.
      XPathFound <- FALSE

    }else{

      # need this if it has two elements. need to better understand this.
      XPathFound <- iterTag %in% rootTags %>% sum | html_name(iterTag) == "html"

    }

    if(getOtherCols){

      if(length(matches) > 1 & hasTarget){

        tagsOtherCols <- tagNameAltern #lapply(tagNameAltern, function(tag) c(tags, tag))

      }else{

        if(!is.null(tagsOtherCols)){

          tagsOtherCols <- lapply(tagsOtherCols, function(tag) c(tag, tagNameInsert))

        }

      }

    }

    xpElems <- c(xpElems, tagNameInsert)
    iterTag <- iterTag %>% html_nodes(xpath = "..")
    # print(iterTag %>% html_text)

  }

  #todo: sometimes a tag "text" comes up, that i dont want. However, if i debug nike jobsite
  # tag script does not come up, but i want it.
  xpElems <- xpElems[magrittr::not(xpElems %in% c("text", ""))]
  if(rootPath != "/*") xpElems <- xpElems[-length(xpElems)] # & length(xpElems) > 1
  xpath <- paste(c("", xpElems[length(xpElems):1]), collapse = "/")

  return(
    list(
      xpath = xpath
    )
  )
}

# # tags <- tagsOtherCols[[2]]
# xpathOtherCols <- NULL
# if(getOtherCols){
#
#   # tags <- tagsOtherCols[[4]]
#   xpathOtherCols <- lapply(tagsOtherCols, FUN = function(tags){
#     tags <- tags[magrittr::not(tags %in% c("text", ""))]
#     xpCand <- paste(c("", tags[length(tags):1]), collapse = "/")
#
#     hasTextChild <- TRUE
#
#     while(hasTextChild){
#
#       children <- lapply(xpCand, FUN = function(xpath){
#         nodes <- html_node(x = doc, xpath = xpath) %>%
#           html_nodes(xpath = "child::*") %>% as.list
#       })
#       children
#       if(!length(unlist(children))) break
#
#       addTagName <- lapply(children, function(child){
#         if(!length(child)) return("")
#         lapply(child, function(ch){
#           return(ifelse(length(ch), yes = ch %>% html_name %>% paste0("/", ., collapse = ""), no = ""))
#         })
#       })
#
#       # library(rlist)
#       # addTagName <- rlist::list.flatten(addTagName)
#
#       addTagName <- lapply(addTagName, function(tagName){
#         tbl <- table(unlist(tagName))
#         name <- names(tbl)[1]
#         for(name in names(tbl)){
#           idx <- which(name == tagName)
#           if(nchar(name) & length(idx) > 1) tagName[idx] <- glue("{name}[{1:length(idx)}]")
#         }
#         tagName
#       })
#
#       nr <- 1
#       out <- list()
#       for(nr in 1:length(xpCand)){
#         out[[nr]] <- paste0(xpCand[nr], addTagName[[nr]] %>% unlist)
#       }
#       xpCand <- out %>% unlist
#       hasTextChild <- lapply(children, function(child) child %>% html_text %>% nchar) %>% unlist %>% sum
#
#     }
#
#     # todo very dirty - remove first element, because it is already in commonXPath, but i need it as a start point
#     if(rootPath != "/*"){
#       xpCand %<>% strsplit(split = "/") %>% unlist %>% .[-2] %>% paste(collapse = "/")
#     }
#     xpCand
#
#   })
# }
#


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


get_Leaf_Pathes <- function(doc, tags){

  len <- length(tags)
  lenTags <- len
  out <- list()
  nr <- 1
  has_valid_Parent <- lenTags == len & sum(tags %>% html_name != "html")
  link_Path <- ""

  while(has_valid_Parent){

    out[[nr]] <- tags

    hasLink <- tags %>%
      html_name %>%
      magrittr::equals("a") %>%
      sum

    if(hasLink){

      href <- tags %>% html_attr(name = "href")

      if(href %>% nchar %>% sum){

        link_Path <- getXPathByTag(tag = tags[1], doc = doc)$xpath

      }

    }

    tags <- tags %>% html_nodes(xpath = "..")
    lenTags <- length(tags)
    nr <- nr + 1

    has_valid_Parent <- (tags %>% html_name != "html") # let iter till html otherwise cant catch parent of link a with lenTags == len &
    # should actually not happen, because before we should hit the html tag
    if(nr > 70) stop("Too many iterations in get_Leaf_Pathes(). Stopped after iteration 70.")

  }

  ### speculative change could it be that i need nr-1 or nr-2: LINK is example for nr-1
  #### lets say i have lengths of 2,3, 72, 72, 72. I would want to have nr=3. He would start at 72 ->
  # better way -> just compare length of out

  idx <- (lengths(out) == len) %>%
    which %>%
    max %>%
    max(1) # to avoid integer(0) mismatch --> add max(1)

  # todo: how to avoid these double ifs. Arent there programming languages that
  # tried to use for allow 50, 50 , 1 catch one as it does not have other children and
  # catch link. Does not work.
  # if(idx < length(out)){
  #
  #   if(lengths(out)[idx + 1] == 1) idx <- idx + 1
  #


  start <- out[[idx]]
  # start <- out[[max(1, nr - 1)]] # use max, to avoid getting negative value for "nr".
  allText <- start %>% html_text

  start_Tag <- start[1]

  #### ASSUMPTION: since i need the xpath just as the upper bound root path, i can try to
  ### leave out the indexing
  rootPath <- getXPathByTag(
    tag = start_Tag,
    doc = doc,
    allText = allText,
    byIndex = FALSE,
    getOtherCols = FALSE
  )$xpath

  # weird workaround? https://stackoverflow.com/questions/61064681/how-do-i-find-all-non-parent-nodes-in-xpath-without-converting-to-string
  #
  # leaves <- start %>% toString %>% read_html %>% html_nodes(xpath = "*//*[not(descendant::*)]")
  leaves <- start %>% html_nodes(xpath = ".//*[not(descendant::*)]")

  # if no results are found, but there is a parent which does not have other siblings,
  # parent can be checked without risking to get false positives.
  parent_node <- start %>% html_nodes(xpath = "..")
  empty_but_valid_parent <- !length(leaves) & length(parent_node) == 1

  if(empty_but_valid_parent){

    leaves <- parent_node %>% html_nodes(xpath = ".//*[not(descendant::*)]")

  }

  parent_Leaves <- search_Parent_Nodes(leaves, rootPath, doc)

  leafPathes <- list()
  if(!length(leaves)){

    return(
      list(
        subPathes = NULL,
        rootPath = NULL
      )
    )

  }

  for(nr in 1:length(leaves)){

    leafPathes[[nr]] <- getXPathByTag(
      tag = leaves[nr],
      rootPath = rootPath,
      doc = doc,
      getOtherCols = FALSE
    )$xpath

  }


  # group them by similarity
  ########## new experimental -  to start a node higher and catch link for https://careers.moodys.com/jobs?jobpage=2
  ### -> difficult due to sthg like:
  #### [19]  id="jobTitle_2578"
  #### [20]  href="index.cfm?fuseaction=app.jobinfo&amp;jobid=2578&amp;source=ONLINE&amp;JobOwner=993583&amp;com

  # tag_name <- leaves %>% html_name
  # tag_name_attr <- leaves %>%
  #   html_nodes(xpath = "@*") %>%
  #   paste(tag_name)
  #
  # leaves_by_Group <- sapply(tag_name_attr, FUN = "==", tag_name_attr) %>%
  #   apply(MARGIN = 1, FUN = which) %>%
  #   { .[, !duplicated(t(.))] } %>%
  #   apply(X = ., MARGIN = 2, FUN = function(idx) leaves[idx])
  #
  #
  # group_Nr <- 1
  # leaf_Nr <- 1
  # leafPathes <- list()
  # for(group_Nr in 1:length(leaves_by_Group)){
  #
  #    group <- leaves_by_Group[[group_Nr]]
  #    group_List <- list()
  #    for(leaf_Nr in 1:length(group)){
  #
  #      group_List[[leaf_Nr]] <- getXPathByTag(
  #       tag = group[leaf_Nr],
  #       allText = group %>% html_text,
  #       rootPath = rootPath,
  #       doc = doc,
  #       getOtherCols = FALSE
  #     )$xpath
  #
  #    }
  #
  #    leafPathes[[group_Nr]] <- group_List
  #
  # }

  subPathes <- leafPathes %>%
    unlist %>%
    table %>%
    names %>%
    substring(first = 2) %>%
    c(., parent_Leaves)


  if(is.null(rootPath)) rootPath <- "/*"

  list(
    subPathes = subPathes,
    rootPath = rootPath,
    link_Path = link_Path
  )

}


mgsub2 <- function(string, pattern, replacement, recycle = FALSE, ...){

  if(!sum(nchar(pattern))){

    return(string)

  }

  pattern %<>% .[. != ""]

  if(length(replacement) == 1){

    replacement %<>% rep(length(pattern))

  }

  mgsub::mgsub(string, pattern, replacement, recycle, ...)

}

# todo: leaves_Parent_Text in addition to grant_parent
search_Parent_Nodes <- function(leaves, rootPath, doc){

  leaves_Parent <- leaves %>% html_nodes(xpath = "..")
  leaves_GrantParent <- leaves_Parent %>% html_nodes(xpath = "..")

  leaves_Text <- leaves %>% html_text
  leaves_Parent_Text <- leaves_Parent %>% html_text

  # require performance optimization here - still 13 seconds
  # mgsub is more performance than gsub(pattern = targets %>% paste(collapse = "|"))
  covered_text <- leaves_Text %>%
    unique

  # todo: could place this when i actually start the (optional) shiny app - might not always be used.
  message("Start finding additional columns,... this could be time consuming.")
  leaves_GrantParent_Text <- leaves_GrantParent %>%
    html_text %>%
    mgsub2(pattern = covered_text, replacement = "", fixed = TRUE) %>%
    mgsub2(pattern = c(" ", "\n", "\t", "\r", "|"), replacement = "", fixed = TRUE)

  #duples <- duplicated(leaves_GrantParent_Text, fromLast = TRUE) | duplicated(leaves_GrantParent_Text, fromLast = FALSE)
  # want to remove redundant rows here. in code above

  txt <- leaves_GrantParent_Text[1]
  unique_txt <- sapply(
    X = leaves_GrantParent_Text, # replaced leaves_GrantParent_Text in line below otherwise i just search for duplicates?
    FUN = function(txt){
      other_txts <- leaves_GrantParent_Text[leaves_GrantParent_Text != txt]
      mgsub2(string = txt, pattern = other_txts %>% toString, replacement = "", fixed = TRUE)
    }
  )

  new_data <- unique_txt %>%
    gsub(pattern = " ", replacement = "") %>% # prevent that str = " " stays
    nchar %>%
    magrittr::is_greater_than(0) %>%
    which %>% leaves_GrantParent[.]

  len <- new_data %>% length
  res <- rep(NA, len)

  if(length(res)){

    for(nr in 1:len){

      res[nr] <- getXPathByTag(
        tag = new_data[nr],
        doc = doc,
        allText = new_data %>% html_text,
        rootPath = rootPath
      )

    }

  }


  return(

    # todo: get rid of first / as it is a subnode - could include that in getxpathbytag
    res %>%
      unique %>%
      unlist %>%
      substring(first = 2)

  )

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

# for  "httpspfizerwd1myworkdayjobscomPfizerCareersclientRequestIDe761010b8e564743ad14e7dca67f09e2.RData"
#lstRaw[[1]]$body$children$children[[1]]$listItems[[1]]$title$instances[[1]]$text
subsetByStr <- function(lstRaw, arr, targetValues){
  nrr <- 1
  nr <- 1
  lst <- lstRaw
  base <- NULL
  baseFollow = NULL
  # nr <- 2
  # nr <- 3
  # nr <- 4
  # nr <- 5
  if(length(arr$str)){
    for(nr in 1:length(unlist(arr$str))){
      if(is.null(lst)) stop("extraction in json is wrong have a null list.")

      # lst could be split in multiple elements. Therefore just for targetvalue check wrap lst in list(lst)
      percentFound <- sapply(targetValues, grepl, x = list(lst), fixed = TRUE) %>% {sum(.) / length(.)}

      # config parameter
      IsSingleTarget <- percentFound < 0.1
      #hasName <- !is.null(names(lst))
      if(IsSingleTarget){
        subsetBy <- 1 #arr$iter
        if(nrr > 1) base <- arr$str[1:(nrr - 1)]
        baseFollow <- arr$str[nrr:length(arr$str)]
        break
      }else{
        subsetBy <- arr$str[nrr]
        nrr <- nrr + 1
      }
      noName <- is.null(names(lst))
      if(noName) lst <- lst[[1]] # or only for nr == 1 and better filter for singletarget? ?? counterexample "httpspfizerwd1myworkdayjobscomPfizerCareersclientRequestIDe761010b8e564743ad14e7dca67f09e2.RData"
      lst <- lst[[subsetBy]]
      if(!is.null(baseFollow) & nrr > 1) lst <- lst[[baseFollow]]
    }
  }

  if(is.null(base) & nrr > 1) base <- arr$str
  list(
    base = base,
    baseFollow = baseFollow,
    lst = lst
  )
}

# [1] "body"      "children"  "children"  "listItems" "title"     "instances"

subsetByStr2 <- function(lstRaw, arr){

  lst <- lstRaw
  # nr <- 1

  for(nr in 1:length(arr)){

    if(is.null(names(lst))) lst <- lst[[1]]
    lst <-  lst[[arr[nr]]]
    nr <- nr + 1

  }

  lst

}

# arr = targetKey

# https://flir.wd1.myworkdayjobs.com/flircareers/jobs
#https://mosaic.wd5.myworkdayjobs.com/mosaic
#[[49]]
# id  widget                                text action    v
# 1 20846215 moniker Production Supervisor - Granulation    GET TRUE
#
# [[50]]
# id  widget                              text action    v
# 1 20846224 moniker Production Supervisor - Phos Acid    GET TRUE

## --> try: sapply(lstRaw, "[", arr, USE.NAMES = FALSE) %>% unname %>% unlist


##### https://careers.invesco.com/ListJobs
##key: JobTitleRegex

# JobTitle      ShortTextField1
# 1 Specialist, RTR Finance & Accounting

#####lstRaw$JobTitle


# JobTitle                 Location
# 1       Apartment Maintenance Technician


subsetByStr3 <- function(lstRaw, arr){

  if(typeof(lstRaw) == "list" & !is.data.frame(lstRaw)){

    # refactor: really dirty
    if(is.list(lstRaw[[1]]) & !is.data.frame(lstRaw[[1]])){

      if(is.null(lstRaw[[1]][[arr]]) & !is.null(lstRaw[[1]][[1]][[arr]]) & length(lstRaw) == 1){

        lstRaw <- lstRaw[[1]]

      }

    }

    # as.data.frame for     fl <- "httpshiremyaviontecomsonarv2jobBoardQ16L3ooLDFQW1VE2wFUOsQ.RData"
    lstRaw <- do.call(rbind, lstRaw) %>% data.frame # for workadays: https://flir.wd1.myworkdayjobs.com/flircareers/jobs
  }

  # there can be multple matches like for https://aimcojobs.com/ListJobs?. But why do i do this anyway? why dont take regex
  # directly, because i cant subset that way,  but can i later?? or subset with regex?
  arrs <- sapply(arr, grep, x = names(lstRaw)) %>% unlist %>% names(lstRaw)[.]

  # transformation fails: https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25679&siteid=5313#keyWordSearch=&locationSearch=
  if(!length(arrs)){

    warning("transformation of target keys failed.")
    arr %<>% adist(names(lstRaw)) %>% which.min %>% names(lstRaw)[.]

  }else{

    arr <- arrs %>% adist(arr) %>% which.min %>% arrs[.] #unique

  }

  if(is.null(arr)) stop("Do not find correct targetKeys for targetValues in json.")
  lst <- lstRaw
  nr <- 1
  #if(typeof(lst) == "list") lst <- do.call(rbind, lst)

  lst[[arr]] %>% as.character # prevent dropping nulls and getting results of different length that are harder to bind.

  # cant there be multiple arr?? actualy no right? why this loop then?
  # for(nr in 1:length(arr)){
  #   lst <-  lst[[arr[nr]]]
  #   if(!length(lst)) return("") # missing element return empty string
  #   #if(is.null(names(lst))) lst <- lst[[1]] # todo: what do i need this for: counterexample: "httpscareerbelufthansacomglobaljobboard_apisearchdata7B22LanguageCode223A22DE222C22SearchParameters2 (2).RData"
  # }
  # lst
}


# targetValue <- targetValues[1]
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

    winnerIndex <- grep(
      pattern = targetValue,
      x = candidates,
      fixed = TRUE
    )

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

#https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25679&siteid=5313#keyWordSearch=&locationSearch=
# jsonContent[[1]]$Jobs$Job$Questions[[1]]$Value[12]

# base --> Jobs Job Questions
# INDEX
# targetKey value
# index = 12

allJSONValues <- function(jsonContent, targetValues){

  jsonContentFlat <- unlist(jsonContent)

  # since i allow for json array now, this test is not needed anymore !?
  # hasNames <- jsonContentFlat %>%
  #   names %>%
  #   is.null
  # if(hasNames){
  #   stop("content can not be parsed - transformation to data.frame failed. Require json that can be parsed to data.frame as input")
  # }

  lst2 <- rlist::list.flatten(jsonContent)
  lastKeys <- unlist(
    unname(
      Map(
        function(x, y) sub('.*\\.', '', rep(x = x, each = y)),
        names(lst2),
        sapply(lst2, length)
      )
    )
  )

  # lastKeys <- names(jsonContentFlat) %>%
  #   strsplit(split = "[.]") %>%
  #   sapply(FUN = tail, n = 1)

  targetValues <- targetValues %>%
    gsub(pattern = "\n|\t", replacement = "") %>%
    trimws

  candidates <- jsonContent %>%
    unlist() %>%
    trimws

  matchIdx <- sapply(targetValues, matchStrings, candidates = candidates) %>%
    unlist %>%
    unname

  if(!length(matchIdx)){
    stop(
      paste0("No match for targetValues:'", paste(targetValues, collapse = " | "),"' in json.")
    )
  }

  targetKeys <- lastKeys[matchIdx %>% as.numeric()]
  targetKeyCount <- targetKeys %>% table

  # example for multipleMax on httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData
  multipleMax <- (targetKeyCount == max(targetKeyCount)) %>% sum %>% magrittr::is_greater_than(1)
  if(multipleMax){

    # or go directly on regex targetkey? but how to handle double targetkeyslim below again?
    targetKey <- gsub(x = targetKeys, pattern = "[0-9]", replacement = "") %>%
      table %>%
      which.max %>%
      targetKeys[.]

  }else{

    targetKey <- targetKeyCount %>%
      which.max %>%
      names

  }

  texts <- jsonContentFlat[lastKeys %in% targetKey] %>% unname

  # eliminating NAs, example: "httpswwwcotycomviewsajax.RData"
  texts %<>% .[!is.na(.)]

  # todo: how do i know if i have an html below or close match
  # check for html is not possible, because plain text is also html.
  # Possible options would include:
  # - check for text length
  # - check if there are plently <>.
  ### --> currently decided for <> count

  ##### Alternative
  # docLength <- nchar(texts) %>% sum
  # textsLength <- nchar(targetValues) %>% sum


  # todo? what if there are multiple texts, does this check still work?
  isLargeHTML <- texts %>%
    findDocType(targetValues = targetValues) %$%
    type %>%
    magrittr::equals("text/html")


    if(isLargeHTML){

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

    targetKeysSlim <- gsub(x = targetKeys, pattern = "[0-9]*", replacement = "") %>% table %>% which.max %>% names
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
  # idx <- which(targetKey == lastKeys)
  # lastKeys[idx]
  idx <- grepl(pattern = targetKey, x = lastKeys) %>% which
  lastKeys[idx]
  texts <- jsonContentFlat[idx] %>% unname

  # have to distinguish here between base and targetkey. often it works to just take the first element.
  # but "httpsapismartrecruiterscomjobapipublicsearchwidgetsExpeditorspostingsoffset0limit700fqcallbackjQuery1.RData" is
  # a counter example, which has two base values.
  # instead i have to identify to check if the second value is a targetkey. That will be the case if it repeats.

  # parentElemNmsRaw <- jsonContentFlat %>%
  #   names %>%
  #   .[matchIdx[1]] %>%
  #   strsplit(split = "[.]") %>%
  #   unlist

  # todo: stupid switching between tibble/dplyr and characters
  baseCandidates <- jsonContentFlat %>%
    names %>%
    .[matchIdx] %>%
    strsplit(split = "[.]")

  multBaseCand <- all(lengths(baseCandidates) > 1)

  if(multBaseCand){

    ## open issue: todo: targetkey value index 12, merges to value12. And here i am just lucky,
    ## that there is one value8 in there. so too issues --value index12 merges too value12 and
    ## then value12 matches also length(unique(.)) == 12
    ## httpssjobsbrassringcomTgNewUISearchAjaxMatchedJobs.RData"
    match <- baseCandidates %>%
      do.call(what = rbind) %>%
      data.frame %>%
      select_if(~length(unique(.)) == 1) %>%
      unique %>%
      as.character

  }else{

    if(baseCandidates %>% unlist %>% unique %>% length %>% magrittr::is_greater_than(1)){

      match <- NULL

    }else{

      match <- baseCandidates %>%
        unlist %>%
        .[1]

      # example: https://cboe.wd1.myworkdayjobs.com/External_Career_CBOE
      # todo: very dirty: collecting more examples
      # another example would be having title5 as tail(match, 1)
      # --> therefore need slim variant (without the numbers).
      # test for both. tes5t==tes5t and test5==test (want both to succeed)
      lastVal <- tail(match, 1)
      lastValSlim <- gsub(
        x = lastVal,
        pattern = "[0-9]",
        replacement = ""
      )

      if(tail(match, 1) == targetKey | lastValSlim == targetKey) match %<>% head(n = -1)

    }

  }


  # match <- parentElemNmsRaw %>% {ifelse(test = length(.) > 1, yes =  head(n = -1), no = .)} %>% c
  # roberts half: c("rh_job_search", "initial_results") fails if head(n = -1) is used
  # match <- parentElemNmsRaw #%>% head(n = -1) %>% c

  arr <- list(
    str = match,
    iter = matchIdx[1]
  )

  lstRaw = jsonContent

  neighbours <- subsetByStr( # todo. rename
    lstRaw = lstRaw, # todo. rename
    arr = arr, # todo. rename
    targetValues = targetValues
  )

  # workaround
  nbr <- neighbours$lst
  if(!is.data.frame(nbr)) nbr <- nbr[[1]]

  list(
    targetKey = targetKey,
    texts = texts,
    isLargeHTML = isLargeHTML,
    neighbours = nbr,
    base = neighbours$base,
    baseFollow = neighbours$baseFollow
  )
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
# dir <- "tests/testthat"
# fileNames <- list.files(path = dir, pattern = ".RData")
# #fileName <- fileNames[1]
# for(fileName in fileNames){
#   print(fileName)
#   dir <- "tests/testthat"
#   load(file = file.path(dir, fileName))
#   response <- lst$response
#   base <- lst[["base"]]
#   baseFollow <- lst[["baseFollow"]]
#
#   targetKeys <- lst$targetKeys
#   if(!is.null(baseFollow)) print(2)
#   rm(list = c("response", "base", "baseFollow"))
# }


#, reqMethod = "GET", headers = NULL, body = NULL
unpack_JSON <- function(response, targetKeys, base, baseFollow = NULL){
  # if(is.null(base)){
  #   stop("Parameter base, provided to unpack_JSON(), is NULL - please provide a valid subset value.")
  # }
  # name <- sample(letters, size = 30, replace = TRUE) %>% {paste0(c("unpack_JSON_",., ".RData"), collapse = "")}
  # lst <- list(response = response, base = base, baseFollow = baseFollow, targetKeys = targetKeys)
  # save(file = name,list = "lst")

  response %<>% lapply(FUN = jsonlite::fromJSON)
  contentGETFlat <- response %>% unlist

  splitNames <- names(contentGETFlat) %>% strsplit(split = "[.]")
  lastKeys <- sapply(X = splitNames, FUN = tail, n = 1)

  # todo;do i neeed two of these subsetbystr functions?
  # todo; refactor
  if(is.null(base)) base <- NA
  if(suppressWarnings(any(is.na(base)))){
    if(length(response) == 1){
      # if i index here already it cant subset later correctly: example: "httpscareersglobelifeinsurancecomjobsjobsbycategory.RData".
      baseElems <- response #[[1]]
    }else{
      baseElems <- response
    }
  }else{
    baseElems <- lapply(response, FUN = subsetByStr2, arr = base)
  }
  if(is.null(baseElems)) stop("got a empty list trying to extract values from json.")

  # config parameter
  # todo: better refactor here?
  isLargeHTML <- baseElems %>%
    stringr::str_count(pattern = "<") %>%
    is_greater_than(5) %>% any

  # example for islargehtml false alarm: https://www.cfindustries.com/careers/list.html
  if(isLargeHTML & baseElems %>% unlist %>% length %>%  magrittr::equals(1)) return(
    list(
      res = baseElems %>% unlist
    )
  )

  # could go for rlist::list.flatten but how to ensure then that i have the elements of same length?
  # baseElems2 <- rlist::list.flatten(baseElems) %>% list ###### TRY ME

  if(baseElems %>% unlist %>%  is.null) return(NULL)
  if(!length(baseElems)) return(NULL)


  # requirements: baseElems[[1]][[1]]$Value[8] for https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25678&siteid=5275#keyWordSearch=&locationSearch=
  targetKey <- targetKeys[1]
  texts <- lapply(targetKeys, function(targetKey){
    baseElem <- baseElems[1]
    raw <- sapply(baseElems, function(baseElem){
      if(!is.null(baseFollow)){
        #### bei https://www.fbhs.com/careers  baseElem$baseFollow bzw. baseElem[["baseFollow"]]
        baseElem <- baseElem[[baseFollow]] #subsetByStr3(lstRaw = baseElem, arr = baseFollow)
      }
      subsetByStr3(lstRaw = baseElems, arr = targetKey)
    }, USE.NAMES = FALSE)
    raw2 <- sapply(raw, paste, collapse = " | ") %>% unname
    if(!length(raw2)) return(raw2)
    df <- data.frame(raw2, stringsAsFactors = FALSE)
    df <- setNames(df, paste(targetKey, collapse = "|"))
    df
  })

  ff <- rlist::list.flatten(texts)
  res <- do.call(what = cbind, texts)
  colnames(res) <- targetKeys
  rownames(res) <- NULL

  addLinks <- TRUE
  links <- sivis$browserOutput$links
  if(addLinks & nrow(res) == length(links)){
    res$Href <- links
  }

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

# str <- doc %>% toString
#
# target <- "joghurt"
# color <- "yellow"
# str %<>% gsub(pattern = target, replacement = glue('<span style = "color:{color}">{target}</span>'))
#   # safeDeparse()
#
#
# showHtmlPage(str, fileExt = ".html")
# %>%
#   regmatches(x = str) %>%
#   unlist

showHtmlPage <- function(doc, fileExt = ".html"){
  tmp <- tempfile(fileext = fileExt)
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

  #header[is.na(body)] <- ""
  return(body)
}

anonymise <- function(str){
  responseString2 <- gsub(pattern = 1:9 %>% paste(collapse = "|"), replacement = 3, x = str)
  responseString2 <- gsub(pattern = c("a", "e", "i", "o") %>% paste(collapse = "|"), replacement = "e", x = responseString2)
  gsub(pattern = setdiff(x = letters, y = c("a", "e", "i", "o", "u", "n", "l")) %>% paste(collapse = "|"), replacement = "m", x = responseString2)
}


post_Code_As_Is <- function(doc, prod_Iterations, mail, timing) {

  lines <- readLines(doc$path)
  start <- which(lines == "```{r}")[1] + 1
  end <- which(lines == "```")[1] - 1
  code <- lines[start:end] %>% paste(collapse = "\n")

  code %>% cat

  localTest <- eval(parse(text = code))

  code <- URLencode(code, reserved = TRUE, repeated = TRUE)
  # code <- URLencode("GET('http://www.r-bloggers.com')", reserved = TRUE)
  #URLdecode(code) -> f
  #eval(parse(text = f))

  body = paste0(
    "code=", code,
    "&mail=", mail,
    "&timing=", timing,
    "&prod_Iterations=", prod_Iterations
  )

  method <- "code_As_Is"
  httr::POST(
    url = glue("{host}:{port}/{method}"),
    body = body
  ) %>%
    content %>%
    unlist

}

post_Code_Params <- function(host, port, method, prod_Iterations, mail, timing){

  print("xx")
  body = paste0(
    "&url=", sivis$url %>% URLencode(reserved = TRUE),
    "&xpathes=", sivis$XPathes %>% URLencode(reserved = TRUE),
    "&rootXPath=", sivis$rootXpath %>% URLencode(reserved = TRUE)
  )

  method <- "codeparams"
  httr::POST(
    url = glue("{host}:{port}/{method}"),
    body = body
  ) %>%
    content %>%
    unlist

}


# host = "http://192.168.1.11"
# port = 7192
# mailAddress = "liebrr@gmail.com"
post_To_Production <- function(
  mailAddress = "liebrr@gmail.com", doc = getActiveDocumentContext(), code_As_Is = FALSE,
  host = "http://192.168.1.11", port = 7192, prod_Iterations = 1e5, ...){

  ####### Debug Plumber if it works.
  # # set productive
  url <- paste0("http://192.168.1.11:7192/echo")
  httr::GET(url = url, body = "msg=asd", timeout = 1) %>% content

  rstudioapi::documentSave(id = doc$id)

  mail <- URLencode(
    URL = mailAddress,
    reserved = TRUE,
    repeated = TRUE
  )

  timing <- "" %>% #list(...)
    safeDeparse() %>%
    URLencode(reserved = TRUE)


  if(code_As_Is){

    post_Code_As_Is(host, port, method, doc, mailAddress, prod_Iterations, mail, timing)


  }else{

    post_Code_Params(host, port, method, prod_Iterations, mail, timing)

  }

}


txt <- "
library(dplyr)
library(rlang)

1+1
"

silent_pack <- function(code){

  pattern <- code %>%
    gregexpr(pattern = "library[(].*?[)]", perl = TRUE) %>%
    regmatches(x = code) %>%
    unlist

  if(!length(pattern)) return(code)

  replace <- pattern %>%
  {glue::glue("suppressMessages({.})")}

  mgsub::mgsub(
    pattern = pattern,
    replacement = replace,
    string = code,
    fixed = TRUE) %>%
    cat

}

silent_pack(txt)

