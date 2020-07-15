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

# Problem in all_json_values - variable base_candidates - but did not find a good counter example yet.
# need a json response with mutliple base candidate ("rows")
# https://jobs.aon.com/api/jobs?page=2&sortBy=relevance&descending=false&internal=false

# search_parent_nodes takes ages for ralph-lauren - too many subnodes?


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



### browser + server response differ - check show_html_page
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


# todo: inpage source reparieren und von get und post untersuchungen trennen: {"identifier":"Siviscb_dataIdent","page_url":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","click_type":"contextmenu","selected_text":["\n            Mcclean, VA, US\n            \n        "],"links":[null],"XPath":"/html/body/div[2]/div[2]/div/div[4]/table/tbody/tr[1]/td[3]/span","XPathClassRaw":"/html[class = 'html5']/body[class = 'coreCSB search-page body']/div[class = 'outerShell']/div[class = 'innerShell']/div[class = 'content']/div[class = 'searchResultsShell']/table[class = 'searchResults full']/tbody/tr[class = 'dbOutputRow2 jobgrid-row']/td[class = 'colLocation']/span[class = 'jobLocation']","getUrl":"https://jobs.fastretailing.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=50","postUrl":"","postBody":{"raw":"[object Object]"},"InPageSource":true}
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
# todo: Missing clipBoardText: https://connekthq.com/plugins/ajax-load-more/examples/default/
#todo: FÃ¼r Unterschied von   url = cb_data$request$request$url und url <- cb_data$clipBoardText$page_url bei abendblatt.de. Ersterer ist irgendein JS von anderer Seite.






# print <- function(x, max = 500){
#   if(is.character(x)){
#     print(substr(x = x, start = 1, stop = min(max, length(x)))
#   }else{
#     print(x[1:max])
#   }
# }

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

########## individual base_follow names
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
# in response from server they are fine. Just an error in target_values.


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

# double get_json_structure --> means i need multiple target_keys?

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

cleaning <- FALSE
if(cleaning){
  rm(list = ls())
}

options(stringsAsFactors = FALSE)

#debugSource("~/sivis/rvestFunctions.R")

#source("QueryParams/createNewUrl.R")
sivis <- new.env(parent = emptyenv())
sivis$resp_content <- list()
sivis$timeout <- 6
sivis$tv_ratio_treshold <- 0.4
sivis$max_target_values <- 10
sivis$param_fuzzy <- 0.1
sivis$tv_extract_ratio <- 0.1
sivis$avoid_header <- TRUE


# ff <- cb_data$getUrl %>% GET %>% content %>% show_html_page

# cb_dataFlat <- readClipboard() %>% fromJSON(simplifyDataFrame = FALSE, flatten = FALSE)
# cb_data

# url %>% GET %>% content %>% show_html_page
# url <- "C:/Users/User1/Documents/sivis/test.html"


##
# For testing i need the following structure:
# A function i can feed in sivis environment with output: expected result data and
# input --> clipboard data + optional response object from server.
# (Optional, because it could either be objects from internet - that one i want to save because later on i might not
# be able to reproduce them [time variant data] or required, because i simulate a server with e.g. plumber.)

# In order to arrive there i need a preprocessing function that can arrive there. It creates all the necessary inputs
# for the testing



#' Check allowance to scrape in robotstxt
#'
#' robots.txt can give an indication whether a host allows a scraping of his website, see \url{https://en.wikipedia.org/wiki/Robots_exclusion_standard}.
#' It will be checked  whether a robots.txt is present. If the robotstxt disallows scraping the user is given the choice to
#' interrupt the process.
#'
#' @param page_url The url on which the data were selected. This is not necessarily the url of the request, but the url on which the content
#' was shown to the user in the browser.
#'
#' @return Is scraping allowed yes / no.
#'
#' @examples
#'
#' check_robotstxt("https://www.r-bloggers.com")
#'
check_robotstxt <- function(page_url){

  message("Checking robotstxt,...")

  domain <- urltools::domain(page_url)
  robots_URL <- paste0("https://", domain, "/robots.txt")

  # duplicate request, but cant extract this data from the package?
  status_code <- suppressMessages(
    robots_URL %>%
    httr::GET() %>%
    httr::status_code()
  )
  has_robotstxt <- status_code >= 200 & status_code < 300

  # Potential errors. E.g. Error: no more error handlers available (recursive errors?); invoking 'abort' restart
  scrape_allowed <- tryCatch(
    expr = suppressMessages(
      robotstxt::paths_allowed(
        paths = page_url,
        warn = FALSE
      )
    ),
    error = function(e){
      warning(glue::glue("Analysing robotstxt for '{page_url}' failed with: {e}. Setting result to TRUE."))
      return(TRUE)
    }
  )

  if(scrape_allowed){

    message(glue::glue("The requested page: '{robots_URL}' does{ifelse(has_robotstxt, yes = '', 'not')} yield a webiste with scraping rules."))
    message("There are no restriction in the robotstxt,... To be sure you should check the terms of usage on the website.")

  }else{

    message(
      glue::glue("robotstxt does not allow scraping this page, see {robots_URL}.")
    )

    user_choice <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to continue anyway?" #the page now?
    )

    if(user_choice == 1){

      #utils::browseURL(url = robots_URL)

    }else{

      return(FALSE)

    }

  }

  message("Proceeding with scraper building.")
  return(TRUE)

}


#' Create and run request to target url
#'
#' In order to find a path to extract the data from a given document, we first have to load the data in R by building a working request. We have the data that yielded a successful request from the browser. This data can not be used one to one, but is a great starting point and will yield succesful requests on
#' most web pages that do not prohibit web scraping.
#' Given this request we can download the document containg our target information.
#'
#' @param cb_data clipboard data. JSON from the Chrome addin shared via the clipboard.
#'
#'

create_run_request <- function(cb_data){

  if(is.null(cb_data$clipBoardText)){

    stop("No clipboard text found. The data from the Chrome addin is not sufficient. Please file an issue.")

  }

  page_url <- cb_data$clipBoardText$page_url
  can_scrape <- check_robotstxt(page_url)

  if(!can_scrape){

    return()

  }

  sivis$headers <- cb_data$request$request$headers %>%
    {setNames(object = .$value, nm = .$name)}

  if(grepl(pattern = "br", sivis$headers["accept-encoding"])){

    message("Info: removing br(otli) as accepted encoding as its not supported by curl.")
    sivis$headers["accept-encoding"] <- gsub(pattern = ", br|br, ", replacement = "", x = sivis$headers["accept-encoding"])

  }

  # resource type can be NULL
  resource_type <- cb_data$request$`_resource_type` %>%
    {ifelse(is.null(.), yes = "", no = .)}

  pattern <- "file:///"
  page_url <- cb_data$request$request$url
  url = cb_data$request$request$url # alternative: cb_data$clipBoardText$page_url
  ##url <- cb_data$clipBoardText$page_url

  if(grepl(pattern = pattern, x = url)){

    sivis$browser_output$page_url %<>% gsub(pattern = pattern, replacement = "")
    url %<>% gsub(pattern = pattern, replacement = "")

  }

  sivis$browser_output <- cb_data$clipBoardText
  target_values <- sivis$cb_data$clipBoardText$selected_text


  if(resource_type %in% "png"){

    stop(glue::glue("Wrong resource type: {resource_type}. File an issue with: resource_type = {resource_type},
              time: {Sys.time()}, source url = {url} and an indication if this issue would be reproducible at a later time or if your
              selected_text = {sivis$browser_output$selected_text[1]},... will likely change over time."))

  }


  resp_type <- cb_data$request$response$content$mimeType

  assign("url", value = url, envir = sivis)
  # if GET request was already performed in this session dont perform it again to avoid extensive amount of requests for the same server
  contact_details <- NULL
  agentName <- paste(contact_details, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36")
  #        user_agent(agentName)
  sivis$resp_content <- list()

  req_method <- sivis$cb_data$request$request$method
  req_method

  already_scraped <- sivis$url %in% names(sivis$resp_content)


  # cant merge post and get request together even though they just differ in the request method.
  # could set non needed body for get request to NULL.
  # But dont know to set an empty get value yet, see https://stackoverflow.com/questions/59971870/how-to-set-empty-body-in-httr-get-request.

  if(req_method == "POST" & !already_scraped){

    sivis$request_body <- sivis$cb_data$request$request$postData$text
    # sivis$request_body <- sivis$cb_data$request$request$queryString

    # bdy <- sivis$request_body
    # list(bdy$value)
    # c(setNames(object = bdy$value, nm = bdy$name))


    verbose_log_name <- "verbose3.log"
    con <- file(verbose_log_name)
    sink(file = con, append = TRUE)
    sink(con, append = TRUE, type = "message")

    sivis$resp_content[[sivis$url]] <- tryCatch(expr = suppressMessages(
      httr::POST(
        url = sivis$url,
        httr::add_headers(.headers = sivis$headers),
        body = sivis$hdr_body,
        #httr::verbose(),
        httr::timeout(sivis$timeout)
      )
    ),
    error = function(e){
      warning(glue::glue("POST request with headers failed with {e}. Will attempt request without headers next."))
      return(NULL)
    })

    sink()
    sink(type = "message")
    sivis$verboseLog <- verbose_log_name %>% readLines %>% paste(collapse = "\n")
    unlink(verbose_log_name)


    # config parameter - timeout = 6
    no_header_request <- tryCatch(expr = suppressMessages(
      httr::POST(
        url = sivis$url,
        body = sivis$request_body,
        #httr::verbose(),
        httr::timeout(sivis$timeout)
      )
    ),
    error = function(e){
      warning(glue::glue("POST request without headers failed with {e}."))
      return(NULL)
    })

  }

  if(req_method == "GET" & !already_scraped){

    #config parameter
    sivis$resp_content[[sivis$url]] <- tryCatch(expr = suppressMessages(
        httr::GET(
          url = sivis$url,
          httr::add_headers(.headers = sivis$headers),
          httr::timeout(sivis$timeout)
          # httr::verbose()
        )
    ),
    error = function(e){
      warning(glue::glue("GET request with headers failed with {e}. Will attempt request without headers next."))
      return(NULL)
    })

    # check if i need headers.
    #config parameter
    no_header_request <- tryCatch(expr = suppressMessages(
        httr::GET(
          url = sivis$url,
          httr::timeout(sivis$timeout)
          # httr::verbose() # want to capture but not to show in console.
        )
    ),
    error = function(e){
      warning(glue::glue("GET request without headers failed with {e}."))
      return(NULL)
    })

  }

  with_header_request <- sivis$resp_content[[sivis$url]]

  # cant compare contents
  # example: https://www.macysjobs.com/search-results?
  # identical(sivis$resp_content[[sivis$url]] %>% content, noHeader %>% content))
  # should not take status code, because status code can be 200 but with empty result
  # in the end i am interested if it contains the target values or not, see solution above

  with_header_request$response_encoding <- with_header_request$headers[["content-type"]] %>%
    toString %>% # turning NULL into empty string
    strsplit(split = "charset=") %>%
    unlist %>%
    .[2]


  no_encoding_hints <- is.na(with_header_request$response_encoding)
  has_response <- class(with_header_request) == "response"
  if(no_encoding_hints & has_response){
    sivis$response_encoding <- "UTF-8"
    sivis$with_hdr_man_encod <- TRUE
  }

  no_header_request$response_encoding <- with_header_request$headers[["content-type"]] %>%
    toString %>%
    strsplit(split = "charset=") %>%
    unlist %>%
    .[2]

  # dont print for both if only one is used.
  # message("Sivis did not find an encoding specification in the server response. Defaulting to UTF-8.")
  no_encoding_hints <- is.na(no_header_request$response_encoding)
  has_response <- class(no_header_request) == "response"
  if(no_encoding_hints & has_response){
    sivis$response_encoding <- "UTF-8"
    sivis$no_hdr_man_encod <- TRUE
  }

  with_header_content <-  ifelse(
    test = class(with_header_request) == "response",
    yes = with_header_request %>% httr::content(type = "text", encoding = sivis$response_encoding),
    no = ""
  )

  no_header_content <-  ifelse(
    test = class(no_header_request) == "response",
    yes = no_header_request  %>% httr::content(type = "text", encoding = sivis$response_encoding),
    no = ""
  )

  # config parameter
  tv_ratio_treshold <- sivis$tv_ratio_treshold
  with_header_ratio <- sapply(
    X = target_values %>% gsub(pattern = "\n|\t", replacement = "") %>% trimws,
    FUN = grepl,
    fixed = TRUE,
    x = with_header_content,
    USE.NAMES = FALSE
  ) %>%
    {sum(.) / length(.)}

  with_header_works <- with_header_ratio %>%
    magrittr::is_greater_than(tv_ratio_treshold)

  # config parameter
  # funny exception that it works without headers only for https://www.zeit.de/index.
  no_header_ratio <- sapply(
    X = target_values,
    FUN = grepl,
    fixed = TRUE,
    x = no_header_content,
    USE.NAMES = FALSE
  ) %>%
    {sum(.) / length(.)}

  no_header_Works <- no_header_ratio %>%
    magrittr::is_greater_than(tv_ratio_treshold)

  if(!no_header_Works & !with_header_works){

    message(glue::glue("Status code of a request without headers is: {no_header_request$status_code}."))
    message(glue::glue("Status code of a request with headers is: {with_header_request$status_code}."))

    message(glue::glue("Amount of target values found - for request with headers: {with_header_ratio}. Success treshold is (higher than) {tv_ratio_treshold}."))
    message(glue::glue("Amount of target values found - for request without headers: {no_header_ratio}. Success treshold is (higher than) {tv_ratio_treshold}."))

    assign(x = "no_header_request", value = no_header_request, envir = .GlobalEnv)
    assign(x = "with_header_request", value = with_header_request, envir = .GlobalEnv)

    message("Response for the request with headers is saved to a variable named 'no_header_request'. It can be inspected visually (as html) with `show_html_page(no_header_request)`.")
    message("Response for the request without headers is saved to a variable named 'with_header_request'. It can be inspected visually (as html) with `show_html_page(with_header_request)`.")
    message("Executing `show_html_page(with_header_request)` now. It will be available in the viewer pane.")

    "show_html_page(with_header_request)" %>%
      parse(text = .) %>%
      eval

    message(glue::glue("target_values are: {target_values %>% paste(collapse = '\n ')}."))

    stop("request failed with and without headers.")

  }

  if(!with_header_works){

    message("Request with headers do not yield a response including the target values.")

  }

  only_nohdr_works <- !with_header_works & no_header_Works

  if(only_nohdr_works){

    message("but found them attempting a request without headers,...")
    sivis$resp_content[[sivis$url]] <- no_header_request
    sivis$use_header <- FALSE
    sivis$response_encoding <- no_header_request$response_encoding

    req_manual_encoding <- !is.null(sivis$no_hdr_man_encod)
    if(req_manual_encoding){
      message("Sivis did not find an encoding specification in the server response. Defaulting to UTF-8.")
    }

  }else{

    sivis$response_encoding <- with_header_request$response_encoding
    req_manual_encoding <- !is.null(sivis$with_hdr_man_encod)
    if(req_manual_encoding){
      message("Sivis did not find an encoding specification in the server response. Defaulting to UTF-8.")
    }

    sivis$use_header <- TRUE

  }

  #  # config parameter
  # cut target values after it was verified that target values are within response
  # This check is not performance critical and it should be done to ensure that
  # it is done on all target values
  target_values <- target_values[1:min(sivis$max_target_values, length(target_values))]


  if(sivis$avoid_header & no_header_Works) sivis$use_header <- FALSE

  # for saving browser data for reproducibility
  urlShort <- gsub(
    x = sivis$url,
    pattern = "[:]|[.]|[/]|[?]|[&]|[=]|[%]|[-]",
    replacement = "",
    perl = TRUE
  ) %>%
    substring(first = 1, last = 50) # have restriction in

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
    #DONEDONE# fl <- "httpscareersintuitcomjobsearch.RData" too many target_values
    fl <-  "tr_j_httpscareersgooglecomapijobsjobsv1searchcompanyGooglecompanyYouTubehlenjloenUSlocationZC3BCrich2C20S.RData"

    load(file = paste0("R/fromWeb/", fl))
    target_values <- sivis$browser_output$selected_text
    xpath_from_browser <- sivis$browser_output$XPath
    page_url <- sivis$browser_output$page_url

  }else{

    xpath_from_browser <- cb_data$clipBoardText$XPath
    target_values <- sivis$cb_data$clipBoardText$selected_text

    missing_dir <- !dir.exists("R/fromWeb")
    if(missing_dir) dir.create("R/fromWeb")

    fileName <- paste0(file.path(getwd(), "R/fromWeb", substring(text = urlShort, first = 1, last = 101)), ".RData")
    # print(fileName)
    save(sivis, file = fileName)
    return(sivis)

  }

}

# target_values:
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
    if(sivis$use_header %>% is.null) sivis$use_header <- FALSE
    save(sivis, file = paste0("R/fromWeb/", fl))


    assign(x = "fileName", value = paste0("test", nr, ".Rmd"), envir = sivis)
    #sivis[["fileName"]] <- 1#paste0("test", nr, ".Rmd")
    test_run <- FALSE
    test_eval <- FALSE
    extract_data_write_rmd(sivis, test_run = test_run, test_eval = test_eval)
    tryCatch(
      expr = extract_data_write_rmd(sivis, test_run = test_run, test_eval = test_eval),
      error = function(e) print(e)
    )



    Sys.sleep(3)
  }
}


## fromChrome from chrome new scraper


#' Create an Rmd script with scraping code by Chrome data in the clipboard
#'
#' An Rmd script is created with reproducible scraping code. Input is read from clipboard
#' with `readClipboard()`. The clipboard is filled from the chrome addin with a json containing
#' target data, the page url and meta data for a working request to request this page. \cr \cr
#'
#' The result is that an Rmd document opens. \cr
#' This is the top level function called by the user that initiates the process.

#' @export
#'
create_scraper <- function(){

  nr <<- 19

  host <<- "192.168.1.11"
  #host <- "77.182.65.66"
  # host <<- "10.22.20.120"

  sivis$cb_data <- readClipboard() %>% jsonlite::fromJSON()
  sivis$cb_data$clipBoardText$selected_text %>% head
  sivis$cb_data$request$request$url

  cb_data <- sivis$cb_data
  sivis <- create_run_request(cb_data)

  test_run <- FALSE
  test_eval <- FALSE
  #newscraper
  success <- extract_data_write_rmd(sivis, test_run = test_run)

  success
}


#
# sivis$cb_data$request
# #, encoding = "UTF-8"
# extract_meta$response_string2 <- get_result %>% content(type = "text/plain")
#
# grep(pattern = "&#8211;", x = extract_meta$response_string2)
# grep(pattern = "-", x = extract_meta$response_string2)




#' Prepare the extraction phase with the initial data from chrome
#'
#' The function follows \code{\link{create_run_request}} and is nested within \code{\link{extract_data_write_rmd}}.
#' After a succesful request was performed, the data will be transformed for the transaction phase.
#' Target values are potentially adapted due to encoding issues, request body and headers are formatted, request
#' method is extracted etc.
#'
#'#' @return
#'\itemize{
#' \item{cb_data = the data from the clipboard / addin.}
#' \item{req_method = Request method being used. Potentially all methods from \url{https://developer.mozilla.org/de/docs/Web/HTTP/Methods}. Currently POST and GET request are supported.}
#' \item{body = request body, see \url{https://en.wikipedia.org/wiki/HTTP_message_body}}
#' \item{xpath_from_browser = The Xpath generated by the addin in Chrome.}
#' \item{page_url = The request url of the target page.}
#' \item{target_values = The selected text by the user in the browser.}
#' \item{headers = The header of the request.}
#' \item{use_header = Binary parameter (true / false) should the request header be used or not for the request from R.}
#'}

prepare_extraction <- function(sivis){

  extract_meta <- list()

  req_method = sivis$cb_data$request$request$method
  cb_data = sivis$browser_output

  if(!length(sivis$resp_content)){

    stop("empty get contents in sivis.")

  }

  get_result = sivis$resp_content[[1]]

  body = sivis$request_body
  headers = sivis$headers

  use_header <- !is.null(sivis$use_header)
  if(!use_header){

    sivis$use_header <- FALSE

  }

  status <- get_result %>%
    httr::status_code()

  if(status < 200 | status > 300){

    glue::glue("Request did not seem to be successful: Status code of server response is: {status}") %>% warning

  }

  #### fl <-  "https://www.rewe.de/angebote/hamburg/540724/rewe-markt-waldweg-43/" example for non replacement of \n
  target_values <- sivis$browser_output$selected_text %>% trimws #gsub(pattern = "\n", replacement = "") %>%

  extract_meta$response_string <- get_result %>%
    httr::content(type = "text", encoding = sivis$response_encoding)

  if(is.na(extract_meta$response_string)) stop("response body from server is not available.")
  if(!nchar(extract_meta$response_string)) stop("response body from server seems to be empty.")
  #extract_meta$response_string %>% show_html_page()

  ## config parameter - max check
  target_values <- target_values[1:min(length(target_values), sivis$max_target_values)]
  target_in_response <- sapply(target_values, grepl, x = extract_meta$response_string, fixed = TRUE) %>%
    {sum(.) / length(.)} %>%
    magrittr::is_greater_than(sivis$tv_ratio_treshold)
  if(!target_in_response) stop("target values not in filtered server response.")


  xpath_from_browser <- sivis$browser_output$XPath
  # for additional on https://www.macysjobs.com/search-results? i need the request url, not the page url.
  # page_url <- sivis$browser_output$page_url
  page_url <- sivis$cb_data$request$request$url


  # config parameter - 0.1
  # adjusting target values for findable values in response body.

  # todo: target_values with "&" a problem?
  # target_value <- target_values[5]

  # alle sonderzeichen: ? - / \
  # grep("as\asss", pattern = "\\")
  # changed to
  # "SAP Basis-Berater (m\/w\/divers)", "Berater (m\/w\/divers)
  # grep(extract_meta$response_string, pattern = "&amp;", fixed = TRUE)
  # grep(extract_meta$response_string, pattern = "", fixed = TRUE)
  # responseToText <- gsub(extract_meta$response_string, pattern = "&amp;", replacement = "&", fixed = TRUE)

  r <- unescape_html2(extract_meta$response_string)
  # responseToText <- gsub(responseToText, pattern = "\/", replacement = "/", fixed = TRUE)

  if(!sum(nchar(target_values))) stop("target_values are empty")

  new_target_values <- sapply(target_values, FUN = function(target_value){
    aregexec(
      pattern = target_value,
      # todo: check if that has a better alternative.
      text = r,
      max.distance = nchar(target_value)*sivis$param_fuzzy,
      fixed = TRUE
    ) %>%
      regmatches(x = r)
  }, USE.NAMES = FALSE)

  # in case of encoding errors i get funny output like ????-??. No need for aregex to avoid funny matches like "löschen?"
  encoding_err <- grepl("^[?-]+$", target_values)
  reg_not_found <- !(new_target_values %>% lengths)

  #NotMatchAfter <- newtarget_values[!excluded] %>% unlist %>% substr(start = 1, stop = 20)
  not_match_before <- target_values[!reg_not_found & !encoding_err]
  not_match_after <- new_target_values[!reg_not_found & !encoding_err] %>% unlist
  fuzzy_matches <- which(unlist(not_match_after) != not_match_before)

  if(length(fuzzy_matches)){

    fuzzyBefore <- not_match_before[fuzzy_matches] %>%
      paste(collapse = '\", \"') %>%
      c('"', ., '"') %>%
      paste(collapse = "")

    fuzzyAfter <- not_match_after[fuzzy_matches] %>%
      paste(collapse = '\", \"') %>%
      c('"', ., '"') %>%
      paste(collapse = "")

    warning(glue::glue("target_values:\n {fuzzyBefore} \nchanged to\n {fuzzyAfter}, \nbecause they where not found with a direct match. But only with a fuzzy match using a fuzzy parameter of {sivis$param_fuzzy}. \nConsider configuring that parameter if that change was inaccurate."))

  }

  new_target_values  %<>% unlist
  if(length(new_target_values)) target_values <- new_target_values  %>% unlist

  # split for text/html, because dont want to differentiate between encoding!?
  content_type <- get_result$headers$`content-type`

  # content type can be NULL? see https://www.accenture.com/de-de/careers/jobsearch?jk=&sb=1

  # could also extract encoding here?? utf-8?
  has_content_type <- !is.null(content_type)
  if(has_content_type){

    doc_type <- content_type %>%
      strsplit(split = ";") %>%
      unlist %>%
      .[1]

  }else{

    doc_type <- NULL

  }

  # check if json is within string.
  resource_type <- sivis$cb_data$request$`_resource_type`
  if(is.null(resource_type)) resource_type  <- ""
  if(resource_type == "script" & doc_type == "application/json") doc_type <- "script/json"

  extract_meta$doc_type <- doc_type
  extract_meta$extract_pathes = list()
  extract_meta$iter_nr <- 0

  return(
    list(
      extract_meta = extract_meta,
      cb_data = cb_data,
      req_method = req_method,
      body = body,
      xpath_from_browser = xpath_from_browser,
      page_url = page_url,
      target_values = target_values,
      headers = headers,
      use_header = use_header
    )
  )
}


#' Loop through extraction until target values are found and write corresponding pathes to an rmd file
#'
#' Extract the target values from the reseponse body. Target values might be nested in multiple data formats.
#' E.g. target values might be in a json, that is within javascript code that is within html code. Then
#' the xpath will be generated to identify the location within html, next regex is created to extract the json
#' from the javascript code. Next, an extraction path is generated that yields the target values from the json. \cr \cr
#' Finally write the required extraction path in the rmd doc
#'
#' @param sivis Environment to store extraction pathes etc. Todo: should be turned into an object
#' @param test_run Parameter for automated testing
#' @param test_eval Parameter for automated testing
#'
extract_data_write_rmd <- function(sivis = sivis, test_run = FALSE, test_eval = FALSE){

  const_meta <- prepare_extraction(sivis)
  has_meta <- !is.null(const_meta)

  if(!has_meta) return()

  extract_meta <- const_meta$extract_meta
  need_extraction <- TRUE

  response_string = extract_meta$response_string
  doc_type = extract_meta$doc_type
  extract_pathes = extract_meta$extract_pathes
  iter_nr = extract_meta$iter_nr

  while(need_extraction){

    cb_data = const_meta$cb_data
    req_method = const_meta$req_method
    body = const_meta$body
    xpath_from_browser = const_meta$xpath_from_browser
    page_url = const_meta$page_url
    target_values = const_meta$target_values
    headers = const_meta$headers
    use_header = const_meta$use_header

    extract_meta <- extract_data(

      response_string = extract_meta$response_string,
      doc_type = extract_meta$doc_type,
      extract_pathes = extract_meta$extract_pathes,
      iter_nr = extract_meta$iter_nr,

      cb_data = cb_data,
      req_method = req_method,
      body = body,
      xpath_from_browser = xpath_from_browser,
      page_url = page_url,
      target_values = target_values,
      headers = headers,
      use_header = use_header,

      test_run = test_run,
      test_eval = test_eval

    )

    eval_success <- extract_meta[["test_eval"]]

    if(!is.null(eval_success)){

      if(eval_success) return(TRUE)

    }

    # todo: Repair
    stop <- extract_meta[["all_found"]]
    need_extraction <- ifelse(is.null(stop), FALSE, !stop)
    need_extraction
  }

  return(TRUE)

}


unescape_html2 <- function(str){

  html <- paste0("<x>", paste0(str, collapse = "#_|"), "</x>")
  parsed <- xml2::xml_text(xml2::read_html(html, options = "HUGE"))
  strsplit(parsed, "#_|", fixed = TRUE)[[1]]

}


#' Wrapper function to create extraction pathes for html or jsons
#'
#'#' Parent function for \code{\link{get_json_structure}} and \code{\link{extract_html}}. \cr
#' This function can get multiple times if the target_values have to be extracted "across multiple levels". E.g. if within
# response is a json with an object including html.
# If it is called a second time, the document type (html) is not known before and has to be identified.

#' @param response_string Character string containing the target values. Initially, it will be the response body. In follow up iterations it will be
#' a part of it, e.g. a json extracted from the response body.
#' @param doc_type The document type of the response_string. Initially, the document type can be read from the response headers. In follow up rounds
#' it will be passed as NULL and determined within this function.
#' @param cb_data clipboard data. JSON from the Chrome addin shared via the clipboard.
#' @param xpath_from_browser The xpath being generated by the browser addin. It can be helpful choosing the correct xpath if there are multiple candidates.
#' @param body The body of the request.
#' @param extract_pathes The target data can be nested in multiple data formats. For example a json containg html code.
#' To build and dynamically update the scraper all extraction steps have to be available and in the correct order.
#' Therefore, this extraction steps will be stored in this variable: extract_pathes and updated in the corresponding
#' extraction functions.
#' @param page_url The url on which the data were selected. This is not necessarily the url of the request, but the url on which the content
#' was shown to the user in the browser.
#' @param target_values The strings selected by the user within the browser. For these values the corresponding extraction
#' method has to be found.
#' @param iter_nr This function can get multiple times if the target_values have to be extracted "across multiple levels".
#' To avoid being caught in an infinite loop it will be stopped after n rounds. n can be set in the config.
#' @param test_run Parameter for automated testing
#' @param test_eval Parameter for automated testing
#' @param req_method The required request method to perform the request. Potentially all metods of \url{https://developer.mozilla.org/de/docs/Web/HTTP/Methods}.
#' Currently supported are POST and GET requests.
#' @param headers The request headers.
#' @param use_header Binary parameter (TRUE / FALSE). Should a header be used when performing a request. Note that requests can
#' @param req_single_quote Boolean parameter if the json key and values are identified with a single quote. Example:
#' {'price': '69'} instead of {"price": "69"}
#'
#' @return Does not return values (yet). Todo: Change it, that create document is not nested within this function.
#'
extract_data <- function(response_string, doc_type = NULL, cb_data, xpath_from_browser = "", body = NULL, extract_pathes = list(), page_url = page_url,
                          target_values, iter_nr = 0, test_run = FALSE, test_eval = FALSE, req_method = "GET", headers = NULL, use_header = FALSE,
                          req_single_quote = NULL){

  iter_nr = iter_nr + 1
  # config parameter
  if(iter_nr > 6) stop("Too many iterations. Want to avoid getting caught in an infinite loop.")

  # script/json is not a pure json, but json within js code. not_json is also true in future extraction calls
  # when doc_type is NULL initially.
  not_json <- identical(doc_type, "script/json")

  if(not_json){

    doc_type_details <- find_doc_type(
      response_string = response_string,
      target_values = target_values
    )
    doc_type <- doc_type_details$type

  }

  if(doc_type == "application/vnd.oracle.adf.resourcecollection+json") doc_type <- "application/json"
  if(doc_type == "application/xhtml+xml") doc_type <- "text/html"

  is_json <- response_string %>% jsonlite::validate()
  if(is_json){

    # text/plain -> is json_object for https://akamaijobs.referrals.selectminds.com/jobs/search/5145592/page2
    # decided for generic json check
    doc_type <- "application/json"

  }

  might_be_json <- doc_type != "application/json" & grepl(pattern = "application", x = doc_type) & grepl(pattern = "json", x = doc_type)
  if(might_be_json){

    warning(glue::glue("doc_type: {doc_type} seems to be of type 'application/json'. Attempting the corresponding extraction method.
                  Please file an issue to add this document type to the list with an indication whether the scrape was successful."))
    doc_type <- "application/json"

  }

  might_be_html <- doc_type != "text/html" & grepl(pattern = "html", x = doc_type)
  if(might_be_html){

    warning(glue::glue("doc_type: {doc_type} seems to be of type 'text/html'. Attempting the corresponding extraction method.
                  Please file an issue to add this document type to the list with an indication whether the scrape was successful."))
    doc_type <- "text/html"

  }

  doc_type_unknown <- magrittr::not(doc_type %in% c("text/html", "application/json", "script/json"))
  if(doc_type_unknown){

    stop(glue::glue("For doc_type: '{doc_type}' there is no extraction method available yet. Please file an issue."))

  }

  if(doc_type == "script/json"){

    json_extract <- json_from_string(
      str = response_string,
      regexStr = doc_type_details$jsonRegex,
      req_single_quote = doc_type_details$req_single_quote,
      index_nr = doc_type_details$JSONIdx,
      target_values = target_values
    )

    is_json_parsable <- jsonlite::validate(json_extract$jsons$jsons)
    if(!is_json_parsable) stop("Identified a json, but can not parse it with jsonlite.")

    response_string <- json_extract$jsons$jsons
    extract_pathes[[length(extract_pathes) + 1]] <- list(doc_type_details)
    names(extract_pathes)[length(extract_pathes)] <- "script_json_index"

    # have extracted a JSON now, can move on as if i would have gotten a JSON from the server.
    # The necessary extraction step is saved in variable above: extract_pathes.
    doc_type <- "application/json"

  }

  doc_type
  if(doc_type == "application/json"){

    return(
      evaluate_json(iter_nr, response_string, target_values, extract_pathes, req_single_quote, page_url, req_method, headers, use_header, body, test_run, test_eval, xpath_from_browser)
    )

  }else if(doc_type == "text/html"){

    # nest in else if otherwise json with html is extracted and jumped right into html extraction without adjusting the inputs
    return(
      evaluate_html(iter_nr, response_string, target_values, extract_pathes, xpath_from_browser, maxCheck, req_method, body, test_eval, use_header, XPathes, page_url, test_run = test_run)
    )

  }

}


#' Get extraction path from html
#'
#' Find the extraction path to extract the target values from a html. \cr
#' Wrapper for \code{\link{extract_html}}.
#'
#' @inheritParams  extract_data
#'
evaluate_html <- function(iter_nr, response_string, target_values, extract_pathes, xpath_from_browser, maxCheck, req_method, body, test_eval, use_header, XPathes, page_url, test_run){

  # config parameter
  maxCheck = 5
  html_result <- extract_html(
    response_string = response_string,
    target_values = target_values,
    extract_pathes = extract_pathes,
    xpath_from_browser = xpath_from_browser,
    maxCheck = maxCheck
  )

  html_result
  html_result$extract_pathes
  html_result$all_found

  # have to set xpath to environ/global variable, so that later on xpathes can be added
  XPathes <- html_result$extract_pathes$xpath
  extract_pathes[[length(extract_pathes) + 1]] <- html_result$extract_pathes
  names(extract_pathes)[length(extract_pathes)] <- "xpath"

  if(all(html_result$all_found)){

    if(test_run & !test_eval){
      return(
        list(all_found = TRUE)
      )
    }

    req_method <- sivis$req_method
    test_eval <- create_document(
      page_url = page_url,
      req_method = req_method,
      response_string = response_string,
      extract_pathes = extract_pathes,
      body = body,
      XPathes = XPathes,
      test_eval = test_eval,
      use_header = use_header
    )

    return(
      list(
        test_eval = test_eval
      )
    )

  }else{

    return(
      list(
        response_string = html_result$result_values,
        extract_pathes = extract_pathes,
        doc_type = NULL,
        xpath_from_browser = "",
        iter_nr = iter_nr,
        target_values = target_values,
        test_run = test_run,
        all_found = FALSE
      )
    )
  }

}


#' Identify the extraction path from a json
#'
#' Find the extraction path to extract the target values from a json. \cr
#' Wrapper around \code{\link{get_json_structure}}.
#'
#' @inheritParams extract_data

evaluate_json <- function(iter_nr, response_string, target_values, extract_pathes, req_single_quote, page_url, req_method, headers, use_header, body, test_run, test_eval, xpath_from_browser){

  json_struct <- get_json_structure(
    response_string = response_string,
    target_values = target_values,
    extract_pathes = extract_pathes,
    req_single_quote = req_single_quote
  )
  extract_pathes <- json_struct$extract_pathes

  json_struct$all_found

  if(json_struct$all_found){

    if(test_run & !test_eval){

      return(list(all_found = TRUE))

    }

    test_eval <- create_document_get(
      page_url = page_url,
      extract_pathes = extract_pathes,
      test_eval = test_eval,
      target_keys = NULL, # need this for call from shiny
      req_method = req_method,
      headers = headers,
      use_header = use_header,
      body = body
    )

    return(
      list(test_eval = test_eval, all_found = TRUE)
    )

  }else{

    return(
      list(
        response_string = json_struct$result_values,
        extract_pathes = json_struct$extract_pathes,
        doc_type = NULL,
        xpath_from_browser = xpath_from_browser,
        iter_nr = iter_nr,
        target_values = target_values,
        test_run = test_run,
        all_found = FALSE
      )
    )

  }

}


create_document_getWrap <- function(target_keys){

  page_url <- sivis$page_url
  extract_pathes <- sivis$extract_pathes
  test_eval <- sivis$test_eval
  req_method <- sivis$req_method
  headers <- sivis$headers
  use_header <- sivis$use_header
  body <- sivis$body

  create_document_get(
    page_url = page_url,
    extract_pathes = extract_pathes,
    test_eval = test_eval,
    target_keys = target_keys,
    req_method = req_method,
    headers = headers,
    use_header = use_header,
    body = body,
    searchMultiPage = FALSE
  )

}

#' Find the extraction path from html
#'
#' @inheritParams evaluate_html
#'
extract_html <- function(response_string, target_values, extract_pathes, xpath_from_browser = "", maxCheck = 5){

  XPathNr <- 1
  xpath <- ""
  text <- target_values[1]
  all_text <- target_values[1:min(length(target_values), maxCheck)] %>%
    gsub(pattern = "\n", replacement = "") %>%
    trimws

  strict_xpath <- FALSE
  url <- sivis$url
  sivis$doc = response_string %>% xml2::read_html()
  doc <- sivis$doc
  #doc %>% show_html_page
  attr = NULL #"class"
  by_index = TRUE

  # text = all_text[5]
  xpath_all_cols <- lapply(
    FUN = get_xpath,
    X = all_text,
    all_text = all_text,
    # url = url,
    strict_xpath = strict_xpath,
    doc = doc,
    attr = attr, #"class"
    by_index = by_index
  )

  # need single accces "[" for:  "httpswwwfocusde.RData"
  xpath_candidates <- lapply(xpath_all_cols, "[", "xpathes") %>%
    unlist %>%
    unique

  no_xpath_found <- is.null(xpath_candidates)
  if(no_xpath_found){

    stop("Could not find an xpath with get_xpath. Please file an issue.")

  }

  # alternative approach go for frequencies
  #xpath_candidates <- xpathes %>% c %>% table %>% data.frame

  #if(is.null(xpath_candidates))
  xpath <- xpath_candidates %>%
    adist(y = xpath_from_browser) %>%
    data.frame(dist = ., xpathes = xpath_candidates) %>%
    dplyr::filter(dist == min(dist)) %>%
    dplyr::select(2) %>%
    unname %>%
    unique %>%
    unlist %>%
    .[1] %>%
    paste0(., "[not(descendant::*)]") # could be config parameter. Only add if necessary.

  # need single accces "[" for:  "httpswwwfocusde.RData"
  # col_altern <- lapply(xpath_all_cols, "[", "col_altern") %>% unlist %>% unique
  # print(lapply(xpath_all_cols, "[", "col_atern") %>% unlist %>% unique)


  result_values <- doc %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text()

  target_values %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws
  result_values %<>% gsub(pattern = "\n|\r", replacement = "", fixed = TRUE) %>% trimws

  # config parameter
  approximate <- TRUE

  lengths <- target_values %>% nchar
  similar_ratio <- adist(x = target_values, y = result_values) %>%
    apply(MARGIN = 1, FUN = min) %>%
    divide_by(lengths)

  all_found <- similar_ratio %>%
    magrittr::is_less_than(sivis$tv_extract_ratio) %>%
    all

  if(!all(all_found)) warning("not all target_values found!")

  response_string = result_values
  doc_type <- find_doc_type(
    response_string = response_string,
    target_values = target_values
  )

  if(doc_type == "unknown type"){

    all_found <- TRUE

  }

  # todo: What do i want to do if not all values are found
  list(
    all_found = all_found,
    extract_pathes = list(xpath = xpath), #, col_altern = col_altern
    result_values = result_values
  )
}

# For the scheduled scrape i want to extract by index not by target_value, since the target values wll change.
# For the initial scrape i want to extract by target value. The index value i can not know so far.


#' Extract json from string
#'
#' In some cases a json, containing the target values, is nested within javascript code. This function
#' generates the extraction path for the json.
#'
#' @param json_regex Regex for extracting a json from javascript code. Either a json object or a json array.
#' @param index_nr If multiple jsons are present, the index_nr points at the json containing the target values
#' @inheritParams extract_data
#'
#' @examples
#' doc_script_json <- 'var doc = {target: "target_text"}'
#' json_from_string(doc_script_json, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = "target_text", index_nr = NULL)
#'
#' doc_script_json <- 'var doc = {target: "target_text"}; var doc2 = {target: "other text"}'
#' json_from_string(doc_script_json, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = "target_text", index_nr = NULL)
#' json_from_string(doc_script_json, json_regex = c(jsonObject = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = NULL, index_nr = 1)

json_from_string <- function(response_string, json_regex = c(json_object = "\\{(?:[^{}]+|(?R))*?\\}"), req_single_quote = FALSE, target_values = NULL, index_nr = NULL){
  # httpsjobsapigooglemcloudioapijobsearchcallbackjobsCallbackpageSize10offset0companyNamecompanies2Fc3f8
  #### recursion limit reached in PCRE for element 1

  all_jsons <- gregexpr(
    pattern = json_regex,
    perl = TRUE,
    text = response_string
  ) %>%
    regmatches(x = response_string) %>%
    unlist

  no_jsons <- !length(all_jsons)

  if(no_jsons){

    stop("Expected a JSON, but did not find one.")

  }

  has_target_values <- !is.null(target_values)
  has_index_nr <- !is.null(index_nr)
  if(!has_target_values & !has_index_nr) stop("Please specify either the target_values or the index_nr parameter.")

  if(has_target_values){

    jsons <- all_jsons %>%
      data.frame(jsons = ., match = grepl(pattern = paste(target_values, collapse = "|"), x = .), index = 1:length(.)) %>%
      dplyr::filter(match == TRUE)

  }else if(has_index_nr){

    jsons <- all_jsons %<>%
      data.frame(jsons = ., index = 1:length(.)) %>%
      dplyr::filter(index == index_nr) %$% jsons

  }

  if(req_single_quote){

    jsons$jsons <- gsub(
      x = jsons$jsons,
      pattern = "'",
      replacement = '"'
    )

  }

  return(list(jsons = jsons))

}

#' Get extraction path for a JSON object or array
#'
#' @inheritParams evaluate_json
#' @param response_string character string(s) of type JSON object or array containing the target_values
#'
#' @return A list of three variables:
#' \itemize{
#'   \item all_found: Boolean parameter. Are all target values found or should an additional extraction step by attempted.
#'   \item extract_pathes: See Input value.
#'   \item result_values: The resulting texts that are produced as output from the extraction step. In case the final
#'   extraction step is reached these texts equal target_values or obtain a minimal distance to it.
#' }
#' @examples
#' data("doc_json")
#' target_values <- "69"
#' get_json_structure(response_string = doc_json, target_values)
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"

get_json_structure <- function(response_string, target_values, extract_pathes = list(), req_single_quote = NULL){

  json_content <- lapply(response_string,  FUN = jsonlite::fromJSON)

  # handle json arrays (wihout names)
  is_json_array <- json_content %>%
    unlist %>%
    names %>%
    is.null
  is_json_array

  if(is_json_array){

    result_values <- json_content[[1]]
    need_more_extract <- FALSE

  }else{

    # config parameter  counter argument - need values for finding pattern?
    target_values <- target_values[1:min(length(target_values), sivis$max_target_values)]

    json_values <- all_json_values(
      json_content = json_content,
      target_values = target_values
    )

    # example: https://cboe.wd1.myworkdayjobs.com/External_Career_CBOE
    # todo: very dirty: collecting more examples
    # another example would be having title5 as tail(match, 1)
    # --> therefore need slim variant (without the numbers).
    # test for both. tes5t==tes5t and test5==test (want both to succeed)

    last_val <- tail(json_values$base, 1)
    last_val_slim <- gsub(
      x = last_val,
      pattern = "[0-9]",
      replacement = ""
    )

    if(!is.null(last_val) & !is.null(json_values$target_key)){

      if(last_val == json_values$target_key | last_val_slim == json_values$target_key){

        json_values$base %<>% head(n = -1)

      }

    }

    sivis$neighbours <- json_values$neighbours

    is_json <- jsonlite:::validate(json_values$texts)

    target_values %<>% gsub(pattern = "\n|\r", replacement = "") %>% trimws
    result_values <- json_values$texts %>% gsub(pattern = "\n|\r", replacement = "") %>% trimws

    need_more_extract <- is_json | json_values$is_large_html
    if(!need_more_extract){

      dist_matrix <- adist(x = target_values, y = result_values)
      distances <- apply(dist_matrix, 1, min, na.rm = TRUE)

      # config parameter
      # todo how do i decide whether everything was found or not??????
      found_ratio <- (distances / nchar(target_values) < 0.1) %>% {sum(.) / length(.)}

      # check if it is another json: example: httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData
    }

    if(!is.null(json_values$target_key) & !is.null(json_values$base)){

      if(tail(json_values$base, 1) == json_values$target_key) json_values$base %<>% head(-1)

    }

    json_to_extract <- list(
      reponse = "json",
      is_large_html = json_values$is_large_html,
      neighbours = json_values$neighbours,
      texts = json_values$texts,
      target_key = json_values$target_key,
      base = json_values$base,
      base_follow = json_values$base_follow
    )

    extract_pathes[[length(extract_pathes) + 1]] <- json_to_extract
    names(extract_pathes)[length(extract_pathes)] <- "json"

  }

  if(need_more_extract){

    return(
      list(
        all_found = FALSE,
        extract_pathes = extract_pathes,
        result_values = json_values$texts
      )
    )

  }

  ####################### INDEXING ########################
  # Indexing can be needed in both JSON arrays and JSON objects
  ### limits of indexing: httpssjobsbrassringcomTgNewUISearchAjaxMatchedJobs.RData"
  ### cant even find right pattern manually there?

  # add potential indexing of result_values

  # take all selected text not the limited amount (limitation is done due to performance issues) - here are no significant performance
  # issues to be expected
  target_seq <- result_values %in% sivis$cb_data$clipBoardText$selected_text %>% which
  amt_matches <- colSums(data.frame(sapply(sivis$cb_data$clipBoardText$selected_text, FUN = "==", result_values)), na.rm = TRUE)

  if(all(amt_matches > 1)){

    found_ratio <- (target_values %in% result_values) %>% {sum(.) / length(.)}

    target_seq <- target_seq[seq(from = 1, to = length(target_seq), by = amt_matches[1])] # assuming all amtMatches are the same - alternative go for table()

    # config parameter
    if(found_ratio > 0.4) all_found <- TRUE

    pattern <- target_seq %>%
      diff %>%
      table %>%
      {.[which.max(.)] / (length(target_seq) - 1)}

    if(length(pattern)){

      # config parameter
      if(pattern > 0.45){

        seq_by <- pattern %>%
          names %>%
          as.numeric

        extract_pathes[[length(extract_pathes) + 1]] <- list(
          start = target_seq[which(diff(target_seq) == seq_by)[1]],       #min(targetSeq),
          end = target_seq[tail(which(diff(target_seq) == seq_by), 1) + 1],         #max(targetSeq),
          by = seq_by,
          isString = " \n\tjsonlite::fromJSON() %>%",
          req_single_quote =  " \n\tgsub(pattern = \"'\", replacement = '\"') %>%"
        )

        names(extract_pathes)[length(extract_pathes)] <- "ArrayIndex"

      }else{

        stop("Finding a pattern in JSON array failed!")

      }

    }

    return(
      list(
        all_found = all_found,
        extract_pathes = extract_pathes,
        result_values = result_values[target_seq]
      )
    )

  }else{

    pattern <- target_seq %>%
      diff %>%
      table %>%
      {.[which.max(.)] / (length(target_seq) - 1)}

    if(length(pattern)){

      if(pattern > 0.45){

        seq_by <- pattern %>%
          names %>%
          as.numeric

        if(seq_by > 1){

          extract_pathes[[length(extract_pathes) + 1]] <- list(
            start = target_seq[which(diff(target_seq) == seq_by)[1]],       #min(targetSeq),
            end = target_seq[tail(which(diff(target_seq) == seq_by), 1) + 1],         #max(targetSeq),
            by = seq_by,
            req_single_quote = req_single_quote
          )
          names(extract_pathes)[length(extract_pathes)] <- "ArrayIndex"

        }
      }
    }
  }

  # config parameter
  if(found_ratio < 0.9){

    glue::glue("Only found {foundRatio*100} per cent of target values, while extracting from json.") %>% warning
    all_found <- FALSE
    if(found_ratio > 0.4) all_found <- TRUE

  }else{

    all_found <- TRUE

  }

  has_NAs <- json_values$texts %>% is.na %>% any
  if(all_found & has_NAs) message("All target values found, but extraction of JSON values yields additional NAs")
  if(has_NAs) message("Extraction of JSON values yields NAs")

  if(length(result_values) > length(target_values)){

    message("Found more values than expected!")

  }

  return(
    list(
      all_found = all_found,
      extract_pathes = extract_pathes,
      result_values = json_values$texts
    )
  )
}

addXPathFromShiny <- function(XPathes, source = "shiny"){
  rstudioapi::documentSave(id = "Notebook_scraping.Rmd") # do i need this? does it work?

  existingXPathes <- get_xpathFromScript()
  c(existingXPathes, XPathes) %>% unique

  #create_document(browser_output = browser_output, OneXPathOnly = FALSE)

}
# create_document <- function(page_url, extract_pathes, response_string, test_eval = FALSE, req_method = "GET", Code_3_Extract = NULL,
#                            use_header = FALSE, body = NULL, XPathes = "", searchMultiPage = TRUE, search_multi_cols = TRUE, rCode = NULL){



# response_string <- "asdd <a>"
# response_string <- "asd {a:b, c:d} asdasd {a:b, c:e}"
# response_string <- '{"a":"text","b":"<html>asd</html>"}'
# response_string <- '"/*<!--*/ var phApp = phApp || {\"widgetApiEndpoint\":\"' is identified as html
# ---> if html and html, then html has to have a script tag

# complete list of meme types: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types


#' Test if given document is of type html.
#'
#' @inheritParams find_doc_type
#'
#' @return Return boolean value (TRUE / FALSE) if response_string is of type html.
#'
#' @seealso \code{\link{find_doc_type}}
#'
#' @examples
#' doc_html <- "<html><div><a>target_text</a></div></html>"
#' is_html(response_string = doc_html, target_values = "target_text")
#'
is_html <- function(response_string, target_values){
  doc <- response_string %>%
    xml2::read_html()

  is_list <- doc %>%
    typeof %>%
    magrittr::equals("list")
  is_list

  # get false alarm for html if its text json instead, check if i find the tags
  # todo: do i really want to check if over sapply and df/matrix?
  isUsefulXPath <- sapply(
    X = target_values,
    FUN = get_xpath_by_text,
    doc = doc,
    only_tags = TRUE
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
  #   sapply(FUN = grep, x = response_string) %>%
  #   lengths %>%
  #   all

  #if(is_list & foundTags & length(tags) == 1) stop ("HEEEEEEEEEREEE")
  htmlFound <- is_list & isUsefulXPath
  return(htmlFound)
}




#' Get document type of response string
#'
#' Given the filtered response on the browser side the document type is evaluated.
#' Potential values are either json, html/xml, scriptJSON (which is a json within javascript code).
#' Or "unknown type". For details see 'Value'. \cr \cr
#' The identification of html is not straight forward, because plain text is also html.
#' html does not have to include the following tags: html + body because additional html content might be loaded via ajax.
#' The final goal within sivis is not to identify the document type, but the document
#' type is a vehical to identify an extraction method to yield the target values. \cr
#' So the question is if the string under consideration is structured in a way that xpath can be used to make a (sub)-extraction of the target values.
#' Therefore, the variable response_string is required to identify the document type html. It is attempted to
#' generate an xpath. If that is successful the document type html will be assigned. \cr \cr
#' Jsons are identified with jsonlite:::validate(). \cr \cr
#' Jsons within javascript code are identified with regex. For json objects "\\{(?:[^{}]+|(?R))*?\\}" and for
#' jsonArray  "\\[.*?\\]" is used. \cr
#' It can be the case, that both jsonArray and json_objects match and they contain the same
#' amount of matches as the structure is [{...}, {...}, {...},]. In that case
#' it is prefered to have a single json array, instead of multiple json_objects, since it is easier to parse.
#'
#'
#' @param response_string The text of the server response for which it is expected to contain the target values
#' @param target_values The strings selected by the user within the browser. For these values the corresponding extraction
#' method has to be found.
#'
#' @return Returns the document type of the response_string variable. Values are either json, html/xml, scriptJSON (which is a json within javascript code).
#' Or "unknown type" if none of the other values are matched. That would be the rare case when the target values are within unstructured text and it
#' would yield to difficulties finding an extraction method for them.
#' @export
#'
#' @examples
#' library(magrittr)
#' doc_html <- "<html><div><a>target_text</a></div></html>"
#' find_doc_type(response_string = doc_html, target_values = "target_text")
#'
#' doc_json <- '{"target": "target_text"}'
#' find_doc_type(response_string = doc_json, target_values = "target_text")
#'
#' doc_script_json <- 'var doc = {target: "target_text"}'
#' find_doc_type(response_string = doc_script_json, target_values = "target_text")
#'
#' doc_unknown <- "This is unstructured target_text."
#' find_doc_type(response_string = doc_unknown, target_values = "target_text")

find_doc_type <- function(response_string, target_values){

  is_json <- jsonlite:::validate(response_string)

  if(is_json){

    return(list(type = "application/json"))

  }

  ####if(length(response_string) > 1) stop("response_string has to be of length one.") # cant use, because i also want to check the final result_values
  is_html <- tryCatch(
    expr = is_html(response_string = response_string, target_values = target_values),
    error = function(e){
      warning("Testing for html failed. Assuming the document is not of type hmtl.")
      return(FALSE)
    }
  )

  # more complete solution if necessary: https://stackoverflow.com/questions/59695961/find-json-in-string-with-recursion-limit-in-r-windows.

  regexs <- c(
    json_object = "\\{(?:[^{}]+|(?R))*?\\}",
    json_array = "\\[.*?\\]"
  )
  json_regex <- regexs[1]

  script_json <- lapply(
    X = regexs,
    FUN = check_for_json,
    target_values = target_values,
    response_string = response_string
  )

  #todo: refactor
  matches <- script_json %>%
    sapply(FUN = "[", "is_match") %>%
    unlist

  has_matches <- sum(matches)
  if(has_matches){

    target_value_match_ratio <- script_json %>%
      sapply(FUN = "[", "match_ratio") %>%
      unlist
    match_winner <- target_value_match_ratio %>% which.max

    # it can be the case, that both jsonArray and json_objects match and they contain the same
    # amount of matches as the structure is [{...}, {...}, {...},]. In that case
    # i would prefer having a single json array, instead of multiple json_objects.
    all_equal <- length(unique(target_value_match_ratio)) == 1

    if(all_equal){

      len_json_object <- script_json$json_object$jsons %>%
        unlist %>%
        nchar %>%
        sum

      len_json_array <-  script_json$jsonArray$jsons %>%
        unlist %>%
        nchar %>%
        sum

      match_winner <- c(len_json_object, len_json_array) %>%
        which.max

    }

    script_json <- script_json[[match_winner]]

  }else{

    script_json <- list(is_match = FALSE)

  }

  #### todo: refactor this shit
  if(is_html){

    if(!script_json$is_match) return(list(type = "text/html"))

    if(script_json$is_match){

      has_script_tag <- response_string %>%
        xml2::read_html() %>%
        rvest::html_nodes(xpath = "/script") %>%
        length

      if(has_script_tag){

        return(list(type = "text/html"))

      }else{

        ## example: "httpswwwpepsicojobscommainjobspage1.RData"
        return(script_json)

      }

    }

  }else if(script_json$is_match){

    return(script_json)

  }else{

    return(list(type = "unknown type"))

  }
}

# regexs <- c("\\{(?:[^{}]|(?R))*\\}", "\\[.*?\\]")
# jsonRegex <- regexs[1]
# jsonRegex <- regexs[2]

# more complete solution if necessary: https://stackoverflow.com/questions/59695961/find-json-in-string-with-recursion-limit-in-r-windows.

check_for_json <- function(jsonRegex, response_string, target_values, req_single_quote = FALSE){
  if(is.null(target_values) | !length(target_values)) stop("target_values are empty")

  jsons <- gregexpr(
    pattern = jsonRegex,
    text = response_string,
    perl = T
  ) %>%
    regmatches(x = response_string) %>%
    unlist

  # now check for occurence of target values
  if(length(jsons)){
    # config parameter
    matches <- sapply(target_values, grepl, fixed = TRUE, x = jsons) %>% as.matrix

    noMatch <- !length(matches)
    if(noMatch) stop("no target_values match found in response_string.")

    # example for rowSums: "x_r_j_httpscareersactivisioncomsearchresults.RData"
    match_ratio <- matches %>% rowSums %>% {. / length(target_values)}
    #if(match_ratio < 0.9) warning(paste0("only ", match_ratio, " per cent of targets found in json wrapped in html text."))

    # config parameter
    if(sum(match_ratio) < 0.5){
      JSONIdx <- 0
      is_match <- FALSE
    }else{
      JSONIdx <- match_ratio %>% which.max
      is_match <- TRUE
    }
  }else{
    is_match <- FALSE
    JSONIdx <- 0
    match_ratio <- 0
  }

  if(is_match){
    # todo: modify in case only one of jsons uses single quotes
    is_json <- jsonlite::validate(jsons[JSONIdx])
    if(!is_json & jsons %>% length){
      jsons <- jsons %>% gsub(pattern = "'", replacement = '"')
      is_json <- jsonlite::validate(jsons[JSONIdx])
      req_single_quote <- is_json
      if(!is_json) ScriptJSON <- FALSE
    }
  }

  return(
    list(
      type = "script/json",
      is_match = is_match,
      match_ratio = max(match_ratio),
      jsons = jsons,
      jsonRegex = jsonRegex,
      JSONIdx = JSONIdx,
      req_single_quote = req_single_quote
    )
  )
}



# url <- sivis$browser_outputRaw$url
# base <- deparse(dput(sivis$initGET$base), width.cutoff = 500L)
# base_follow <- deparse(dput(sivis$initGET$base_follow), width.cutoff = 500L)
# target_keys <- deparse(sivis$target_keys, width.cutoff = 500L)

# this covers: text2json, json extraction and early exit for huge html.
# does not cover: follow-up process of html or only html


#' Title
#'
#' @param page_url
#' @param base
#' @param base_follow
#' @param target_keys
#' @param extract_pathes
#' @param req_method
#' @param body
#' @param headers
#' @param use_header
#'
#' @return
#' @export
#'
#' @examples
base_get_template <- function(page_url, base, base_follow, target_keys = NULL, extract_pathes, req_method = "GET", body = NULL, headers = NULL, use_header = FALSE){


  xpath <- paste0("\tresponse <- tryCatch(expr = response %>% xml2::read_html() %>% rvest::html_nodes(xpath = '", extract_pathes$xpath$xpath, "') %>% rvest::html_text(), error = function(e) NULL)")

  # todo: what if they are multiple extractions. then index differently
  indexes <- extract_pathes$script_json_index[[1]]$JSONIdx
  if(length(indexes) > 1){
    indexes <- extract_pathes$script_json_index %>% paste(collapse = ", ") %>% c("c(", ., ")") %>% paste(collapse = "")
  }

  regex <- extract_pathes$script_json_index[[1]]$jsonRegex %>%
    dput %>%
    safe_deparse()

  # todo: refactor, that no coercing to string
  handleQuotes <- ""
  if(identical(extract_pathes$script_json_index$req_single_quote, "TRUE")){
    handleQuotes <- "response %<>% gsub(pattern = \"'\", '\"')"
  }

  script_json_index <- paste0(c(
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
  json <- function(target_keys, base, base_follow){
    paste0('\tresponse <- unpack_json(
    \tresponse = response,
    \ttarget_keys = ', target_keys %>% unique %>% safe_deparse,',
    \tbase = ', base,',
    \tbase_follow = ', base_follow,'
    )$res')
  }

  #\treq_method = ', req_method, scrHdr, scrBdy, ',

  if("ArrayIndex" %in% names(extract_pathes)){
    from <- extract_pathes$ArrayIndex$start
    to <- extract_pathes$ArrayIndex$end
    by <- extract_pathes$ArrayIndex$by
    singleQuote <- extract_pathes$ArrayIndex$req_single_quote
    isString = extract_pathes$ArrayIndex$isString

    ArrayIndex <- glue::glue("\tseq <- seq(from = {from}, to = {to}, by = {by})")
    display <- paste0("tbl <- do.call(what = rbind, args = output) %>% \n\tunlist %>% \n\tunname %>%", singleQuote, isString," \n\t.[seq] %>% \n\tdata.frame(data = .)")
  }else{
    # dont unlist for sivis additional keys: "tr_j_httpscareersgooglecomapijobsjobsv1searchcompanyGooglecompanyYouTubehlenjloenUSlocationZC3BCrich2C20S.RData"
    # %>% unlist %>% unname %>% data.frame(data = .)
    display <- "tbl <- do.call(what = rbind, args = output) %>% .[complete.cases(.), ] %>% data.frame"
  }
  # indexes <- extract_pathes$script_json_index[[1]]$JSONIdx
  # if(length(indexes) > 1){
  #   indexes <- extract_pathes$script_json_index %>% paste(collapse = ", ") %>% c("c(", ., ")") %>% paste(collapse = "")
  # }

  hdr <- switch(is.null(headers) + 1, paste0(', add_headers(.headers = scraper$headers)'), "")
  bdy <- switch(is.null(body) + 1, paste0(', body = scraper$body'), "")

  # todo info in #so
  # https://www.nisource.com/careers/job-search benötigt double quotes im body
  # bdyQuote <- ifelse(test = grepl(x = body, pattern = '"'), yes = "'", no = '"')
  # scrBdy <- switch(is.null(body) + 1, paste0(',\n\tbody = ', bdyQuote, body, bdyQuote), "")
  # scrHdr <- switch(is.null(headers) + 1, paste0(',\n\theaders = ', headers), "")


  # create code from extract_pathes by using coding templates.
  extractions <- extract_pathes %>%
    names %>%
    mget(envir = environment(), inherits = TRUE)

  jsonsRaw <- extractions[names(extractions) == "json"]

  nr <- 1
  jsons <- c()
  if(length(jsonsRaw)){
    for(nr in 1:length(jsonsRaw)){
      vars <- extract_pathes[names(extract_pathes) == "json"]
      base <- vars[[nr]]$base %>% safe_deparse()
      base_follow <- vars[[nr]]$base_follow %>% safe_deparse()

      # if target_keys are added by shiny, take them, else take them from extract_pathes
      if(is.null(target_keys)){
        target_keys <- vars[[nr]]$target_key
        sivis$target_keys <- target_keys
      }
      extractions["json"][nr] <- jsonsRaw[[1]](target_keys = target_keys, base = base, base_follow = base_follow)
    }
  }

  extractionAll <- extractions %>% unlist %>%
    paste(collapse = "\n")

  # require a function here, because it should be optional to replace page_url with a pageSize or itemSize dynamic parameter
  xhr_header <- function(url_func){
    paste0(c(
      'library(DT)',
      'library(httr)',
      'library(glue)',
      '# ", 1 + (nr - 1)*maxItems,"',
      '# ", maxItems*nr,"',
      '',
      'scraper <- list(',
      # paste0('\turlHTTP = "', url, '",'),
      paste0('\turlGen = ', url_func, ','),

      #'\ t urlGen = function(nr){', paste0('\ t \ t paste0("', page_url, '")'), '\ t},',
      # paste0('\tbase = ', base,','),
      # paste0('\tbase_follow = ', base_follow,','),
      # paste0('\ttarget_keys = ', target_keys,','),
      paste0('\treq_method = "httr::', req_method, '"', scrBdy, scrHdr, ""),
      ')',
      '',
      '',
      'output <- list()',
      'hasResult <- TRUE',
      'nr <- 1',
      ''
    ), collapse = "\n")
  }

  sivis$xhr_header <- xhr_header

  request <- paste0(
    c('while(hasResult){',
      '\tSys.sleep(0.2)',
      '\tprint(nr)',
      '\turl <- scraper$urlGen(nr)',
      '\t',
      paste0(c('\tget_result <- httr::', req_method,'(url = url', bdy, hdr, ')'), collapse = ""),
      '\tif(get_result$status_code != 200) return(NULL)',
      paste0('\tresponse <- httr::content(get_result, type = "text", encoding = "', sivis$response_encoding,'")'),
      extractionAll
    ),
    collapse = "\n"
  )

  list(
    headers = xhr_header,
    request = request,
    display = display
  )
}
# get_result <- get(scraper$req_method)(url = url, body = scraper$body, add_headers(.headers = scraper$headers)) %>%

# missing source of the code.
safe_deparse <- function(expr){
  ret <- paste(deparse(expr), collapse="")
  #rm whitespace
  gsub("[[:space:]][[:space:]]+", " ", ret)
}

create_document_get <- function(page_url = page_url, target_keys = NULL, extract_pathes = extract_pathes, test_eval = FALSE,
                                req_method = "GET", headers = NULL, use_header = FALSE, body = NULL, searchMultiPage = TRUE){


  assign(x = "neighbours", value = sivis$neighbours, envir = .GlobalEnv)
  assign(x = "extract_pathes", value = extract_pathes, envir = .GlobalEnv)

  sivis$page_url = page_url
  sivis$extract_pathes = extract_pathes
  sivis$test_eval = test_eval
  sivis$req_method = req_method
  sivis$headers = headers
  sivis$use_header = use_header
  sivis$body = body
  sivis$target_keys <- extract_pathes$json$target_key %>% safe_deparse


  # need this later for the .rmd file, to get additional fields from the get/post request
  print("create_document_get")
  # fileName <- sivis[["fileName"]]
  #if(is.null(fileName))
  fileName <- "Notebook_Scraping.Rmd"
  # url <- sivis$browser_outputRaw$url

  base <- extract_pathes$json$base %>% dput %>% safe_deparse
  base_follow <- extract_pathes$json$base_follow %>% dput %>% safe_deparse


  body <- sivis$request_body
  if(is.null(use_header)) use_header <- FALSE
  headers <- switch(use_header + 1, NULL, headers %>% dput %>% safe_deparse)
  #is_large_html <- extract_pathes$json$is_large_html

  baseGet <- base_get_template(
    page_url = page_url,
    base = base,
    base_follow = base_follow,
    target_keys = target_keys,
    extract_pathes = extract_pathes,
    req_method = req_method,
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
  # i need sivis$url, sivis$xhrRequest, sivis$xhr_header

  if(searchMultiPage){
    url_func <- get_url_json_page_change(
      url = sivis$url,
      headerCode = sivis$xhr_header(url_func = glue::glue("function(nr){{'{page_url}'}}")),
      requestCode = sivis$xhrRequest
    )

    newurl_func <- url_func %>%
      deparse %>%
      trimws %>%
      paste(collapse = "")
    sivis$page_url <- newurl_func
  }else{
    newurl_func <- sivis$page_url
  }

  sivis$reproduceForPageChange <- paste(c(
    baseGet$headers(url_func = newurl_func),
    sivis$xhrRequest,
    baseGet$display
  ), collapse = "\n")


  if(test_eval){
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
                    paste0('This is a scraping suggestion for the following website: ', sivis$browser_output$url, '.'),
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
                    '\tactionButton(\n\t\tinputId = "update_document", \n\t\tlabel = "Add selected keys to document:"\n\t),',
                    '\tbr(),',
                    '\tcheckboxGroupInput(\n\t\tinputId = "additionalKeys", \n\t\tlabel = "Additional keys to parse: ", choiceValues = \n\t\tas.list(names(neighbours)\n\t),',
                    paste0('\tselected = "', sivis$initGET$target_key,'", choiceNames = choiceNames)'),
                    ')',
                    'server <- function(input, output, session){',
                    '\tobserveEvent(eventExpr = input$update_document,{',
                    '#\t\tfile.remove("Notebook_Scraping.Rmd")',
                    '\t\tsivis$target_keys <-  c(sivis$target_keys, input$additionalKeys)',
                    '\t\tcreate_document_getWrap(target_keys = sivis$target_keys)',
                    '\t\tstopApp(returnValue = invisible())',
                    '\t})',
                    '}',
                    '',
                    'runApp(\n\tappDir = shinyApp(ui, server), \n\tlaunch.browser = shiny::paneViewer(minHeight = "maximize")\n)',
                    '```'), collapse = "\n"),
    con = fileName)
  file.edit(fileName)
}


#' Create Rmd document with scraping code
#'
#' Sivis provides the user a script with code how to scrape the provided page.
#' This function uses building blocks of code to create this script.
#'
#' The Rmd script is split in four blocks:
#' 1. package loading and url specification
#' 2. Performing the request
#' 3. Extracting the content
#' 4. Display the results
#'

#'
#' @param page_url The url of the page ot scrape
#' @param extract_pathes
#' @param response_string
#' @param test_eval
#' @param req_method
#' @param Code_3_Extract
#' @param use_header
#' @param body
#' @param XPathes
#' @param searchMultiPage
#' @param search_multi_cols
#' @param rCode
#'
#' @seealso \code{\link{update_document}}

create_document <- function(page_url, extract_pathes, response_string, test_eval = FALSE, req_method = "GET", Code_3_Extract = NULL,
                            use_header = FALSE, body = NULL, XPathes = "", searchMultiPage = TRUE, search_multi_cols = TRUE, rCode = NULL){

  sivis$req_method <- req_method
  sivis$response_string <- response_string
  sivis$extract_pathes <- extract_pathes
  sivis$body <- body
  sivis$XPathes <- XPathes
  sivis$test_eval <- test_eval

  print("use_header")
  print(use_header)
  # XPathes <- sivis$XPathes
  OneXPathOnly <- TRUE #length(XPathes) == 1
  fileName <- sivis[["fileName"]]
  if(is.null(fileName)) fileName <- "Notebook_Scraping.Rmd"
  if(is.null(use_header)) use_header <- FALSE
  headers <- switch(sivis$use_header + 1, NULL, headers %>% dput %>% safe_deparse)
  hdr <- paste0('(add_headers(.headers = ', headers,'))')
  if(is.null(headers)) hdr <- "()"

  has_json <- !is.null(extract_pathes$json)

  if(has_json){

    libCall <- ""
    indent <- "\t"
    sivis$initGET <- extract_pathes$json
    base <- safe_deparse(dput(extract_pathes$json$base))
    base_follow <- safe_deparse(dput(extract_pathes$json$base_follow))
    target_keys <- safe_deparse(sivis$target_keys)

    get_template <- base_get_template(
      page_url = page_url,
      base = base,
      base_follow = base_follow,
      target_keys = target_keys,
      extract_pathes = extract_pathes,
      req_method = req_method,
      body = body,
      use_header = use_header,
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

    sivis$xhrRequest <- paste(c(get_template$request, getFinishTemplate), collapse = "\n")

    url_func  <- glue::glue("function(nr){{'{sivis$url}'}}")
    url_func <- getUrlJSONPageChange(
      url = sivis$url,
      headerCode = sivis$xhr_header(url_func = url_func),
      requestCode = sivis$xhrRequest
    )

    if(searchMultiPage){
      newurl_func <- url_func %>%
        deparse %>%
        trimws %>%
        paste(collapse = "")
    }else{
      newurl_func <- sivis$page_url
    }

    sivis$reproduceForPageChange <- paste(
      sivis$xhr_header(url_func = newurl_func),
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
      paste0('\tresponse <- url %>% httr::GET', hdr,' %>% httr::content(type = "text", encoding = "', sivis$response_encoding ,'")')
    ), collapse = "")

    if(is.null(Code_3_Extract)){
      Code_3_Extract <- paste0(c(
        '\txpath <- data.frame(',
        paste(c('\t\t"', XPathes, '"'), collapse = ""),
        '\t)',
        '\tresponse <- tryCatch(\n\t\texpr = response %>% xml2::read_html() %>% rvest::html_nodes(xpath = as.character(xpath)) %>% rvest::html_text(), \n\t\terror = function(e) NULL\n\t)'
      ), collapse = "\n")
    }

    Request_Extract <- paste(c(Code_2_Request, Code_3_Extract), collapse = "\n")

    if(searchMultiPage){

      url_func <- dynamic_url(
        url = sivis$url,
        requestCode = Request_Extract
      )$func

      if(is.null(url_func)){
        url_func <- glue::glue("function(nr) '{sivis$url}'") %>% toString
      }

      # need this for update_document function
      sivis$isMultiPage <- !is.null(url_func)
      sivis$page_url <- url_func
    }else{
      url_func <- sivis$page_url
    }

    getFinishTemplate <- ""
    indent <- ""

    if(sivis$isMultiPage){

      Code_1_LibUrl <-paste0(c(
        'library(httr)',
        'library(DT)',
        paste0(c('urlGen <- ', sivis$page_url), collapse = ""),
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
      #get_template %<>% gsub(pattern = "\n", replacement = "\n\t")

      # config parameter: limit loop
      Code_4_Display <- paste0(c(
        '\thasResult <- length(response) & nr < 3',
        '\toutput[[nr]] <- response',
        '\tnr <- nr + 1',
        '}',
        '',
        'output %>% \n\tlapply(FUN = data.frame) %>% \n\tdo.call(what = rbind) %>% \n\tDT::datatable()'
      ), collapse = "\n")

    }else{

      Code_1_LibUrl <-paste0(c(
        'library(httr)',
        'library(DT)',
        paste0('url <- "', url_func, '"')),
        collapse = "\n"
      )
      Code_4_Display <- 'response %>% data.frame %>% DT::datatable()'

    }

    # todo: do i still need this reproduce? already did it here?
    sivis$reproduceForPageChange <- Request_Extract

  }

  # sivis$cb_data$request$request$url
  # url <- sivis$cb_data$request$request$url
  # requestCode <- sivis$reproduceForPageChange
  # dynamic_url(url, requestCode)


  if(test_eval){
    success <- tryCatch(
      eval(parse(text = request_code)),
      error = function(e) return(FALSE)
    )
    return(TRUE)
  }


  # result of the function is sivis$more_cols
  # and multicol_filt in global environment. So this can be placed in a seperate function
  if(search_multi_cols){

    add_multi_cols(
      XPathes = XPathes,
      response_string = response_string,
      extract_pathes = extract_pathes,
      search_multi_cols = search_multi_cols,
      page_url = page_url
    )

  }else{

    multicol_filt <- NULL

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
                   'This is a scraping suggestion for the following website: ', page_url,
                   '',
                   '```{r}',
                   rCode,
                   '```',

                   '',
                   '```{r}',
                   paste0(c('post_To_Production(host = "http://', host,'")'), collapse = ""),
                   '```',
                   '',

                   sivis$more_cols),


                 collapse = "\n"),
    con = fileName
  )
  file.edit(fileName)
}


#' Identifying additional columns / variables from xml/html document
#'
#' Relevant data might be spread across multiple columns. Selecting all data at once by the user in the browser might yield difficulties for the extraction. Manually label each column / variable might get cubersome for the user.
#' Another possibility is to suggest additional variables / columns to the user.
#' Goal: Find neighbour columns but avoid displaying other data, that is not a neighbour column.
#'
#' @param XPathes todo: can there be multiple xpathes here? only one is used and only that makes sense. Todo: savely reduce to one.
#' @param response_string An string with html content
#' @inheritParams get_json_structure
#' @param search_multi_cols todo: i should be able to savely remove this parameter
#' @param page_url In the interactive session the user can select additional columns that are created with this function.
#' Then the Markdown file is updated. For that the url is required. Todo: It might be better to assign the url at an earlier
#' step to sivis$url?
#'
#' @return Does not return a value, since the result is required in the interactive session with the user. Therfore, the
#' value is written to a new environment called sivis.
#' The result of the function is written to sivis$more_cols.
#' and to multicol_filt in the global environment.
#'
#' @examples
#' data("doc_html")
#' sivis <- new.env(parent = emptyenv())
#' xpath <- "/html/body/table/tr/td[1]"
#' add_multi_cols(XPathes = xpath, response_string = doc_html, extract_pathes = list(), search_multi_cols = TRUE, page_url = "")
#' sivis$more_cols
#' sivis$root_xpath

add_multi_cols <- function(XPathes, response_string, extract_pathes, search_multi_cols, page_url){

  # todo ????
  sivis$more_cols <- "3"

  xp1 = XPathes
  sivis$XPathes <- XPathes
  # xp2 = extract_pathes$xpath$col_altern

  multicol_candidates <- get_multicol_candidates(
    response_string = response_string,
    xp1 = xp1,
    extract_pathes = extract_pathes
  )

  multicol_filt <- multicol_candidates$multicol_filtOutput %>%
    do.call(what = cbind) %>%
    apply(MARGIN = 2, FUN = gsub, pattern = "  |\t|\n|\r|View More|[|]", replacement = "") %>% # to better identify duplicates - could be made as config parameter
    .[, colSums(is.na(.) | !nchar(.) | . == " ") != nrow(.), drop = FALSE] %>% # remove NA cols
    .[, !duplicated(., MARGIN = 2), drop = FALSE] %>%  # remove duplicate cols
    {.[colSums(apply(., 1, is.na) %>% data.frame) != ncol(.), , drop = FALSE]}  # and remove complete NA rows
  #as.data.frame(col.names = colnames(.)) %>% # ensure two dimensions - not needed anymore due to drop = FALSE?

  if(length(multicol_filt)){
    # make the order that columns which have only same values appear at last, because they might contain only title
    # of other columns or other irrelevant data
    only_dupes_in_col <- apply(multicol_filt, 2, FUN = function(col) col %>%
                                 table %>%
                                 {max(.) == length(col)}) %>%
      which

    order <- c(setdiff(1:ncol(multicol_filt), only_dupes_in_col), only_dupes_in_col)
    multicol_filt <- multicol_filt[, order, drop = FALSE]

    # config parameter - remove empty character columns
    non_empty_col <- multicol_filt %>%
      apply(MARGIN = 2, FUN = function(col) col %>%
              gsub(pattern = "\n|\t", replacement = "") %>%
              nchar %>%
              sum(na.rm = TRUE) %>%
              magrittr::is_greater_than(0))

    multicol_filt <- multicol_filt[, non_empty_col, drop = FALSE]

    if(!exists("nr")){
      nr <- 999
      save(multicol_filt, file = paste0("multicol_filt_", nr, ".RData")) # for batch testing of notebook_scraping.rmd files
    }
    root_xpath <- multicol_candidates$root_xpath

    additionColExist <- ncol(multicol_filt)

  }else{

    additionColExist <- FALSE

  }

  if(additionColExist){
    mat <- apply(multicol_filt, 1, function(row) !is.na(row) & nchar(row)) %>% data.frame
    idx1 <- apply(mat, 2, sum) %>% order(decreasing = TRUE)

    missing <- mat[, idx1[1], drop = FALSE] %>% magrittr::not() %>% which
    idx2 <- apply(mat[missing, , drop = FALSE], 2, sum) %>% order(decreasing = TRUE)

    idx <- c(idx1[1], setdiff(idx2, idx1[1]))
    multicol_filt <- multicol_filt[idx, , drop = FALSE]
    assign("multicol_filt", multicol_filt, envir = .GlobalEnv) # need the variable accessible in scraping script, to make selections from shiny

    # config parameter
    selectedCol <- 1 # initially only the column is selected, that was selected in the browser.

    # mostly more columns will be selected than not selected by the user-
    # However, if all columns will be selected at the beginning the user, initially,
    # might not realise that columns cann be selected
    selectedCol <- 1:ncol(multicol_filt)

    sivis$more_cols <- add_cols_option(
      multicol_filt = multicol_filt,
      root_xpath = root_xpath,
      page_url = page_url,
      nr = nr,
      selectedCol = 0 # initially only the column is selected, that was selected in the browser.
    )
    sivis$root_xpath <- root_xpath
  }

  if(!additionColExist & search_multi_cols){
    sivis$more_cols <- paste0(
      c(
        "",
        "",
        "Did not detect any candidates for additional columns/variables."
      ), collapse = "\n"
    )
  }

  # need to return sivis environment for automated testing(?)! Otherwise test-add_multi_cols.R fails, because update to sivis can not
  # be detected.
  return(sivis)
}


add_cols_option <- function(multicol_filt, root_xpath, page_url, selectedCol, nr){
  paste0(
    c(
      '',
      '',
      'Potential new fields to consider:',
      '',
      '```{r}',
      paste0(c("#load('multicol_filt_", nr, ".RData')"), collapse = ""),
      '',
      'library(shiny)',
      '',
      'ui <- fluidPage(',
      '\th3(shiny::tags$b("Suggestions for additional data to scrape.")),',
      '\th5(shiny::tags$b("Click on columns to add/remove an xpath from the scraper.")),',
      '\tbr(),',
      '\tDT::dataTableOutput("tbl", height = "30em"),',
      '\tbr(),',
      '\th5(shiny::tags$i("Rows are ordered so that empty columns are attempted to be avoided on the first page.")),',
      '\tbr(),',
      '\tactionButton(',
      '\t\tinputId = "update_document",',
      '\t\tlabel = "Add selected columns to document:",',
      '\t\ticon("paper-plane"),',
      '\t\tstyle="color: #fff; background-color: #337ab7; border-color: #2e6da4"',
      '\t)',
      ')',

      'server <- function(input, output, session){',
      '',
      '\tobserveEvent(eventExpr = input$update_document,{',
      '\t\t#file.remove("Notebook_Scraping.Rmd")',
      paste0('\t\troot_xpath <- ', root_xpath %>% safe_deparse,' '),
      paste0('\t\tpage_url <- ', page_url %>% safe_deparse,' '),
      '\t\tXPathes <- colnames(multicol_filt)[input$tbl_columns_selected + 1]',
      '\t\tupdate_document(',
      '\t\t\tXPathes = XPathes,',
      '\t\t\troot_xpath = root_xpath,',
      '\t\t\tpage_url = page_url,',
      '\t\t\tselectedCol = input$tbl_columns_selected %>% sort %>% safe_deparse',
      ')',
      '\t\tstopApp(returnValue = invisible())',
      '\t})',
      '',
      '\toutput$tbl = DT::renderDataTable(',
      '\t\tDT::datatable(',
      '\t\t\tdata = multicol_filt,',
      paste0('\t\t\tselection = list(mode = "multiple", target = "column", selected = ', selectedCol,'),'),
      '\t\t\toptions = list(pageLength = 10, autoWidth = TRUE)',
      '\t\t) %>%',
      '\t\tDT::formatStyle(colnames(multicol_filt), lineHeight = "90%"),',
      '\t\tserver = FALSE,',
      '\t)',
      '}',
      '',
      'runApp(\n\tappDir = shinyApp(ui, server), \n\tlaunch.browser = shiny::paneViewer(minHeight = "maximize")\n)',
      '```'
    )
  )
}



#' Update the Rmd scraping document via user selection in Shiny.
#'
#' The user is given the option to add additional columns to the dataset via a shiny app.
#' After the user has chosen this columns the rmd scraping document has to be updated.
#' Initially the document is created via \code{\link{create_document}}. It will be called
#' again, just with adpated parameters.
#'
#' The Rmd script is split in four blocks:
#' 1. package loading and url specification
#' 2. Performing the request
#' 3. Extracting the content
#' 4. Display the results
#'
#' In this function block 3 is updated: Extracting the content (--> with new xpathes for additional columns)
#'
#' @param XPathes The selected xpathes from the user in the shiny app when switching from one to multiple columns
#' @param root_xpath The common root xpath for the XPathes parameter
#' @param page_url The url of the page ot scrape
#' @param selectedCol The selected column: Todo is this relevant?
#' @param doc The loaded document from page_url
#'
#' @export
#' @seealso \code{\link{create_document}}
#'
update_document <- function(XPathes, root_xpath, page_url, selectedCol, doc = getActiveDocumentContext()){

  page_url <- sivis$page_url
  xp <- paste(XPathes, collapse = ",\n\t")

  amtXP <- length(XPathes)

  Code_1_LibUrl <- paste0(c(
    'options(stringsAsFactors = FALSE)',
    'library(rvest)',
    'library(DT)',
    '',
    paste0('url <- ', page_url %>% safe_deparse())
  ), collapse = "\n")

  Code_2_Request <- 'response <- xml2::read_html(x = url)'

  # need this part to test for additional pages where similar data could be found.
  Code_3_Extract <- paste0(
    c('xpathes <- ', paste(c(paste(c("data.frame(", glue::glue("XPath{1:amtXP} = '{XPathes}'{c(rep(',', amtXP - 1), '')}\n\t")), collapse = "\n\t"), ")"), collapse = "\n"),
      'nodes <- response %>% xml2::read_html() %>% rvest::html_nodes(xpath = ', root_xpath %>% safe_deparse,')',
      'response <- lapply(xpathes, function(xpath){',
      '\tlapply(nodes, function(node) rvest::html_nodes(x = node, xpath = xpath) %>% {ifelse(length(.), yes = rvest::html_text(.), no = NA)}) %>% unlist',
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

  print("sivis$use_header")
  print(sivis$use_header)
  create_document(
    page_url = sivis$page_url,
    req_method = sivis$req_method,
    response_string = sivis$response_string,
    extract_pathes = sivis$extract_pathes,
    body = sivis$body,
    XPathes = XPathes,
    test_eval = sivis$test_eval,
    use_header = sivis$use_header,
    search_multi_cols = FALSE,
    Code_3_Extract = Code_3_Extract,
    rCode = NULL
  )

}




#' Create candidates for additional columns / variables
#'
#' Relevant data might be spread across multiple columns. Selecting all data at once by the user in the browser might yield difficulties for the extraction. Manually label each column / variable might get cubersome for the user.
#' Another possibility is to suggest additional variables / columns to the user.
#' Goal: Find neighbour columns but avoid displaying other data, that is not a neighbour column. \cr \cr
#'
#' This function generates candidates that will be filtered in \code{\link{add_multi_cols}}.

#' @inheritParams add_multi_cols
#' @param xp1
#'
#' @return Candidates for additional columns

#'@examples
#' data("doc_html")
#' doc <- xml2::read_html(doc_html)
#' xpath <- "/html/body/table/tr/td[1]"
#' tags <- rvest::html_nodes(doc, xpath = xpath)
#' get_multicol_candidates(doc_html, xpath, list())


get_multicol_candidates <- function(response_string, xp1, extract_pathes){

  doc <- response_string %>% xml2::read_html()
  tags <- doc %>% rvest::html_nodes(xpath = xp1)

  if(!length(tags)){

    stop(glue::glue("Found an invalid XPath: {xp1} while attempting to find a commonXPath."))

  }


  leafPathes <- get_leaf_pathes(doc, tags)

  xpathes <- xp1 #, extract_pathes$xpath$col_altern
  if(nchar(leafPathes$link_Path)){

    xpathes %<>% c(., leafPathes$link_Path)

  }

  # xpath <- xpathes[2]
  addXP <- sapply(xpathes, FUN = function(xpath){
    tags <- rvest::html_nodes(x = doc, xpath = xpath)
    all_text <- tags %>% rvest::html_text()
    xp <- get_xpath_by_tag(
      html_tag = tags[1],
      doc = doc,
      root_path = leafPathes$root_path,
      all_text = all_text,
      by_index = TRUE,
      get_other_cols = FALSE,
      just_one_tag = TRUE
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
  multicol_filt <- lapply(
    X = leafPathes$subPathes,
    FUN = get_text_given_root,
    root_path = leafPathes$root_path,
    doc = doc
  )

  multicol_filt %<>% .[!is.null(.)]

  multicol_filt %<>% .[!sapply(., is.null)]

  multicol_filtOutput <- sapply(multicol_filt, "[", "texts") %>%
    do.call(what = cbind) %>%
    data.frame

  names(multicol_filtOutput) <- sapply(multicol_filt, "[", "xpCand") %>%
    as.character

  list(
    multicol_filtOutput = multicol_filtOutput,
    root_xpath = leafPathes$root_path
  )
}

#' Get the text of all elements that fulfill roothPath + XPath
#'
#' Helper function to get neighbour columns of a target column. For details see: commonxpathdata
#'
#' @param root_path a
#' @param XPath v
#' @param doc html document yielded from the server response
#'
#' @return list of texts of the elements that were identified with the XPath given the root_path + this XPath that was used.

get_text_given_root <- function(root_path, XPath, doc){

  if(is.null(root_path)){
    root_path <- "/*"
  }

  nodes <- rvest::html_nodes(x = doc, xpath = root_path)
  texts <- lapply(
    X = nodes,
    FUN = rvest::html_nodes,
    xpath = XPath
  ) %>% lapply(
    FUN = function(node){
      ifelse(test = length(node), yes = rvest::html_text(node), no = NA)
    }
  )

  list(
    xpCand = XPath,
    texts = texts %>% unlist %>% gsub(pattern = "\n|\t", replacement = "")
  )
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
  xp <- get_xpathFromScript()

  if(newLink){

    browser_output <- sivis$browser_output
    if(is.null(browser_output)) browser_output <- readClipboard() %>% fromJSON

    links <- browser_output$links
    if(all(is.na(links))) stop("Provided clipboard data doesnt have links as href attributes.")

    allEqual <- all(duplicated(links, fromLast = TRUE) | duplicated(links, fromLast = FALSE))
    if(allEqual){

      warning("href Attribute is not iterable")
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

                  This is a scraping suggestion for the following website: ', as.character(sivis$browser_output$url),'.

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
                  code <- xml2::read_html(x = url)
                  scrapeOutput <- sapply(xpathes, function(xpath) rvest::html_nodes(x = code, xpath = xpath) %>% rvest::html_text())
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


get_xpathFromScript <- function(){
  rmdCode <- readLines("Notebook_Scraping.Rmd")

  # todo: clean matching on xpath data.frame
  startIdx <- sapply(rmdCode, grep, pattern = ' <- data.frame(', fixed = TRUE) %>% lengths %>% {. > 0} %>% which %>% unname
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
  firsthrefPart <- strsplit(hrefAttrText, ";")[[1]][1]
  hasJavascript <- grep(pattern = "javascript:", x = firsthrefPart)
  if(length(hasJavascript)){
    extractedRaw <- str_extract(
      string = firsthrefPart,
      pattern = "\\(([^)]+)\\)"
    )
    firsthrefPart <- gsub(pattern = "\\(|\\)|'", replacement = "", x = extractedRaw)
  }
  match <- substr(firsthrefPart, 1, 4) == "http"
  if(sum(match) / length(match) < 1){
    dom <- domain(baseUrl) # scraper[[compName]]$url
    url <- paste0("https://", dom,  firsthrefPart)
  }else{
    url <- firsthrefPart
  }
  return(url)
}


# text = browser_output$selected_text[XPathNr]
# text = all_text[1]
# todo: escaping quotes, see below and example in: httpswwwfocusde.RData


#' Get xpath given the target values.
#'
#' Wrapper function for \code{\link{get_xpath_by_tag}}. Before an xpath can be derived from tags,
#' this tags have to be identified. Tags are found by searching through the document for the target
#' values. It might be confusing, but it will also be done via xpath.\cr
#' A simplified form of that xpath would be: //text()[contains(., '", target_values,"')]"). It will search for nodes
#' with texts that contain the target values. The xpath search is case sensitive. But the rendered browser
#' texts selected by the user can be upper case while the server reponse contains lower case and vice versa.
#' Therefore, translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz') is used to make
#' the search for nodes case insensitive. Also spaces are removed, so that additional spaces wil not prohibit
#' a match. This case rarely occurs, but false positive cases are not known. So we can savely use it so far. \cr \cr
#' To summarize: This search is approximate as it uses contains(.), no spaces and case insensitivity. The upside of using it, is that in most
#' cases small derivations are no problem for sivis. In rare cases it might yield false positive results.
#' Therefore, a config parameter is introduced to disable this behaviour and allow an exact search.
#'
#' @param url page url - todo: seems to be used for usage outside of sivis. adapt that.
#' @param text the target value to find the xpath for
#' @param strict_xpath As written in the description the user has the possibility to
#' @param doc document containing the server response or extracted sub parts
#' @param attr Should xpath be generated by classes. Currently an open issue.
#' @param by_index Should xpath be generated by index.
#' @param all_text All target values. They are required to avoid over and underfitting the xpath.
#'
# '@examples

# deleted xpath = NULL, and url, if ever needed write a wrapper function for that.
get_xpath <- function(text = NULL, strict_xpath = FALSE, doc = NULL, attr = NULL, by_index = FALSE, all_text){

  # todo: can the xpath parameter be safely removed?

  ########### FOR ESCAPING QUOTES
  # text3 <- "How can select from same Id\\'s different values?"
  # text2 <- text %>%  gsub(pattern = "'", replacement = "\\\\'")
  # text2 <- text

  has_text <- !is.null(text)
  if(!has_text) stop("text cant be of type NULL")

  text_cleaned <- gsub(pattern = " |\n|\t", replacement = "", tolower(text))

  if(strict_xpath){

    xpath <- paste0("//*[text() = '", text_cleaned,"']")

  }else{

    xpath <- paste0("//text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", text_cleaned,"')]")

  }

  #doc %>% show_html_page
  # doc_missing <- is.null(doc)
  # if(doc_missing) doc <- xml2::read_html(x = url)

  # beispiel für nicht \n\t --> httpswwwrewedeangebotehamburg540724rewemarktwaldweg43.RData, weil ich nicht weiss wie in xpath replacen.
  # aber kann es im text nicht rausnehmen, weil dann andere beispiele failen, dann im dokument.
  doc %<>%
    gsub(pattern = "\n|\t", replacement = "") %>%
    xml2::read_html()

  tags <- doc %>%
     rvest::html_nodes(xpath = xpath)

  # Maybe only show this if creation failed? Otherwise its shown to often and kind of confusing.
  # if(length(tags) > 1) warning("Found more than one matching element!")

  has_tags <- length(tags)
  if(!has_tags){

    message("Did not find element as text, checking within attributes,...")

    xpath_of_attrib <- paste0("//@*[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'),'", text_cleaned,"')]")
    attribute_tags <- doc %>% rvest::html_nodes(xpath = xpath_of_attrib)

    # open issue: attribute tags are analysed but not used
    has_attribute_tags <- length(attribute_tags)
    if(has_attribute_tags){

      xpath_of_attrib <- get_xpath_by_tag(
        html_tag = tags[1] %>% rvest::html_nodes(xpath = ".."),
        all_text = target_values
      )

      message(glue::glue("target_values found in attribute: '{tags %>% rvest::html_name()}' in element with xpath: {xpath_of_attrib}."))
      tags <- attribute_tags

    }else{

      message("target_values also not found in attributes.")

    }

    glue::glue("Did not find element for {text}.") %>% warning
    return(NULL)

  }

  # somehow cant use apply family here. last tag vanishes.

  xpathes <- rep(NA, length(tags))
  col_altern <- list()
  # nr <- 2
  # tag <- tags[nr]

  for(nr in 1:length(tags)){

    all_xpath <- get_xpath_by_tag(
      html_tag = tags[nr],
      attr = attr,
      by_index = by_index,
      all_text = all_text,
      doc = doc
    )
    xpathes[nr] <- all_xpath$xpath
    col_altern[[nr]] <- unlist(all_xpath$xpathOtherCols)

  }

  col_altern %<>%
    unlist %>%
    unique
  message("Any additional unknown columns here? Otherwise safely remove!")
  print(col_altern)

  ###########get_leaf_pathes(doc, tag)
  list(
    xpathes = xpathes
    # ,col_altern = col_altern
  )

}

# by_index = FALSE
# tag <- tag[2]
# attr = "class"



# text <- "Anlagenmechaniker"
# doc <- contInit %>% read_html
# get_xpath_by_text(text, doc, exact = FALSE, attr = NULL, by_index = FALSE)
get_xpath_by_text <- function(text, doc, exact = FALSE, attr = NULL, by_index = FALSE, only_tags = FALSE, do_warn = TRUE){

  if(is.character(doc)){

    warning("doc is of type character - trying to convert to xml doc with xml2::read_html()")
    doc %<>% xml2::read_html()

  }

  text %<>% tolower %>% gsub(pattern = " ", replacement = "")
  xpath <- paste0("//*[text()[contains(translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'), '", text, "')]]")
  tag <- doc %>% rvest::html_nodes(xpath = xpath)

  tag_name <- ""
  tags <- c()

  multiple_tag <- length(tag) > 1
  if(multiple_tag){

    tag_names <- sapply(tag, rvest::html_name())
    match <- which(tag_names != "script")

    has_match <- length(match)
    if(!has_match) match <- 1
    tag <- tag[match[1]]

  }else if(length(tag) == 0){

    if(do_warn){
      warning(glue::glue("Did not find an xpath element that matches target text: {text}!"))
    }

    return(NULL)

  }

  # assumption: There is always a hmtl tag
  while(tag_name != "html"){

    tag_name <- tag %>% rvest::html_name()
    tags <- c(tags, tag_name)
    tag <- tag %>% rvest::html_nodes(xpath = "..")

  }

  if(only_tags){

    return(tags %>% unique)

  }

  xpath <- paste(c("", tags[length(tags):1]), collapse = "/")
  xpath

}

#tag <- tags[[1]]
by_index = TRUE
root_path = "/*"
all_text = NULL
get_other_cols = TRUE


#' Get the xpath given a html document and html tag
#'
#' @param attr A node can have multiple subnodes of the same type, but only a subset of them is relevant. Distinguishing between
#' nodes of the same type can be done by indexing or by classes. attr is a boolean parameter (true = use attributes, false = dont use attributes).
#' Choosing 'false' does not mean that indexes are used. See by_index for that.
#' @param by_index A node can have multiple subnodes of the same type, but only a subset of them is relevant. Distinguishing between
#' nodes of the same type can be done by indexing or by classes. by_index is a boolean parameter (true = use index, false = dont use index).
#' 'false' does not mean that attributes are used. See attr for that.
#' @param all_text all target_values that have to be found by the xpath. It is used to ensure that the identified xpath does not underfit (does not find all target_values) or
#' overfit (find additional values that do not belong to the target_values)
#' @param doc responseString  of type html
#' @param root_path The root tag  where to stop looking for parent nodes. Default is "/*", which will be /html most often. For some
#' use cases it is of interest to find all sub nodes given a parent node.
#' @param get_other_cols When using sivis within the browser users can select a column of the table. The user might be interested to get the "neighbour"
#' columns as well. Its is not straightforward to add the functionality within the browser and let the user choose multiple columns, within doing
#' additional manual work of labeling each column. An alternative would be to let sivis identify candidates for additional columns and let the
#' user choose among these candidates within a small shiny app. get_other_cols is a boolean parameter (TRUE = search for other columns, FALSE = do not search
#' for other columns)
#' @param html_tag An html tag generated by \code{\link[rvest]{html_nodes}} or \code{\link[rvest]{html_node}}.
#' @param just_one_tag A boolean parameter. Required for a call from \code{\link{get_multicol_candidates}}, where a common base xpath is attempted to find
#' for multiple nodes. For details see: \code{\link{get_multicol_candidates}}.
#'
#' @return An xpath to identify the provided html tag given the html document.
#'
#' @examples
#' doc_html <- "<html><div><a>target_text</a></div></html>"
#' doc <- xml2::read_html(doc_html)
#' tag <- rvest::html_nodes(x = doc, xpath = "//*[contains(text(), 'target_text')]")
#' get_xpath_by_tag(tag = tag, doc = doc)


get_xpath_by_tag <- function(html_tag, attr = NULL, by_index = TRUE, all_text = NULL, doc, root_path = "/*", get_other_cols = TRUE, just_one_tag = FALSE){

  if(!length(html_tag)){

    warning("tag is of length 0, returning NULL for xpath.")
    return(list(xpath = NULL))

  }

  html_tag <- as.list(html_tag)
  if(rvest::html_name(html_tag) == "html"){
    stop("iter_tag is html root")
  }

  if(is.null(root_path)) root_path <- "/*"

  root_tags <- doc %>% rvest::html_nodes(xpath = root_path)
  tag_name <- ""
  xpath_elements <- ""
  tags_other_cols <- NULL
  iter_tag <- html_tag # in case of debugging nike needed
  xpath_found <- iter_tag %in% root_tags

  iter_nr <- 1
  while(!xpath_found){

    tag_name <- iter_tag %>% rvest::html_name()
    tag_name_insert <- tag_name

    if(by_index){

      children_all <- iter_tag %>%
        rvest::html_nodes(xpath = "..") %>%
        rvest::html_nodes(xpath = "child::*")

      children_match_tag <- children_all[rvest::html_name(children_all) == tag_name]
      children_match_text <- children_match_tag %>% rvest::html_text()

      # In case of multiple target strings: Dont set index if its html_text includes
      # other targetStrings. In current implementation this includes also
      # indexes of all parent tags, since their html_text also include
      # the other targetStrings.

      # refactor better grepl?

      # config parameter
      percentage_foun <- sapply(all_text, grep, x = children_match_text, fixed = TRUE) %>%
        lengths %>%
        {sum(.) / length(.)}

      has_all_children <- percentage_foun > 0.7

      # todo: where do i need - hasAllChildren <- TRUE??
      # counterexample: addcols for https://jobs.fcx.com/search/?searchby=location&createNewAlert=false&q=&locationsearch=&geolocation=
      if(is.null(all_text)){

        has_all_children <- FALSE

      }

      #  | notInAllTags
      # todo: do i really measure this correctly? Example: Blackrock
      # is a tradeoff - cant find all. but things might have changed since i removed
      # addcols calculation from here.
      tag_match <- sapply(all_text, FUN = grepl, x = children_match_text, fixed = TRUE)

      if(!length(tag_match %>% unlist)){

        injust_one_tag <- FALSE

      }else{

        injust_one_tag <- tag_match %>%
          as.data.frame %>%
          rowSums() %>%
          magrittr::equals(0) %>%
          {sum(.) == length(.) - 1}

      }

      need_index <- !has_all_children | iter_nr == 1 | all(unique(xpath_elements) %in% "") #| injust_one_tag

      if(just_one_tag){

        need_index <- !has_all_children | iter_nr == 1 | all(unique(xpath_elements) %in% "") | injust_one_tag

      }

      need_index
      matches <- c()
      has_target <- FALSE

      if(need_index){

        matches <- rep(0, length(children_match_tag))
        # dont need to set index if there is only one element with same tag type.
        # for consistency with chrome output.

        #if(length(iterTag) > 1) iterTag <- list(iterTag) # need to wrap in list so that i can find it children of its parents, see below.
        if(length(matches) > 1){

          for(nr in 1:length(children_match_tag)){

            matches[nr] <- identical(children_match_tag[nr], iter_tag)

          }

          has_target <- all_text %in% rvest::html_text(children_match_tag) %>% sum

          # Example: "httpsemploymentwellsfargocompscPSEAAPPLICANT_NWHRMScHRS_HRAM_FLHRS_CG_SEARCH_FLGBLPageHRS_APP_SCHJOB_.RData"
          # "Product Manager 2 - ETP, CEF and UIT" in "Job TitleProduct Manager 2 - ETP, CEF and UIT",
          # because children are div: JobTitle and span: Product Manager,....
          # but i need the indexing on that div level, so i have to accept this text merge.

          if(is.null(all_text)){

            has_target <- 1

          }else{

            has_target <- sapply(all_text, grepl, x = rvest::html_text(children_match_tag), fixed = TRUE) %>% sum

          }

          tag_name_insert <- glue::glue("{tag_name}[{which(matches == 1)}]")
          # now finding candidates for alternative columns that are displayed in the shiny overview.
          if(has_target) tag_nameAltern <- glue::glue("{tag_name}[{which(matches != 1)}]")

        }

      }

      iter_nr <- iter_nr + 1
      # config parameter
      if(iter_nr > 70) stop("Too many iterations (70+) in get_xpath_by_tag()")

    }

    # todo: refactor
    if(!length(iter_tag)){

      # need this if iterTag is empty: xml_nodeset of 0.
      xpath_found <- FALSE

    }else{

      # need this if it has two elements. need to better understand this.
      xpath_found <- iter_tag %in% root_tags %>% sum | rvest::html_name(iter_tag) == "html"

    }

    if(get_other_cols){

      if(length(matches) > 1 & has_target){

        tags_other_cols <- tag_nameAltern #lapply(tag_nameAltern, function(tag) c(tags, tag))

      }else{

        if(!is.null(tags_other_cols)){

          tags_other_cols <- lapply(tags_other_cols, function(tag) c(tag, tag_name_insert))

        }

      }

    }

    xpath_elements <- c(xpath_elements, tag_name_insert)
    iter_tag <- iter_tag %>% rvest::html_nodes(xpath = "..")
    # print(iterTag %>% html_text)

  }

  #todo: sometimes a tag "text" comes up, that i dont want. However, if i debug nike jobsite
  # tag script does not come up, but i want it.
  xpath_elements <- xpath_elements[magrittr::not(xpath_elements %in% c("text", ""))]
  if(root_path != "/*") xpath_elements <- xpath_elements[-length(xpath_elements)] # & length(xpElems) > 1
  xpath <- paste(c("", xpath_elements[length(xpath_elements):1]), collapse = "/")

  return(
    list(
      xpath = xpath
    )
  )
}

# # tags <- tagsOtherCols[[2]]
# xpathOtherCols <- NULL
# if(get_other_cols){
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
#       addtag_name <- lapply(children, function(child){
#         if(!length(child)) return("")
#         lapply(child, function(ch){
#           return(ifelse(length(ch), yes = ch %>% html_name %>% paste0("/", ., collapse = ""), no = ""))
#         })
#       })
#
#       # library(rlist)
#       # addtag_name <- rlist::list.flatten(addtag_name)
#
#       addtag_name <- lapply(addtag_name, function(tag_name){
#         tbl <- table(unlist(tag_name))
#         name <- names(tbl)[1]
#         for(name in names(tbl)){
#           idx <- which(name == tag_name)
#           if(nchar(name) & length(idx) > 1) tag_name[idx] <- glue("{name}[{1:length(idx)}]")
#         }
#         tag_name
#       })
#
#       nr <- 1
#       out <- list()
#       for(nr in 1:length(xpCand)){
#         out[[nr]] <- paste0(xpCand[nr], addtag_name[[nr]] %>% unlist)
#       }
#       xpCand <- out %>% unlist
#       hasTextChild <- lapply(children, function(child) child %>% html_text %>% nchar) %>% unlist %>% sum
#
#     }
#
#     # todo very dirty - remove first element, because it is already in commonXPath, but i need it as a start point
#     if(root_path != "/*"){
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
# # doc <- xml2::read_html("test.html")
# # tags <- c()
# # tag_nameInsert <- "a"
# lapply(
#   X = XPClasses,
#   FUN = checkXPath,
#   tag_nameInsert = tag_nameInsert,
#   tags = tags
# )


# if(!is.null(attr)){
#
#   NoClass <- checkXPath(
#     tag_nameInsert = tag_nameInsert,
#     tags = tags,
#     all_text = all_text
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
#     #checkXPath(tag_nameInsert, AmtElemBefore = NoClass$AmtElemFound, class = classes[1])
#     WithClasses <- lapply(
#       X = classes,
#       FUN = checkXPath,
#       tags = tags,
#       tag_nameInsert = tag_nameInsert,
#       AmtElemBefore = NoClass$AmtElemFound,
#       all_text = all_text
#     ) %>% do.call(what = rbind)
#     WithClasses
#     WithClasses %<>% dplyr::filter(nchar(class) > 0)
#     WithClasses
#     if(nrow(WithClasses)){
#       tag_nameInsert <- glue("{tag_name}[contains(@class, '{WithClasses$class}')]")
#     }
#   }
# }


#' Get the pathes of potential neighbour columns
#'
#'Helper function for \code{\link{get_multicol_candidates}} in which we find potential neighbour functions by:
#' \itemize{
#'   \item have the count for all target tags
#'   \item go up the tree (until html tag) and count amount of elements on each level.
#'   \item Highest level that has same amount of items than the amount of target tags is our starting point*
#'   \itemize{
#'      \item deriving the xpath of that node is our rootpath for all potential nieghbour columns
#'      \item all children of that starting point that dont have children themselves could contain text that qualify as potential neighbour columns**
#'   }
#' }

#'\strong{Exceptions:} \cr
#' \itemize{
#'   \item * a valid and useful starting point could be as well a parent node of length 1, since it will not contain additional unrelevant data.
#'   \item ** useful text of neighbour columns do not necessarily have to be on leaves that do not have children. Example: <div> Text <p></p></div>. \cr
#'}
#' This additional search will be performed within \code{\link{search_parent_nodes}}.
#'
#' @param doc html document
#' @param tags tags of the target text selected by the user in the browser
#'
#' @examples
#' data("doc_html")
#' doc <- xml2::read_html(html)
#' xpath <- "/html/body/table/tr/td[1]"
#' tags <- rvest::html_nodes(doc, xpath = xpath)
#' get_multicol_candidates(html, xpath, list())



get_leaf_pathes <- function(doc, tags){

  len <- length(tags)
  lenTags <- len
  out <- list()
  nr <- 1

  # intially set to true by lenTags == len
  has_valid_Parent <- lenTags == len & sum(tags %>% rvest::html_name() != "html")
  link_Path <- ""

  while(has_valid_Parent){

    out[[nr]] <- tags

    hasLink <- tags %>%
      rvest::html_name() %>%
      magrittr::equals("a") %>%
      sum

    if(hasLink){

      href <- tags %>% rvest::html_attr(name = "href")

      if(href %>% nchar %>% sum){

        link_Path <- get_xpath_by_tag(html_tag = tags[1], doc = doc)$xpath

      }

    }

    tags <- tags %>% rvest::html_nodes(xpath = "..")
    lenTags <- length(tags)
    nr <- nr + 1

    has_valid_Parent <- (tags %>% rvest::html_name() != "html") # let iter till html otherwise cant catch parent of link a with lenTags == len &
    # should actually not happen, because before we should hit the html tag
    if(nr > 70) stop("Too many iterations in get_leaf_pathes(). Stopped after iteration 70.")

  }

  ### speculative change could it be that i need nr-1 or nr-2: LINK is example for nr-1
  #### lets say i have lengths of 2,3, 72, 72, 72. I would want to have nr=3. He would start at 72 ->
  # better way -> just compare length of out

  idx <- (lengths(out) == len) %>%
    which %>%
    max %>%
    max(1) # to avoid integer(0) mis_match --> add max(1)

  # todo: how to avoid these double ifs. Arent there programming languages that
  # tried to use for allow 50, 50 , 1 catch one as it does not have other children and
  # catch link. Does not work.
  # if(idx < length(out)){
  #
  #   if(lengths(out)[idx + 1] == 1) idx <- idx + 1
  #


  start <- out[[idx]]
  # start <- out[[max(1, nr - 1)]] # use max, to avoid getting negative value for "nr".
  all_text <- start %>% rvest::html_text()

  start_Tag <- start[1]

  #### ASSUMPTION: since i need the xpath just as the upper bound root path, i can try to
  ### leave out the indexing
  root_path <- get_xpath_by_tag(
    html_tag = start_Tag,
    doc = doc,
    all_text = all_text,
    by_index = FALSE,
    get_other_cols = FALSE
  )$xpath

  # weird workaround? https://stackoverflow.com/questions/61064681/how-do-i-find-all-non-parent-nodes-in-xpath-without-converting-to-string
  #
  # leaves <- start %>% toString %>% xml2::read_html %>% html_nodes(xpath = "*//*[not(descendant::*)]")
  leaves <- start %>% rvest::html_nodes(xpath = ".//*[not(descendant::*)]")

  # if no results are found, but there is a parent which does not have other siblings,
  # parent can be checked without risking to get false positives.
  parent_node <- start %>% rvest::html_nodes(xpath = "..")
  empty_but_valid_parent <- !length(leaves) & length(parent_node) == 1

  if(empty_but_valid_parent){

    leaves <- parent_node %>% rvest::html_nodes(xpath = ".//*[not(descendant::*)]")

  }

  parent_Leaves <- search_parent_nodes(leaves, root_path, doc)

  leafPathes <- list()
  if(!length(leaves)){

    return(
      list(
        subPathes = NULL,
        root_path = NULL
      )
    )

  }

  for(nr in 1:length(leaves)){

    leafPathes[[nr]] <- get_xpath_by_tag(
      html_tag = leaves[nr],
      root_path = root_path,
      doc = doc,
      get_other_cols = FALSE
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
  #      group_List[[leaf_Nr]] <- get_xpath_by_tag(
  #       tag = group[leaf_Nr],
  #       all_text = group %>% html_text,
  #       root_path = root_path,
  #       doc = doc,
  #       get_other_cols = FALSE
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


  if(is.null(root_path)) root_path <- "/*"

  list(
    subPathes = subPathes,
    root_path = root_path,
    link_Path = link_Path
  )

}


#' Gsub for multiple values accounting for empty strings in string and pattern
#'
#' Wrapper for \code{\link[mgsub]{mgsub}} allowing for empty strings in pattern and string parameters
#'
#' @inheritParams mgsub::mgsub
#'
#' @return Converted srring
#' @examples
#' mgsub2("hey, ho",pattern = c("hey","ho"), replacement = c("ho","hey"))
#' mgsub2("", pattern = c("hey","ho"), replacement = c("ho","hey"))
#' mgsub2("hey, ho", pattern = "", replacement = c("ho","hey"))

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

#' Get the pathes of potential neighbour columns - Search in parent nodes
#'
#'Helper function for \code{\link{get_leaf_pathes}} in which we find potential neighbour functions by:
#' \itemize{
#'   \item have the count for all target tags
#'   \item go up the tree (until html tag) and count amount of elements on each level.
#'   \item Highest level that has same amount of items then amount of target tags is our starting point
#' }
#'Then we have to make an assumption: \cr
#'all children of that starting point that dont have children themselves could contain text that qualify as potential neighbour columns** \cr \cr
#'In this function we want to relax this assumption: \cr
#'** useful text of neighbour columns do not necessarily have to be on leaves that do not have children. Example: <div> Text <p></p></div>. \cr
#' This additional search will be performed within this function.
#' @inheritParams get_leaf_pathes

search_parent_nodes <- function(leaves, root_path, doc){

  leaves_Parent <- leaves %>% rvest::html_nodes(xpath = "..")
  leaves_GrantParent <- leaves_Parent %>% rvest::html_nodes(xpath = "..")

  leaves_Text <- leaves %>% rvest::html_text()
  leaves_Parent_Text <- leaves_Parent %>% rvest::html_text()

  # require performance optimization here - still 13 seconds
  # mgsub is more performant than gsub(pattern = targets %>% paste(collapse = "|"))
  covered_text <- leaves_Text %>%unique

  # todo: could place this when i actually start the (optional) shiny app - might not always be used.
  message("Start finding additional columns,... this could be time consuming.")
  leaves_GrantParent_Text <- leaves_GrantParent %>%
    rvest::html_text() %>%
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

      res[nr] <- get_xpath_by_tag(
        html_tag = new_data[nr],
        doc = doc,
        all_text = new_data %>% rvest::html_text(),
        root_path = root_path
      )

    }

  }


  return(

    # todo: get rid of first / as it is a subnode - could include that in get_xpath_by_tag
    res %>%
      unique %>%
      unlist %>%
      substring(first = 2)

  )

}




matching <- function(rvestOut, browser_output){
  percentage <- sum(rvestOut %in% browser_output$expectedOutput)/length(rvestOut)
  ident <- identical(x = rvestOut, y = browser_output$expectedOutput)
  c(percentage, ident)
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


IterableJsonName <- function(links, get_resultult){
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
  s <- subsetByArray(lst = get_resultult, arr = as.list(subset))
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
#lst_raw[[1]]$body$children$children[[1]]$listItems[[1]]$title$instances[[1]]$text
subsetByStr <- function(lst_raw, arr, target_values){
  nrr <- 1
  nr <- 1
  lst <- lst_raw
  base <- NULL
  base_follow = NULL
  # nr <- 2
  # nr <- 3
  # nr <- 4
  # nr <- 5
  if(length(arr$str)){
    for(nr in 1:length(unlist(arr$str))){
      if(is.null(lst)) stop("extraction in json is wrong have a null list.")

      # lst could be split in multiple elements. Therefore just for target_value check wrap lst in list(lst)
      percent_found <- sapply(target_values, grepl, x = list(lst), fixed = TRUE) %>% {sum(.) / length(.)}

      # config parameter
      is_single_target <- percent_found < 0.1
      #hasName <- !is.null(names(lst))

      if(is_single_target){
        subsetBy <- 1 #arr$iter
        if(nrr > 1) base <- arr$str[1:(nrr - 1)]
        base_follow <- arr$str[nrr:length(arr$str)]
        break
      }else{
        subsetBy <- arr$str[nrr]
        nrr <- nrr + 1
      }
      noName <- is.null(names(lst))
      if(noName) lst <- lst[[1]] # or only for nr == 1 and better filter for singletarget? ?? counterexample "httpspfizerwd1myworkdayjobscomPfizerCareersclientRequestIDe761010b8e564743ad14e7dca67f09e2.RData"
      lst <- lst[[subsetBy]]
      if(!is.null(base_follow) & nrr > 1) lst <- lst[[base_follow]]
    }
  }

  if(is.null(base) & nrr > 1) base <- arr$str
  list(
    base = base,
    base_follow = base_follow,
    lst = lst
  )
}

# [1] "body"      "children"  "children"  "listItems" "title"     "instances"

subsetByStr2 <- function(lst_raw, arr){

  lst <- lst_raw
  # nr <- 1

  for(nr in 1:length(arr)){

    if(is.null(names(lst))) lst <- lst[[1]]
    lst <-  lst[[arr[nr]]]
    nr <- nr + 1

  }

  lst

}

# arr = target_key

# https://flir.wd1.myworkdayjobs.com/flircareers/jobs
#https://mosaic.wd5.myworkdayjobs.com/mosaic
#[[49]]
# id  widget                                text action    v
# 1 20846215 moniker Production Supervisor - Granulation    GET TRUE
#
# [[50]]
# id  widget                              text action    v
# 1 20846224 moniker Production Supervisor - Phos Acid    GET TRUE

## --> try: sapply(lst_raw, "[", arr, USE.NAMES = FALSE) %>% unname %>% unlist


##### https://careers.invesco.com/ListJobs
##key: JobTitleRegex

# JobTitle      ShortTextField1
# 1 Specialist, RTR Finance & Accounting

#####lst_raw$JobTitle


# JobTitle                 Location
# 1       Apartment Maintenance Technician


subsetByStr3 <- function(lst_raw, arr){

  if(typeof(lst_raw) == "list" & !is.data.frame(lst_raw)){

    # refactor: really dirty
    if(is.list(lst_raw[[1]]) & !is.data.frame(lst_raw[[1]])){

      if(is.null(lst_raw[[1]][[arr]]) & !is.null(lst_raw[[1]][[1]][[arr]]) & length(lst_raw) == 1){

        lst_raw <- lst_raw[[1]]

      }

    }

    # as.data.frame for     fl <- "httpshiremyaviontecomsonarv2jobBoardQ16L3ooLDFQW1VE2wFUOsQ.RData"
    lst_raw <- do.call(rbind, lst_raw) %>% data.frame # for workadays: https://flir.wd1.myworkdayjobs.com/flircareers/jobs
  }

  # there can be multple matches like for https://aimcojobs.com/ListJobs?. But why do i do this anyway? why dont take regex
  # directly, because i cant subset that way,  but can i later?? or subset with regex?
  arrs <- sapply(arr, grep, x = names(lst_raw)) %>% unlist %>% names(lst_raw)[.]

  # transformation fails: https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25679&siteid=5313#keyWordSearch=&locationSearch=
  if(!length(arrs)){

    warning("transformation of target keys failed.")
    arr %<>% adist(names(lst_raw)) %>%
      which.min %>%
      names(lst_raw)[.]

  }else{

    arr <- arrs %>%
      adist(arr) %>%
      which.min %>%
      arrs[.] #unique

  }

  if(is.null(arr)){

    stop("Do not find correct target_keys for target_values in json.")

  }

  lst <- lst_raw
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


# target_value <- target_values[1]
# target_value <- "i Kontrolingu (M/F)"
matchStrings <- function(target_value, candidates){

  directMatch <- which(candidates == target_value)
  distances <- adist(x = candidates, y = target_value) %>% c
  realtive_dist <- (distances / min(nchar(target_value), nchar(candidates)) < 0.05) %>% which

  if(length(directMatch)){

    winner_idx <- directMatch

  }else if(length(realtive_dist)){

    warning(glue::glue("Only approximate match for target_value: '{target_value}'. For string length: {nchar(target_value)}."))
    winner_idx <- realtive_dist

  }else{

    winner_idx <- grep(
      pattern = target_value,
      x = candidates,
      fixed = TRUE
    )

  }

  #winner_idx <- match_idx[which.max(candidates[match_idx] %>% nchar)]
  # targetInCand <- which(lengths(sapply(candidates, grep, pattern = target_value, fixed = TRUE)) > 0)
  # candInTarget <- which(lengths(sapply(candidates, grep, x = target_value, fixed = TRUE)) > 0)
  # match_idx <- c(candInTarget, targetInCand)
  # if(!length(winner_idx)){
  #   distances <- adist(candidates, target_value)
  #   minDist <- min(distances, na.rm = TRUE)
  #
  #   winner_idx <- c("levenDist" = which.min(distances))
  # }
  # todo: check how to use. Currently hardly in use, because of approximate string match above.
  if(!length(winner_idx)){

    warning(glue::glue("no match for target_value: {target_value}"))
    return(NULL)

  }

  names(winner_idx) <- "directMatch"
  return(winner_idx)
}

# todo: make better for lufthansa: https://career.be-lufthansa.com/?
# have multiple target_keys - have to account for numbers and possibly also more values.

#https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25679&siteid=5313#keyWordSearch=&locationSearch=
# json_content[[1]]$Jobs$Job$Questions[[1]]$Value[12]

# base --> Jobs Job Questions
# INDEX
# target_key value
# index = 12

all_json_values <- function(json_content, target_values){

  json_content_flat <- unlist(json_content)

  # since i allow for json array now, this test is not needed anymore !?
  # hasNames <- json_content_flat %>%
  #   names %>%
  #   is.null
  # if(hasNames){
  #   stop("content can not be parsed - transformation to data.frame failed. Require json that can be parsed to data.frame as input")
  # }

  lst2 <- rlist::list.flatten(json_content)
  last_keys <- unlist(
    unname(
      Map(
        function(x, y) sub('.*\\.', '', rep(x = x, each = y)),
        names(lst2),
        sapply(lst2, length)
      )
    )
  )

  # last_keys <- names(json_content_flat) %>%
  #   strsplit(split = "[.]") %>%
  #   sapply(FUN = tail, n = 1)

  target_values <- target_values %>%
    gsub(pattern = "\n|\t", replacement = "") %>%
    trimws

  candidates <- json_content %>%
    unlist() %>%
    trimws

  match_idx <- sapply(target_values, matchStrings, candidates = candidates) %>%
    unlist %>%
    unname

  if(!length(match_idx)){
    stop(
      paste0("No match for target_values:'", paste(target_values, collapse = " | "),"' in json.")
    )
  }

  target_keys <- last_keys[match_idx %>% as.numeric()]
  target_keyCount <- target_keys %>% table

  # example for multiple_max on httpswwwroberthalfcomworkwithuscareersatroberthalfinternaljobsalljobsalllocationsalltypesroberthalfca.RData
  multiple_max <- (target_keyCount == max(target_keyCount)) %>%
    sum %>%
    magrittr::is_greater_than(1)

  if(multiple_max){

    # or go directly on regex target_key? but how to handle double target_keyslim below again?
    target_key <- gsub(x = target_keys, pattern = "[0-9]", replacement = "") %>%
      table %>%
      which.max %>%
      target_keys[.]

  }else{

    target_key <- target_keyCount %>%
      which.max %>%
      names

  }

  texts <- json_content_flat[last_keys %in% target_key] %>% unname

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
  # textsLength <- nchar(target_values) %>% sum


  # todo? what if there are multiple texts, does this check still work?
  is_large_html <- texts %>%
    find_doc_type(target_values = target_values) %$%
    type %>%
    magrittr::equals("text/html")


  if(is_large_html){

    return(
      list(
        is_large_html = TRUE,
        target_key = NULL,
        texts = texts,
        neighbours = NULL,
        base = target_key,
        base_follow = NULL
      )
    )

  }

  if(length(texts) < length(target_values)){

    target_keysSlim <- gsub(x = target_keys, pattern = "[0-9]*", replacement = "") %>%
      table %>%
      which.max %>%
      names

    numbers <- gsub(pattern = target_keysSlim, replacement = "", x = target_keys) %>%
      as.numeric

    seqNumber <- numbers %>%
      sort %>%
      diff %>%
      table %>%
      which.max %>%
      names %>%
      as.numeric

    if(!length(seqNumber)){

      target_key <- target_keys %>% unique

    }else{

      amt_slim_keys <- target_keysSlim %>% length

      if(amt_slim_keys == 1){

        target_key <- paste0(target_keysSlim, "[0-9]*$")

      }else{

        stop("Have too many target_keys while extracting target_values in JSON.")

      }

    }
    # if(seqNumber > 1){
    #   #todo: might add regex divisible by 3. ask #SO question about that
    #   # grep(pattern = "^([0369]|[147][0369]*[258]|(([258]|[147][0369]*[147])([0369]|[258][0369]*[147])*([147]|[258][0369]*[258])))+$", x = 3, fixed = TRUE)
    # }
  }

  # to decide: TradeOff here between direct match and fuzzy match.
  # example: https://pfizer.wd1.myworkdayjobs.com/PfizerCareers. hast key: text and text1, text2, text3, where the latter are not of interest.
  # idx <- which(target_key == last_keys)
  # last_keys[idx]
  idx <- grepl(pattern = target_key, x = last_keys) %>% which
  last_keys[idx]
  texts <- json_content_flat[idx] %>% unname

  # have to distinguish here between base and target_key. often it works to just take the first element.
  # but "httpsapismartrecruiterscomjobapipublicsearchwidgetsExpeditorspostingsoffset0limit700fqcallbackjQuery1.RData" is
  # a counter example, which has two base values.
  # instead i have to identify to check if the second value is a target_key. That will be the case if it repeats.

  # parentElemNmsRaw <- json_content_flat %>%
  #   names %>%
  #   .[match_idx[1]] %>%
  #   strsplit(split = "[.]") %>%
  #   unlist

  # todo: stupid switching between tibble/dplyr and characters
  base_candidates <- json_content_flat %>%
    names %>%
    .[match_idx] %>%
    strsplit(split = "[.]")

  mult_base_cand <- all(lengths(base_candidates) > 1)

  if(mult_base_cand){

    ## open issue: todo: target_key value index 12, merges to value12. And here i am just lucky,
    ## that there is one value8 in there. so too issues --value index12 merges too value12 and
    ## then value12 matches also length(unique(.)) == 12
    ## httpssjobsbrassringcomTgNewUISearchAjaxMatchedJobs.RData"
    match <- base_candidates %>%
      do.call(what = rbind) %>%
      data.frame %>%
      select_if(~length(unique(.)) == 1) %>%
      unique %>%
      as.character

  }else{

    if(base_candidates %>% unlist %>% unique %>% length %>% magrittr::is_greater_than(1)){

      match <- NULL

    }else{

      match <- base_candidates %>%
        unlist %>%
        .[1]

      # example: https://cboe.wd1.myworkdayjobs.com/External_Career_CBOE
      # todo: very dirty: collecting more examples
      # another example would be having title5 as tail(match, 1)
      # --> therefore need slim variant (without the numbers).
      # test for both. tes5t==tes5t and test5==test (want both to succeed)
      last_val <- tail(match, 1)
      last_val_slim <- gsub(
        x = last_val,
        pattern = "[0-9]",
        replacement = ""
      )

      if(tail(match, 1) == target_key | last_val_slim == target_key) match %<>% head(n = -1)

    }

  }


  # match <- parentElemNmsRaw %>% {ifelse(test = length(.) > 1, yes =  head(n = -1), no = .)} %>% c
  # roberts half: c("rh_job_search", "initial_results") fails if head(n = -1) is used
  # match <- parentElemNmsRaw #%>% head(n = -1) %>% c

  arr <- list(
    str = match,
    iter = match_idx[1]
  )

  lst_raw = json_content

  neighbours <- subsetByStr( # todo. rename
    lst_raw = lst_raw, # todo. rename
    arr = arr, # todo. rename
    target_values = target_values
  )

  # workaround
  nbr <- neighbours$lst
  if(!is.data.frame(nbr)) nbr <- nbr[[1]]

  list(
    target_key = target_key,
    texts = texts,
    is_large_html = is_large_html,
    neighbours = nbr,
    base = neighbours$base,
    base_follow = neighbours$base_follow
  )
}

##### e.g for httpsjobsapiinternalmcloudioapijobcallbackjobsCallbackoffset49sortfieldopen_datesortorderdescendingfa
# desired functionality for json with html:
# get_result <- GET(url = url)
# if(get_result$status_code != 200) return(NULL)
# contentGET <- content(get_result)
# res <- contentGET[[target_key]]
# #res %>% show_html_page

### does not work because of below:
##base_elems <- contentGET[[1]]
# raw <- sapply(base_elems, function(base_elem){
### --> have to find out why i need that and how to combine it with my other spec.
#contentGET[[base]][[INDEX]][[base_follow]][[target_key]]
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
#   base_follow <- lst[["base_follow"]]
#
#   target_keys <- lst$target_keys
#   if(!is.null(base_follow)) print(2)
#   rm(list = c("response", "base", "base_follow"))
# }


#, req_method = "GET", headers = NULL, body = NULL
unpack_json <- function(response, target_keys, base, base_follow = NULL){
  # if(is.null(base)){
  #   stop("Parameter base, provided to unpack_json(), is NULL - please provide a valid subset value.")
  # }
  # name <- sample(letters, size = 30, replace = TRUE) %>% {paste0(c("unpack_json_",., ".RData"), collapse = "")}
  # lst <- list(response = response, base = base, base_follow = base_follow, target_keys = target_keys)
  # save(file = name,list = "lst")

  response %<>% lapply(FUN = jsonlite::fromJSON)
  content_get_flat <- response %>% unlist

  split_names <- names(content_get_flat) %>% strsplit(split = "[.]")
  last_keys <- sapply(X = split_names, FUN = tail, n = 1)

  # todo;do i neeed two of these subsetbystr functions?
  # todo; refactor
  if(is.null(base)) base <- NA
  if(suppressWarnings(any(is.na(base)))){
    if(length(response) == 1){
      # if i index here already it cant subset later correctly: example: "httpscareersglobelifeinsurancecomjobsjobsbycategory.RData".
      base_elems <- response #[[1]]
    }else{
      base_elems <- response
    }
  }else{
    base_elems <- lapply(response, FUN = subsetByStr2, arr = base)
  }

  if(is.null(base_elems)) stop("got a empty list trying to extract values from json.")

  # config parameter
  # todo: better refactor here?
  is_large_html <- base_elems %>%
    stringr::str_count(pattern = "<") %>%
    is_greater_than(5) %>% any

  # example for is_large_html false alarm: https://www.cfindustries.com/careers/list.html
  if(is_large_html & base_elems %>% unlist %>% length %>%  magrittr::equals(1)) return(
    list(
      res = base_elems %>% unlist
    )
  )

  # could go for rlist::list.flatten but how to ensure then that i have the elements of same length?
  # base_elems2 <- rlist::list.flatten(base_elems) %>% list ###### TRY ME

  if(base_elems %>% unlist %>%  is.null) return(NULL)
  if(!length(base_elems)) return(NULL)


  # requirements: base_elems[[1]][[1]]$Value[8] for https://sjobs.brassring.com/TGnewUI/Search/Home/Home?partnerid=25678&siteid=5275#keyWordSearch=&locationSearch=
  target_key <- target_keys[1]
  texts <- lapply(target_keys, function(target_key){
    base_elem <- base_elems[1]
    raw <- sapply(base_elems, function(base_elem){
      if(!is.null(base_follow)){
        #### bei https://www.fbhs.com/careers  base_elem$base_follow bzw. base_elem[["base_follow"]]
        base_elem <- base_elem[[base_follow]] #subsetByStr3(lst_raw = base_elem, arr = base_follow)
      }
      subsetByStr3(lst_raw = base_elems, arr = target_key)
    }, USE.NAMES = FALSE)
    raw2 <- sapply(raw, paste, collapse = " | ") %>% unname
    if(!length(raw2)) return(raw2)
    df <- data.frame(raw2, stringsAsFactors = FALSE)
    df <- setNames(df, paste(target_key, collapse = "|"))
    df
  })

  ff <- rlist::list.flatten(texts)
  res <- do.call(what = cbind, texts)
  colnames(res) <- target_keys
  rownames(res) <- NULL

  add_links <- TRUE
  links <- sivis$browser_output$links

  if(add_links & nrow(res) == length(links)){

    res$href <- links

  }

  list(
    res = res,
    base = base, # what do i need these for?
    target_keys = target_keys # what do i need these for?
  )
}



#' Show the manual steps to activate the required addin in Chrome.
#'
#' The identification of the correct request will be performed in a chrome extension. A dataset is generated, that will be sufficient to
#' build a scraper from within R.
#' This dataset will be transformed into a JSON object and loaded in the clipboard from where it can be accessed in R. \cr \cr
#'
#' This function shows the steps to add the required addin to chrome.
#'
#' @export
#'
chrome_addin_tutorial <- function(){

  addin_dir <- file.path(find.package("mypkg2"), "extdata/sivisChrome/sivis-chrome-master")

  if(!dir.exists(addin_dir)){

    stop(glue::glue("Can not find chrome addin. Please file an issue listing the contents of: {file.path(find.package('mypkg2'), 'extdata/')}."))

  }

  message("Start Chrome and insert `chrome://extensions/` as the url.")
  message("If needed: Activate developer mode in the upper right hand corner.")
  message("In the upper left hand corner select: 'Load unpacked extensions'.")
  message(glue::glue("Select the following directory: {addin_dir} and confirm."))
  message("You should see the R logo in the upper right hand corner.")
  message("We are working on automating this process,...")

}

