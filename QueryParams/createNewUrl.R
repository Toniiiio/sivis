#
# 1. Disassemble url in baseUrl and query parameters
# 2. Identify pageChange and itemsize parameter
# 3. Build new url as a function with fixed itemsize and variable pagechange


# Example no pagechange needed
#https://www.essexapartmenthomes.com/careers/jobs

library(magrittr)
library(rlang)
library(dplyr)
library(purrr)
options(stringsAsFactors = FALSE)


decodeUrl <- function(url){
  urlSplit <- url %>%
    strsplit(split = "[?]") %>%
    unlist

  if(length(urlSplit) == 1){
    return(
      list(
        baseUrl = baseUrl,
        params = list()
      )
    )
  }

  baseUrl <- urlSplit[[1]]
  paramStrings <- urlSplit[[2]] %>%
    strsplit(split = "&") %>%
    lapply(FUN = strsplit, split = "=") %>%
    unlist(recursive = FALSE)


  key <- sapply(paramStrings, "[", 1)
  val <- sapply(paramStrings, "[", 2)
  params <- cbind(key, val = ifelse(is.na(val), "", val)) %>% data.frame
  params$type <- val %>%
    {suppressWarnings(as.numeric(.))} %>%
    {ifelse(test = is.na(.), yes = "character", no = "numeric")}

  subPages <- strsplit(
    x = url,
    split = "(?<!/)/(?!/)",
    perl = TRUE
  ) %>%
  unlist %>%
  {
    data.frame(
      val = .,
      type = ifelse(test = is.na(suppressWarnings(as.numeric(.))), yes = "character", no = "numeric")
    )
  }
  subPages


  list(
    url = url,
    baseUrl = baseUrl,
    params = params,
    subPages = subPages
  )
}



encodeUrl <- function(baseUrl, params){
  paramUrl <- apply(params[, 1:2], 1, paste, collapse = "=") %>%
    paste(collapse = "&")

  paste(c(baseUrl, paramUrl), collapse = "?")
}

#params <- decoded$params
encodeUrlFunc <- function(baseUrl, params){
  paramUrl <- apply(params[, 1:2], 1, paste, collapse = "=") %>%
    paste(collapse = "&")

  urlGen <- "function(nr){{paste0('{baseUrl}?{paramUrl}')}}" %>%
    glue %>%
    parse(text = .) %>%
    eval

  return(urlGen)
}



#getPageParameter(url)
#url <- "https://careers.key.com/en-US/search?pagenumber=2"
codeBefore <- sivis$reproduceForPageChange
codeAfter <- sivis$reproduceForPageChange
url <- sivis$url


getPageParameter_html_url <- function(url, codeBefore, codeAfter){
  pageChange <- NULL
  amtItems <- NULL

  decoded <- suppressWarnings(decodeUrl(url))
  hasNumericParams <- nrow(decoded$params %>% filter(type == "numeric"))

  if(!hasNumericParams){
    params <- decoded$subPages
    numericSubPages <- params %>% dplyr::filter(type == "numeric")

    tryCatch(eval(parse(text = codeBefore)), error = function(e) print(e))
    before <- response
    #before <- do.call(cbind, data)

    out <- list()
    rowNr <- 2
    for(rowNr in seq(nrow(numericSubPages))){
      # mutate_when fails to often

      params <- decoded$subPages
      oldVal <- params[params$val == numericSubPages[rowNr, ]$val, ]$val %>% as.numeric
      newVal <- oldVal %>% magrittr::multiply_by(2)

      params[params$val == oldVal, ]$val <- newVal

      url <- params$val %>% paste(collapse = "/")

      tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
      after <- response
      # after <- do.call(cbind, data)

      diffResults <- !identical(after, before) & length(after)
      lengthMatch <- dim(data.frame(after))[1] == newVal

      out[[rowNr]] <- c(diffResults, lengthMatch)
    }
    mat <- do.call(what = rbind, args = out)
    colnames(mat) <- c("diffContent", "lengthMatch")
    rownames(mat) <- numericSubPages$val
    mat

    amtItems <- which(apply(mat, 1, sum) == 2) %>% names
    pageChange <- which((mat[, 1] - mat[, 2]) == 1) %>% names

    return(
      list(
        subPages = decoded$subPages,
        amtItems = amtItems,
        pageChange = pageChange
      )
    )

  }else{

    params <- decoded$params
    filt <- params$key
    initVal <- params$val %>% as.numeric()
    newVal <- initVal*2

    #mutate_when always fails
    decoded$params[decoded$params$key == filt, ]$val <- newVal

    response <- ""
    before <- tryCatch(eval(parse(text = codeBefore)), error = function(e) NULL)
    print(url)
    url <- encodeUrl(decoded$baseUrl, decoded$params)
    print(url)
    after <- tryCatch(eval(parse(text = codeAfter)), error = function(e) NULL)

    # could also be just one column - should i first build the matrix and check then??
    amtItemsBefore <- sapply(before, length) %>%
      table %>%
      .[1] %>%
      names %>%
      as.numeric()

    amtItemsAfter <- sapply(after, length) %>%
      table %>%
      .[1] %>%
      names %>%
      as.numeric()

    amtItems <- ""
    pageChange <- ""
    if(amtItemsAfter == newVal){
      amtItems = decoded$params %>% dplyr::filter(key == filt)
    }
    print(amtItemsBefore)
    print(amtItemsAfter)
    print(identical(before, after))
    if(!identical(before, after)){  # amtItemsBefore == amtItemsAfter &
      pageChange = decoded$params %>% dplyr::filter(key == filt)
    }

    fixParams <- decoded$params %>% dplyr::filter(not(key %in% c(pageChange, amtItems)))

    return(
      list(
        pageChange = pageChange,
        amtItems = amtItems,
        baseUrl = decoded$baseUrl,
        fixParams = fixParams
      )
    )

  }

}

codeBefore <- sivis$reproduceForPageChange
codeAfter <- sivis$reproduceForPageChange
url <- sivis$url
getPageParameter_html_url(url, codeBefore, codeAfter)



url <- sivis$url
getPageParameter <- function(url, codeBefore, codeAfter){
  pageChange <- NULL
  amtItems <- NULL

  decoded <- suppressWarnings(decodeUrl(url))

  params <- decoded$params %>% dplyr::filter(type == "numeric")

  out <- list()
  xx <- 1

  response <- ""
  tryCatch(eval(parse(text = codeBefore)), error = function(e) print(e))
  before <- unlist(output)
  for(xx in 1:length(params$key)){
    filt <- params$key[xx]
    initVal <- params$val[xx]

    newValStr <- glue("', {initVal}*nr,'")
    newVal <- as.numeric(initVal)*2
    #mutate_when always fails
    parms <- decoded$params
    parms[parms$key == filt, ]$val <- newValStr

    print(url)
    nr <- 2
    response <- ""
    output <- list()
    scraper$urlGen <- encodeUrlFunc(baseUrl = decoded$baseUrl, params = parms)
    url <- scraper$urlGen(nr)
    print(url)
    tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
    after <- unlist(output)

    differentResults <- !identical(after, before)
    lengthMatch <- dim(data.frame(after))[1] == newVal

    out[[xx]] <- c(differentResults, lengthMatch)
  }
  mat <- do.call(what = rbind, args = out)
  colnames(mat) <- c("diffContent", "lengthMatch")
  rownames(mat) <- params$key
  mat

  itemsPerPage <- which(apply(mat, 1, sum) == 2) %>% names
  pageChange <- which((mat[, 1] - mat[, 2]) == 1) %>% names

  itemSizeSuccess <- ""
  if(length(itemsPerPage)){

    initVal <- decoded$params %>% dplyr::filter(key == itemsPerPage) %>% dplyr::select(val)
    itemSizes <- c(25, 50, 100, 200, 500, 1000)
    multiples <- itemSizes / as.numeric(initVal)

    itemSizeNr <- 1
    success <- logical(length(itemSizes))
    for(itemSizeNr in 1:length(itemSizes)){

      newVal <- as.numeric(initVal)*multiples[itemSizeNr]
      newValStr <- glue("', {newVal}*nr,'")
      nr <- 1
      parms <- decoded$params
      parms[parms$key == itemsPerPage, ]$val <- newValStr

      scraper$urlGen <- encodeUrlFunc(baseUrl = decoded$baseUrl, params = parms)
      url <- scraper$urlGen(nr)
      print(url)
      output <- list()
      response <- ""
      tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
      after <- unlist(output)
      lengthMatch <- dim(data.frame(after))[1] == newVal
      print(dim(data.frame(after))[1])
      print(lengthMatch)
      success[itemSizeNr] <- lengthMatch
    }
    itemSizeSuccess <- success %>% setNames(itemSizes)
    itemSize <- itemSizeSuccess %>% which %>% tail(1) %>% names

    parms <- decoded$params
    parms[parms$key == itemsPerPage, ]$val <- itemSize
  }

  if(length(pageChange)){
    initVal <- parms[parms$key == pageChange, ]$val
    parms[parms$key == pageChange, ]$val <- glue("', {initVal}*nr,'")
  }

  scraper$urlGen <- encodeUrlFunc(baseUrl = decoded$baseUrl, params = parms)

  return(
    list(
      pageChange = pageChange,
      itemsPerPage = itemsPerPage,
      baseUrl = decoded$baseUrl,
      itemSites = itemSizeSuccess,
      urlGen = scraper$urlGen
    )
  )
}

# url <- "https://careers.key.com/en-US/search?pagenumber=2"
# load("dynamicUrl/dynamicUrl_chipotle.RData")
codeBefore <- paste(sivis$xhrHeader, sivis$xhrRequest, collapse = "\n")
codeAfter <- sivis$xhrRequest
url <- sivis$url
done <- getPageParameter(url = url, codeBefore = codeBefore, codeAfter = codeAfter)
#save(sivis, file = "dynamicUrl/dynamicUrl_republic.RData")

urlGenWithSubPages <- function(splitForPage){
  subPages <- splitForPage$subPages

  initVal <- splitForPage$pageChange
  subPages[subPages$val == splitForPage$pageChange, ]$val <- glue('", {initVal}*nr,"')
  url <- subPages$val %>% paste(collapse = "/")

  str <- glue(
    "function(nr){{paste0(\"{url}\")}}"
  )
  str
  urlGen <- eval(parse(text = str))

  return(
    list(
      urlGen = urlGen
    )
  )
}

urlGenCode <- function(splitForPage){
  if(!nchar(splitForPage$pageChange) & !nchar(splitForPage$amtItems)) return(NULL)

  fixP <- splitForPage$fixParams

  fixParamUrl <- apply(fixP[, 1:2], 1, paste0, collapse = "=") %>%
    paste(collapse = "&")

  str <- paste0(
    "function(val){paste0(\"", splitForPage$baseUrl, "?", fixParamUrl, "&", splitForPage$pageChange$key, "=\", val)}"
  )

  urlGen <- eval(parse(text = str))

  return(
    list(
      urlGen = urlGen
    )
  )
}


###### HTML+URL html url

requestCode <- sivis$reproduceForPageChange
dynamicUrl <- function(url, requestCode){
  # 1. Disassemble url in baseUrl and query parameters
  # 2. Identify pageChange and itemsize parameter
  splitForPage <- suppressWarnings(getPageParameter_html_url(url, codeBefore = requestCode, codeAfter = requestCode))

  # 3. Build new url as a function with fixed itemsize and variable pagechange
  func <- urlGenCode(splitForPage)$urlGen
  initVal <- splitForPage$pageChange$val %>% as.numeric %>% magrittr::divide_by(2)

  list(
    func = func,
    initVal = initVal
  )
}

url <- sivis$cbData$request$request$url
requestCode <- sivis$reproduceForPageChange
dynamicUrl(url, requestCode)

#save(sivis, file = "dynamicUrl/dynamicUrl_ansys.RData")
#load("dynamicUrl/dynamicUrl_chipotle.RData")




url <- sivis$url
requestCode <- sivis$reproduceForPageChange
splitForPage <- suppressWarnings(getPageParameter_html_url(url, codeBefore = requestCode, codeAfter = requestCode))
func <- urlGenWithSubPages(splitForPage)$urlGen
func(2)




decodyBody <- function(body){

  bodyJSON <- sivis$headerBody %>% jsonlite::fromJSON()
  key <- bodyJSON %>% names
  val <- bodyJSON %>% as.character

  params <- cbind(key, val = ifelse(is.na(val), "", val)) %>% data.frame

  params$type <- val %>%
    as.numeric %>%
    {ifelse(test = is.na(.), yes = "character", no = "numeric")}
  params
}
itemSizeNr <- decodyBody(body)

encodeBody <- function(baseUrl, params){
  data.frame(itemSizeNr$val) %>% setNames(itemSizeNr$key) %>% toJSON
}

itemSizeNr[, 1:2] %>% toJSON
T


# > sivis$headerBody
# [1] "partnerId=25678&siteId=5275&keyword=&location=&keywordCustomSolrFields=AutoReq%2CFORMTEXT8%2CFORMTEXT7&locationCustomSolrFields=FORMTEXT2&facetfilterfields=&turnOffHttps=false&Latitude=0&Longitude=0&encryptedsessionvalue=%5EXlE1U_slp_rhc_4NP8rygPLgWYnRMDcP72sQG2OKZlb%2FKzYg_slp_rhc_Dw32OQlCRdWW63tnxlbBUaTHKkvIABE80xVjSfk%2FTp7SxAKL96vEaveyHZlZRJcAbk%3D"

bodyDecoded <- sivis$headerBody %>% fromJSON()
isInteger <- suppressWarnings(!is.na(sapply(x, as.integer)))

nrr <- 13
for(nrr in which(isInteger)){

  eval(parse(text = sivis$xhrHeader))
  eval(parse(text = sivis$xhrRequest))
  before <- output

  bodyDecoded[[nrr]] <-  as.numeric(bodyDecoded[[nrr]])*2
  scraper$body <- bodyDecoded %>% toJSON(auto_unbox = TRUE)
  eval(parse(text = sivis$xhrRequest))
  after <- output

  identical(before, after)
}



identical(xx, sivis$headerBody)


####### URL JSON
url <- "https://jobs.chipotle.com/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
url <- sivis$url
#decoded <- decodeUrl(url = sivis$url)

codeBefore <- paste(sivis$xhrHeader, sivis$xhrRequest, collapse = "\n")
codeAfter <- sivis$xhrRequest
splitForPage <- suppressWarnings(getPageParameter(url = url, codeBefore = codeBefore, codeAfter = codeAfter))
urlGenCode(splitForPage)

# decoded
# scraper$urlGen
# 'function(nr){
#   maxItems <- 100
#   paste0("https://jobs.chipotle.com/search-jobs/results?ActiveFacetID=0&CurrentPage=', nr,'&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf=")
# }'


encodeUrl(decoded$baseUrl, decoded$params)


##examples - open issues
### not in query parameter but before
#### paste0("https://mtb.wd5.myworkdayjobs.com/MTB/4/searchPagination/318c8bb6f553100021d223d9780d30be/150?clientRequestID=49140ba36a324f3a89a4dc0160fd54a7")





c("pr", "CurrentPage", "s", "offset", "start", "page", "pageNum", "pr", "pageNumber")
c("RecordPerPage", "from", "num_items", "items_per_page", "pageSize", "itemsPerPage", "currentnumberofjobsdisplayed")


"https://viacomcbs.careers/jobs/" # url
"https://viacomcbs.careers/jobs/#1" # sivis$browserOutput$pageUrl

url <- "https://de.ccep.jobs/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=14&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
url <- "https://jobs.boeing.com/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=1&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
url <- "https://uscareers-waters.icims.com/jobs/search?pr=1&schemaId=&o=&in_iframe=1"
url <- "https://careers.nrgenergy.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=25"

url <- "https://career.be-lufthansa.com/globaljobboard_api/search/?data=%7B%22LanguageCode%22%3A%22DE%22%2C%22SearchParameters%22%3A%7B%22FirstItem%22%3A1%2C%22CountItem%22%3A10000%2C%22Sort%22%3A%5B%7B%22Criterion%22%3A%22PublicationStartDate%22%2C%22Direction%22%3A%22DESC%22%7D%5D%2C%22MatchedObjectDescriptor%22%3A%5B%22ID%22%2C%22PositionTitle%22%2C%22PositionURI%22%2C%22PositionLocation.CountryName%22%2C%22PositionLocation.CityName%22%2C%22PositionLocation.Longitude%22%2C%22PositionLocation.Latitude%22%2C%22PositionLocation.PostalCode%22%2C%22PositionLocation.StreetName%22%2C%22PositionLocation.BuildingNumber%22%2C%22PositionLocation.Distance%22%2C%22JobCategory.Name%22%2C%22PublicationStartDate%22%2C%22ParentOrganizationName%22%2C%22LogoURI%22%2C%22OrganizationShortName%22%2C%22CareerLevel.Name%22%2C%22JobSector.Name%22%2C%22PositionIndustry.Name%22%2C%22PublicationCode%22%2C%22UserAreaEsaApprenticeship%22%2C%22UserAreaEsaApprenticeshipLocation%22%5D%7D%2C%22SearchCriteria%22%3A%5B%5D%7D"
url <- "https://api-deutschebank.beesite.de/search/?data={%22LanguageCode%22:%22DE%22,%22SearchParameters%22:{%22FirstItem%22:1,%22CountItem%22:10,%22Sort%22:[{%22Criterion%22:%22PublicationStartDate%22,%22Direction%22:%22DESC%22}],%22MatchedObjectDescriptor%22:[%22PositionID%22,%22PositionTitle%22,%22PositionURI%22,%22ScoreThreshold%22,%22OrganizationName%22,%22PositionFormattedDescription.Content%22,%22PositionLocation.CountryName%22,%22PositionLocation.CountrySubDivisionName%22,%22PositionLocation.CityName%22,%22PositionLocation.Longitude%22,%22PositionLocation.Latitude%22,%22PositionIndustry.Name%22,%22JobCategory.Name%22,%22CareerLevel.Name%22,%22PositionSchedule.Name%22,%22PositionOfferingType.Name%22,%22PublicationStartDate%22,%22UserArea.GradEduInstCountry%22,%22PositionImport%22,%22PositionHiringYear%22]},%22SearchCriteria%22:[]}"
url <- "https://careers.leidos.com/search/jobs/in?page=2"
url <- "https://careers-martinmarietta.icims.com/jobs/search?pr=1&schemaId=&o="


# fls <- list.files(path = "R/fromWeb/",pattern = "*.RData")
# lst <- list()
# params <- list()
# for(fl in fls){
#   load(file = paste0("R/fromWeb/", fl))
#   lst[[fl]] <- sivis$url
#   params[[fl]] <- decodeUrl(sivis$url)
# }
# urls <- lst %>% unlist %>% unname
# urls
# params
#
#
# baseUrl <- decoded$baseUrl
# pageKeys <- decoded$params$key %>%
#   {.[grepl(x = tolower(.), pattern = "page")]}
# if(!length(pageKeys)){
#   pageKeys <- decoded$params$key %>%
#     {.[grepl(x = tolower(.), pattern = "p")]}
# }
#
# perPage <- pageKeys %>% {.[grepl(x = tolower(.), pattern = "per")]}
# changePage <- pageKeys %>% {.[!grepl(x = tolower(.), pattern = "per")]}
# changePage <- pageKeys
#
# #dplyr::filter(key == perPage) %>%


# urlGenCode <- function(baseUrl, params){
#   params$valStore <- params$val
#   params %<>% mutate_when(type == "numeric", list(val = paste0("', ",key ,", '")))
#
#   variables <- params %>%
#     dplyr::filter(type == "numeric") %>%
#     dplyr::select(key, valStore)  %>%
#     apply(MARGIN = 1, FUN = paste, collapse = " = ") %>%
#     paste(collapse = "\n")
#
#   keyVal <- params[, 1:2]
#   paramUrl <- apply(keyVal, 1, paste, collapse = "=")
#
#   paramUrl %<>% paste(collapse = "&")
#
#   urlBuilder <- paste(c(baseUrl, paramUrl), collapse = "?") %>%
#     paste0("paste0('", ., "')")
#
#   return(
#     list(
#       variables = variables,
#       urlBuilder = urlBuilder
#     )
#   )
# }


mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (nr in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[nr]], envir = data)
    mutations <- eval(dots[[nr + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}

mutate_when2 <- function(data, condition, ...){
  condition <- enquo(condition)

  dots <- exprs(...)

  expressions <- map2( dots, syms(names(dots)), ~{
    quo( case_when(..condition.. ~ !!.x , TRUE ~ !!.y ) )
  })

  data %>%
    mutate( ..condition.. = !!condition ) %>%
    mutate( !!!expressions ) %>%
    select( -..condition..)
}

