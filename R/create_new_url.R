#todo:
# startwert für seitenzahl. Wenn Wert 15,30,45, etc. brauch ich den absoluten wert als startwert, e.g. 15
# wenn es seiten sind und ich auf seite 2 starte, dann wäre es 2*nr.
# könnte also prüfen ob es ein wert kleiner als 5 ist.

# todo:
# https://careers.twitter.com/en/jobs.html
# wrong limit and wrong addCols

### consolidte jon+url vs html + url
#json+url can have subpages too:
#https://recruiting2.ultipro.com/MON1009MECY/JobBoard/ff7fe76d-7fb9-45e2-8b70-5b8a922358a7/?q=&o=postedDateDesc

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

  baseUrl <- urlSplit[[1]]

  subPages <- strsplit(
    x = url,
    split = "(?<!/)/(?!/)", #split on / but not on //
    perl = TRUE
  ) %>%
    unlist %>%
    {
      data.frame(
        val = ., # deprecated is.na(suppressWarnings(as.numeric(.)))
        type = ifelse(test = grepl(pattern = "[0-9]+", x = .), yes = "hasNumeric", no = "character")
      )
    }

  if(length(urlSplit) == 1){

    return(
      list(
        baseUrl = baseUrl,
        subPages = subPages,
        params = list()
      )
    )

  }

  ## possible seperators for query parameter (pairs) are & ; etc.
  ## The resulting key value pairs are split by = ??
  ## So if a successful seperator is found, all key value pair candidates will have to have include =.

  querySeper = c("&", ";")

  # i cant go for lengths == 2 for all, because after = could be empty, so an empty val right?
  # if sep is unsuccesful it is one string which has a = in it. So testing for = alone is not enough.
  sep <- querySeper[2]

  for(sep in querySeper){

    keyValues <- urlSplit[[2]] %>%
      strsplit(split = sep)
    paramStrings <- keyValues %>%
      lapply(FUN = strsplit, split = "=") %>%
      unlist(recursive = FALSE)


    correctUnpack <- keyValues %>%
      unlist %>%
      sapply(FUN = function(str) stringr::str_count(string = str, pattern = "=") == 1) %>%
      all

    allKeyVal <- keyValues %>% unlist %>% grepl(pattern = "=") %>% all
    correctSep <- correctUnpack & allKeyVal
    if(correctSep) break

  }

  key <- sapply(paramStrings, "[", 1)
  val <- sapply(paramStrings, "[", 2)
  params <- cbind(key, val = ifelse(is.na(val), "", val)) %>% data.frame
  params$type <- val %>%
    {suppressWarnings(as.numeric(.))} %>%
    {ifelse(test = grepl(pattern = "[0-9]+", x = .), yes = "hasNumeric", no = "character")}

  list(
    url = url,
    baseUrl = baseUrl,
    params = params,
    subPages = subPages,
    querySeper = sep
  )
}
#decodeUrl(url)

encodeUrl <- function(baseUrl, params){

  paramUrl <- apply(params[, 1:2], 1, paste, collapse = "=") %>%
    gsub(pattern = "&", replacement = "", fixed = TRUE) %>% # for redundant & that appear during splitting
    paste(collapse = "&")

  paste(c(baseUrl, paramUrl), collapse = "?")
}

#params <- decoded$params
encodeUrlFunc <- function(baseUrl, params){

  paramUrl <- apply(params[, 1:2], 1, paste, collapse = "=") %>%
    gsub(pattern = "&", replacement = "", fixed = TRUE) %>% # for redundant & that appear during splitting
    paste(collapse = "&")

  urlGen <- "function(nr){{paste0('{baseUrl}?{paramUrl}')}}" %>%
    glue::glue %>%
    parse(text = .) %>%
    eval

  return(urlGen)

}




# codeBefore <- sivis$reproduceForPageChange
# codeAfter <- sivis$reproduceForPageChange
# url <- sivis$url
# get_page_parameter_html_url(url, codeBefore = codeBefore, codeAfter = codeAfter)

get_page_parameter_html_url <- function(url, codeBefore, codeAfter){

  pageChange <- NULL
  amtItems <- NULL

  decoded <- suppressWarnings(decodeUrl(url))

  hasNumericParams <- decoded$params %>%
    data.frame %>%
    filter(type == "hasNumeric") %>%
    dplyr::filter(val >= 0) %>%
    nrow

  noParamData <- nrow(decoded$subPages) < 2 & !hasNumericParams
  if(noParamData) return(NULL)

  hasNumericParams
  if(hasNumericParams){

    decodedUrl <- decodeQueryParams(decoded, codeBefore, codeAfter, url)

  }else{

    decodedUrl <- decodeSubPages(decoded, codeBefore, codeAfter, url)

  }

  return(decodedUrl)
}


decodeSubPages <- function(decoded, codeBefore, codeAfter, url){

  numericSubPages <- decoded$subPages %>%
    dplyr::filter(type == "hasNumeric") %>%
    dplyr::filter(val >= 0)

  response <- "" # can this be removed safely?
  hasResult <- TRUE
  tryCatch(
    expr = eval(parse(text = codeBefore)),
    error = function(e) print(e)
  )
  before <- response

  #before <- do.call(cbind, data)  # should have been replaced by response as variable name

  out <- list()
  rowNr <- 1
  nrows <- nrow(numericSubPages)

  if(nrows){

    decoded_SubPage <- decode_SubPages_numeric(nrows, decoded, codeAfter, before)

  }else{

    # look for subpages like url.com/.../Page-2
    warning("no numeric subPage also no character-numeric mixes")
    # candidate <- decoded$subPages$val %>% {.[grepl(x = ., pattern = "[0-9]")]}
    # update_Num_Str(candidate, oper = "*")
    decoded_SubPage <- NULL #decode_SubPages_char(nrows, decoded, codeAfter, before)

  }

  return(decoded_SubPage)

}

#key <- "Page-2"
mult_divide_by <- function(key, operator = "*", factor = 2){

  isNumeric <- suppressWarnings(
    key %>%
      as.numeric %>%
      is.na %>%
      magrittr::not()
  )

  if(isNumeric){

    newKey <- get(operator)(key, factor) %>% ceiling

  }else{

    newKey <- regexec('[0-9]+', key) %>%
      regmatches(x = key) %>%
      unlist %>%
      as.numeric %>%
      {gsub(pattern = ., replacement = get(operator)(., factor), x = key)}

  }

  newKey

}



# # procedure of numeric subpages
# 1. Loop through numeric Sub Pages and modify in object all subpages.
# 2. get old value and create new value
# 3. compare before and after
#
# For some cases the pageChange/itemSize is within a character, e.g. url.com/.../Page-2.
# Procedure would be the same except for the modification of the value. So that could be
# implemented in a more generic way.


decode_SubPages_numeric <- function(nrows, decoded, codeAfter, before){

  rowNr <- 1
  out <- list()
  subPages <- decoded$subPages
  numericSubPages <- subPages %>%
    dplyr::filter(type == "hasNumeric") %>%
    dplyr::filter(val >= 0)

  for(rowNr in seq(nrows)){

    # mutate_when fails to often
    oldVal <- subPages[subPages$val == numericSubPages[rowNr, ]$val, ]$val
    newVal <- oldVal %>% mult_divide_by(factor = 2)

    subPages[subPages$val == oldVal, ]$val <- newVal

    url <- subPages$val %>% paste(collapse = "/")

    response <- "" # can this be removed safely?
    hasResult <- TRUE
    tryCatch(
      expr = eval(parse(text = codeAfter)),
      error = function(e) print(e)
    )
    # after <- do.call(cbind, data) # should have been replaced by response as variable name
    after <- response


    diffResults <- !identical(after, before) & length(after)
    lengthMatch <- dim(data.frame(after))[1] == newVal

    out[[rowNr]] <- c(diffResults, lengthMatch)
  }

  mat <- do.call(what = rbind, args = out)
  colnames(mat) <- c("diffContent", "lengthMatch")
  rownames(mat) <- numericSubPages$val
  mat

  amtItems <- which(apply(mat, 1, sum) == 2) %>% names
  pageChange <- which((mat[, 1, drop = FALSE] - mat[, 2, drop = FALSE]) == 1) %>% {rownames(mat)[.]}

  return(
    list(
      subPages = decoded$subPages,
      amtItems = amtItems,
      pageChange = pageChange,
      urlPart = "subPages"
    )
  )

}


eval_paramChange <- function(before, initVal, currentKey, decoded, numericParams, codeAfter, operator = "*", amtItemsBefore){

  pageChange = ""
  amtItems = ""

  params <- decoded$params
  newVal <- get(operator)(initVal, 2)

  #mutate_when always fails
  params[params$key == currentKey, ]$val <- newVal
  url <- encodeUrl(decoded$baseUrl, params)
  print(url)

  # will i need this as a seperate function?
  # compareRequests <- function(before, url, codeAfter){
  output <- NULL
  response <- "" # can this be removed safely?
  nr <- 1
  tryCatch(eval(parse(text = codeAfter)), error = function(e) NULL)
  after <- response %>% unlist
  if(is.null(after) | !length(after)){
    #warning(glue("second request for analying numeric params is empty, see: {url}."))
    return(NULL)
  }

  amtItemsAfter <- sapply(after, length) %>%
    table %>%
    .[1] %>%
    names %>%
    as.numeric()
  # if its unpacked already in an array. Need a better generic handler here.
  if(typeof(after) == "character") amtItemsAfter <- length(after)
  amtItemsAfter

  amtItemMatch <- amtItemsAfter == newVal

  if(amtItemMatch){

    amtItems = decoded$params %>% dplyr::filter(key == currentKey)

  }else{

    # todo: redundant code: does another function make sense?
    itemSizeCandidate <- amtItemsBefore == initVal
    itemSizeCandidate
    # above it was tried to find the new value by multiplying by 2. But this does not work if
    # the itemSize parameter was already "maxed out". Therefore, if amount of results is equal
    # to the parameter value, it could still be itemSize parameter and that should be checked
    # by dividing by two.
    if(itemSizeCandidate){

      newVal <- initVal/2
      params[params$key == currentKey, ]$val <- newVal

      print(url)
      url <- encodeUrl(decoded$baseUrl, params)
      print(url)
      after <- tryCatch(eval(parse(text = codeAfter)), error = function(e) NULL)

      amtItemsAfter <- sapply(after, length) %>%
        table %>%
        .[1] %>%
        names %>%
        as.numeric()
      # if its unpacked already in an array. Need a better generic handler here.
      if(typeof(after) == "character") amtItemsAfter <- length(after)

      amtItemMatch <- amtItemsAfter == newVal

      if(amtItemMatch){

        amtItems = decoded$params %>% dplyr::filter(key == currentKey)

      }

    }

  }

  resultsChanged <- !identical(before, after)
  if(resultsChanged & !amtItemMatch){  # amtItemsBefore == amtItemsAfter &

    pageChange = decoded$params %>% dplyr::filter(key == currentKey)
    pageChange$startAt <- pageChange$val

  }

  list(
    pageChange = pageChange,
    amtItems = amtItems
  )
}

#url <- sivis$url
decodeQueryParams <- function(decoded, codeBefore, codeAfter, url){

  numericParams <- decoded$params %>%
    dplyr::filter(type == "hasNumeric") %>%
    dplyr::filter(val >= 0)

  params <- decoded$params
  response <- "" # can this be removed safely?
  amtItems <- ""
  pageChange <- ""

  tryCatch(eval(parse(text = codeBefore)), error = function(e) NULL)
  before <- response %>% unlist
  # could also be just one column - should i first build the matrix and check then??

  if(is.null(before)){

    cat(codeBefore)
    warning("'before' request is empty. Request might have failed. Check it above:")

  }

  amtItemsBefore <- sapply(before, length) %>%
    table %>%
    .[1] %>%
    names %>%
    as.numeric()

  if(typeof(before) == "character") amtItemsBefore <- length(before)
  amtItemsBefore

  rowNr <- 1
  for(rowNr in 1:nrow(numericParams)){

    initVal <- numericParams[rowNr, ]$val %>% as.numeric()
    currentKey  <- numericParams[rowNr, ]$key

    res <- eval_paramChange(before, initVal, currentKey, decoded, numericParams, codeAfter, operator = "/", amtItemsBefore)

    if(is.null(res)){

      res <- eval_paramChange(before, initVal, currentKey, decoded, numericParams, codeAfter, operator = "/", amtItemsBefore)

    }

    if(is.null(res)){

      next

    }


    if(nchar(res$pageChange)){

      pageChange <- res$pageChange

    }

    if(nchar(res$amtItems)){

      amtItems <- res$amtItems

    }

  }

  # next test for page 0.
  if(sum(nchar(pageChange))){

    params[params$key == currentKey, ]$val <- 0
    url <- encodeUrl(decoded$baseUrl, params)

    # url+json does not have direct return output. Need to go for variable: "output"
    output <- NULL
    tryCatch(eval(parse(text = codeAfter)), error = function(e) NULL)
    x0  <- output %>% unlist

    params[params$key == currentKey, ]$val <- 1
    url <- encodeUrl(decoded$baseUrl, params)
    tryCatch(eval(parse(text = codeAfter)), error = function(e) NULL)
    x1 <- output %>% unlist

    if(initVal > 5){

      startAt <- ifelse(test = length(x0) & !identical(x0, before), yes = 0, no = initVal)

    }else{ #initVal <= 5

      startAt <- ifelse(test = length(x1), yes = 1, no = initVal)
      startAt <- ifelse(test = identical(x1, x0), yes = initVal, no = 0)

    }

    pageChange$startAt <- startAt

  }

  fixParams <- decoded$params %>% dplyr::filter(not(key %in% c(pageChange, amtItems)))

  return(
    list(
      pageChange = pageChange,
      amtItems = amtItems,
      baseUrl = decoded$baseUrl,
      fixParams = fixParams,
      urlPart = "queryParams"
    )
  )
}



# codeBefore <- sivis$reproduceForPageChange
# codeAfter <- sivis$reproduceForPageChange
# url <- sivis$url
# get_page_parameter_html_url(url, codeBefore, codeAfter)



# url <- sivis$url
get_page_parameter <- function(url, codeBefore, codeAfter){

  pageChange <- NULL
  amtItems <- NULL

  decoded <- suppressWarnings(decodeUrl(url))

  if(length(decoded$params)){
    warning("decoded$params is empty. Could not find a page change parameter")
    return(
      list(urlGen = url)
    )
  }

  numericParams <- decoded$params %>%
    dplyr::filter(type == "hasNumeric") %>%
    dplyr::filter(val >= 0)

  out <- list()
  response <- "" # can this be removed safely?
  tryCatch(eval(parse(text = codeBefore)), error = function(e) print(e))
  before <- unlist(output)
  before
  paramNr <- 1

  for(paramNr in 1:length(numericParams$key)){
    print(paramNr)
    filt <- numericParams$key[paramNr]
    initVal <- numericParams$val[paramNr] %>% as.numeric

    newValStr <- glue::glue("', {initVal}*nr,'")
    newVal <- initVal*2
    #mutate_when always fails
    parms <- decoded$params
    parms[parms$key == filt, ]$val <- newValStr

    print(url)


    output <- list()
    response <- "" # can this be removed safely?
    hasResult <- TRUE
    nr <- 2
    scraper$urlGen <- encodeUrlFunc(baseUrl = decoded$baseUrl, params = parms)
    url <- scraper$urlGen(nr)
    print(url)
    tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
    after <- unlist(output)

    differentResults <- !identical(after, before) & !is.null(after)
    lengthMatch <- dim(data.frame(after))[1] == newVal

    out[[paramNr]] <- c(differentResults, lengthMatch)

  }

  mat <- do.call(what = rbind, args = out)
  colnames(mat) <- c("diffContent", "lengthMatch")
  rownames(mat) <- numericParams$key
  mat

  itemsPerPage <- which(apply(mat, 1, sum) == 2) %>% names
  pageChange <- which((mat[, 1] - mat[, 2]) == 1) %>% names

  itemSizeSuccess <- ""
  if(length(itemsPerPage)){

    initVal <- decoded$params %>%
      dplyr::filter(key == itemsPerPage) %>%
      dplyr::select(val)

    itemSizes <- c(0, 25, 50, 100, 200, 500, 1000)
    multiples <- itemSizes / as.numeric(initVal)

    itemSizeNr <- 1
    success <- logical(length(itemSizes))
    for(itemSizeNr in 1:length(itemSizes)){

      newVal <- as.numeric(initVal)*multiples[itemSizeNr]
      newValStr <- glue::glue("', {newVal}*nr,'")
      nr <- 1
      parms <- decoded$params
      parms[parms$key == itemsPerPage, ]$val <- newValStr

      scraper$urlGen <- encodeUrlFunc(
        baseUrl = decoded$baseUrl,
        params = parms
      )

      url <- scraper$urlGen(nr)
      print(url)
      output <- list()
      response <- "" # can this be removed safely?
      hasResult <- TRUE
      tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
      after <- unlist(output)
      lengthMatch <- dim(data.frame(after))[1] == newVal
      print(dim(data.frame(after))[1])
      print(lengthMatch)
      success[itemSizeNr] <- lengthMatch

    }

    itemSizeSuccess <- success %>% setNames(itemSizes)
    itemSize <- itemSizeSuccess %>%
      which %>%
      tail(1) %>%
      names
  }

  # todo: put both if with the one above
  if(length(itemsPerPage)){

    parms[parms$key == itemsPerPage, ]$val <- itemSize

  }

  # need fresh decoded$params otherwise they overwrite each other.
  # could refactor.
  if(length(pageChange)){

    scraper <- testPage0(
      numericParams = numericParams,
      decoded = decoded,
      pageChange = pageChange,
      scraper = scraper,
      codeAfter = codeAfter,
      before = before
    )

  }

  return(
    list(
      pageChange = pageChange,
      itemsPerPage = itemsPerPage,
      baseUrl = decoded$baseUrl,
      itemSizes = itemSizeSuccess,
      urlGen = scraper$urlGen
    )
  )

}

testPage0 <- function(numericParams, decoded, pageChange, scraper, codeAfter, before){

  parms <- decoded$params
  initVal <- parms[parms$key == pageChange, ]$val
  parms[parms$key == pageChange, ]$val <- glue::glue("', {initVal}*nr,'")

  stepSize <- numericParams[numericParams$key == pageChange, ]$val
  parms[parms$key == pageChange, ]$val <- glue::glue("', {stepSize}*(nr - 1),'")
  saveUrl <- scraper$urlGen
  scraper$urlGen <- encodeUrlFunc(
    baseUrl = decoded$baseUrl,
    params = parms
  )

  # have to set hasResult + output otherwise the while loop
  # in codeAfter will not start
  hasResult <- TRUE
  response <- "" # can this be removed safely?
  output <- list()
  tryCatch(eval(parse(text = codeAfter)), error = function(e) print(e))
  x0 <- unlist(output)
  hasZeroRes <- length(x0) & !identical(before, x0)
  if(!hasZeroRes) scraper$urlGen <- saveUrl

  return(scraper)
}


# url <- "https://careers.key.com/en-US/search?pagenumber=2"
# load("dynamic_url/dynamic_url_chipotle.RData")
# codeBefore <- paste(sivis$xhrHeader, sivis$xhrRequest, collapse = "\n")
# codeAfter <- sivis$xhrRequest
# url <- sivis$url
# done <- get_page_parameter(url = url, codeBefore = codeBefore, codeAfter = codeAfter)
# save(sivis, file = "dynamic_url/dynamic_url_dow.RData")




######## SPEC:
# 2      --->  '", {initVal}*nr,"'
# Page-2 --->  '", {initVal}*nr,"'

# best way to do it is by replacing the numbers with {nr} and use glue().
# replace two digit numbers with [0-9]+, to avoid {nr}{nr} as a replacement

urlGenWithSubPages <- function(splitForPage){

  subPages <- splitForPage$subPages

  initVal <- splitForPage$pageChange
  newVal <- initVal %>% gsub(pattern = "[0-9]+", replacement = "{nr}")
  subPages[subPages$val == splitForPage$pageChange, ]$val <- newVal
  url <- subPages$val %>% paste(collapse = "/")

  str <- glue::glue(
    "function(nr){{glue(\"{url}\")}}"
  )
  str
  urlGen <- eval(parse(text = str))

  return(
    list(
      urlGen = urlGen
    )
  )

}

urlGenCode <- function(splitForPage, requestCode){

  has_Iterable_Params <- nchar(splitForPage$pageChange) | nchar(splitForPage$amtItems)

  fixParamUrl <- apply(splitForPage$fixParams[, 1:2], 1, paste0, collapse = "=") %>%
    gsub(pattern = "&", replacement = "", fixed = TRUE) %>% # for redundant & that appear during splitting
    paste(collapse = "&")

  if(!has_Iterable_Params){

    urlGen <- glue::glue('function(nr){{paste0("{splitForPage$baseUrl}?{fixParamUrl}")}}') %>%
      parse(text = .) %>%
      eval

    return(urlGen)

  }


  pageChangeVal <- splitForPage$pageChange$val %>% as.numeric
  # could also start at zero: E.g. url?...&startrow=25, also works for url?...&startrow=0.
  multiplier <- ifelse(
    test = pageChangeVal > 1 &  splitForPage$pageChange$startAt == 0,
    yes = "(nr - 1)",
    no = "nr"
  )

  # if stepsize is 1, but start at 0. Did this ever occur?
  multiplier <- ifelse(
    test = pageChangeVal == 1 &  splitForPage$pageChange$startAt == 0,
    yes = "nr - 1",
    no = multiplier
  )

  # user should start on page2, could start on 3. But pagechange could also be of type: url?...&startrow=25.
  # dirty baseline rule, smaller 5 set to 1, else keep e.g. 25
  pageChangeVal <- ifelse(test = pageChangeVal < 5, yes = 1, no = pageChangeVal)
  pageChangeVal <- ifelse(test = pageChangeVal == 1, yes = "", no = glue::glue("*{pageChangeVal}"))

  print(splitForPage$pageChange$key)
  splitForPage$pageChange$key %<>% gsub(pattern = "&", replacement = "", fixed = TRUE)
  print(splitForPage$pageChange$key)

  urlGen <- paste0(
    "function(nr){paste0(\"", splitForPage$baseUrl, "?", fixParamUrl, "&", splitForPage$pageChange$key, "=\", ", multiplier, pageChangeVal,")}"
  ) %>%
    parse(text = .) %>%
    eval

  hasItemSize <- nchar(splitForPage$amtItems) %>% sum

  if(hasItemSize){

    amtItemsVal <- splitForPage$amtItems$val %>% as.numeric

    amtItemsStr <- splitForPage$amtItems[1:2] %>%
      paste(collapse = "=") %>%
      paste0("&", .)

    str <- glue::glue('function(nr){{paste0("{splitForPage$baseUrl}?{fixParamUrl}&{splitForPage$pageChange$key}=", nr{pageChangeVal}, "{amtItemsStr}")}}')

    # search highest amtItem parameter
    itemSizes <- c(0, 25, 50, 100, 200, 500, 1000)
    multiples <- itemSizes / as.numeric(amtItemsVal)

    itemSizeNr <- 1
    success <- logical(length(itemSizes))

    for(itemSizeNr in 1:length(itemSizes)){

      newVal <- as.numeric(amtItemsVal)*multiples[itemSizeNr]
      amtItemsStr <- glue::glue("&ps={newVal}")
      # if has changepage parameter, than analyse from first page, because page2 might not have enough elements for high values like 500
      func <- glue::glue('function(nr){{paste0("{splitForPage$baseUrl}?{fixParamUrl}&{splitForPage$pageChange$key}=", 1, "{amtItemsStr}")}}') %>%
        parse(text = .) %>%
        eval

      url <- func(nr)
      print(url)
      output <- list()
      response <- "" # can this be removed safely?
      hasResult <- TRUE
      tryCatch(eval(parse(text = requestCode)), error = function(e) print(e))
      after <- unlist(response)

      lengthMatch <- dim(data.frame(after))[1] >= itemSizes[max(1, itemSizeNr - 1)] # could be replaced by check if result is as good as last one, see open issue in docu
      print(dim(data.frame(after))[1])
      print(lengthMatch)
      success[itemSizeNr] <- lengthMatch

    }

    itemSizeSuccess <- success %>% setNames(itemSizes)
    itemSize <- itemSizeSuccess %>%
      which %>%
      tail(1) %>%
      names

    amtItemsStr <- glue::glue("&{splitForPage$amtItems$key}={itemSize}")
    urlGen <- glue::glue('function(nr){{paste0("{splitForPage$baseUrl}?{fixParamUrl}&{splitForPage$pageChange$key}=", nr{pageChangeVal}, "{amtItemsStr}")}}') %>%
      parse(text = .) %>%
      eval

  }

  return(
    urlGen
  )
  #   list(
  #     urlGen = urlGen
  #   )
  # )

}


###### HTML+URL html url url+html

# url <- sivis$url
# requestCode <- sivis$reproduceForPageChange
dynamic_url <- function(url, requestCode){

  # 1. Disassemble url in baseUrl and query parameters
  # 2. Identify pageChange and itemsize parameter
  splitForPage <- suppressWarnings(
    get_page_parameter_html_url(
      url = url,
      codeBefore = requestCode,
      codeAfter = requestCode
    )
  )
  splitForPage
  if(is.null(splitForPage)) return(NULL)

  if(splitForPage$urlPart == "subPages"){

    urlGen <- urlGenWithSubPages(splitForPage)$urlGen

    return(
      list(
        func = urlGen
      )
    )

  }else{ #splitForPage$urlPart == "queryParams"

    # 3. Build new url as a function with fixed itemsize and variable pagechange
    func <- urlGenCode(splitForPage, requestCode) #$urlGen
    #initVal <- splitForPage$pageChange$val %>% as.numeric %>% magrittr::divide_by(2) # can this be removed safely?

    return(
      list(
        func = func #,
        #initVal = initVal
      )
    )

  }

}

# url <- sivis$cbData$request$request$url
# url <- sivis$url
# requestCode <- sivis$reproduceForPageChange
# xx <- dynamic_url(url, requestCode)
# xx$func(10)
#save(sivis, file = "dynamic_url/dynamic_url_dteenergy.RData")
#load("dynamic_url/dynamic_url_chipotle.RData")




# url <- sivis$url
# requestCode <- sivis$reproduceForPageChange
# splitForPage <- suppressWarnings(get_page_parameter_html_url(url, codeBefore = requestCode, codeAfter = requestCode))
# func <- urlGenWithSubPages(splitForPage)$urlGen
# func(2)




decodyBody <- function(body){

  bodyJSON <- sivis$headerBody %>% jsonlite::fromJSON()
  key <- bodyJSON %>% names
  val <- bodyJSON %>% as.character

  params <- cbind(key, val = ifelse(is.na(val), "", val)) %>% data.frame

  params$type <- val %>%
    as.numeric %>%
    {ifelse(test = grepl(pattern = "[0-9]+", x = .), yes = "hasNumeric", no = "character")}
  params
}
# itemSizeNr <- decodyBody(body)

encodeBody <- function(baseUrl, params){
  data.frame(itemSizeNr$val) %>% setNames(itemSizeNr$key) %>% toJSON
}



# > sivis$headerBody
# [1] "partnerId=25678&siteId=5275&keyword=&location=&keywordCustomSolrFields=AutoReq%2CFORMTEXT8%2CFORMTEXT7&locationCustomSolrFields=FORMTEXT2&facetfilterfields=&turnOffHttps=false&Latitude=0&Longitude=0&encryptedsessionvalue=%5EXlE1U_slp_rhc_4NP8rygPLgWYnRMDcP72sQG2OKZlb%2FKzYg_slp_rhc_Dw32OQlCRdWW63tnxlbBUaTHKkvIABE80xVjSfk%2FTp7SxAKL96vEaveyHZlZRJcAbk%3D"

# bodyDecoded <- sivis$headerBody %>% fromJSON()
# isInteger <- suppressWarnings(!is.na(sapply(x, as.integer)))

# nrr <- 13
# for(nrr in which(isInteger)){
#
#   eval(parse(text = sivis$xhrHeader))
#   eval(parse(text = sivis$xhrRequest))
#   before <- output
#
#   bodyDecoded[[nrr]] <-  as.numeric(bodyDecoded[[nrr]])*2
#   scraper$body <- bodyDecoded %>% toJSON(auto_unbox = TRUE)
#   eval(parse(text = sivis$xhrRequest))
#   after <- output
#
#   identical(before, after)
# }


####### URL+JSON+URL
# url <- "https://jobs.chipotle.com/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
# url <- "https://corporate.dow.com/.corporate-search.servlet.json/?x1=ContentType;q1=Job;x7=JobEndDateEpoch;sp_q_min_7=1584384051;page=2;sp_s=StartDate"

# url = sivis$url
# headerCode = sivis$xhrHeader(urlFunc = glue("function(nr){{'{url}'}}"))
# requestCode = sivis$xhrRequest

get_url_json_page_change <- function(url, headerCode, requestCode){

  decoded <- decodeUrl(url = url)
  codeBefore <- paste(headerCode, requestCode, collapse = "\n")
  codeAfter <- requestCode
  splitForPage <- suppressWarnings(
    get_page_parameter(
      url = url,
      codeBefore = codeBefore,
      codeAfter = codeAfter
    )
  )

  return(splitForPage$urlGen)
}


# decoded
# scraper$urlGen
# 'function(nr){
#   maxItems <- 100
#   paste0("https://jobs.chipotle.com/search-jobs/results?ActiveFacetID=0&CurrentPage=', nr,'&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf=")
# }'


# encodeUrl(decoded$baseUrl, decoded$params)


##examples - open issues
### not in query parameter but before
#### paste0("https://mtb.wd5.myworkdayjobs.com/MTB/4/searchPagination/318c8bb6f553100021d223d9780d30be/150?clientRequestID=49140ba36a324f3a89a4dc0160fd54a7")





c("pr", "CurrentPage", "s", "offset", "start", "page", "pageNum", "pr", "pageNumber")
c("RecordPerPage", "from", "num_items", "items_per_page", "pageSize", "itemsPerPage", "currentnumberofjobsdisplayed")


"https://viacomcbs.careers/jobs/" # url
"https://viacomcbs.careers/jobs/#1" # sivis$browserOutput$pageUrl

# url <- "https://de.ccep.jobs/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=14&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=0&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
# url <- "https://jobs.boeing.com/search-jobs/results?ActiveFacetID=0&CurrentPage=2&RecordsPerPage=15&Distance=50&RadiusUnitType=0&Keywords=&Location=&Latitude=&Longitude=&ShowRadius=False&CustomFacetName=&FacetTerm=&FacetType=0&SearchResultsModuleName=Search+Results&SearchFiltersModuleName=Search+Filters&SortCriteria=0&SortDirection=1&SearchType=5&CategoryFacetTerm=&CategoryFacetType=&LocationFacetTerm=&LocationFacetType=&KeywordType=&LocationType=&LocationPath=&OrganizationIds=&PostalCode=&fc=&fl=&fcf=&afc=&afl=&afcf="
# url <- "https://uscareers-waters.icims.com/jobs/search?pr=1&schemaId=&o=&in_iframe=1"
# url <- "https://careers.nrgenergy.com/search/?q=&sortColumn=referencedate&sortDirection=desc&startrow=25"
#
# url <- "https://career.be-lufthansa.com/globaljobboard_api/search/?data=%7B%22LanguageCode%22%3A%22DE%22%2C%22SearchParameters%22%3A%7B%22FirstItem%22%3A1%2C%22CountItem%22%3A10000%2C%22Sort%22%3A%5B%7B%22Criterion%22%3A%22PublicationStartDate%22%2C%22Direction%22%3A%22DESC%22%7D%5D%2C%22MatchedObjectDescriptor%22%3A%5B%22ID%22%2C%22PositionTitle%22%2C%22PositionURI%22%2C%22PositionLocation.CountryName%22%2C%22PositionLocation.CityName%22%2C%22PositionLocation.Longitude%22%2C%22PositionLocation.Latitude%22%2C%22PositionLocation.PostalCode%22%2C%22PositionLocation.StreetName%22%2C%22PositionLocation.BuildingNumber%22%2C%22PositionLocation.Distance%22%2C%22JobCategory.Name%22%2C%22PublicationStartDate%22%2C%22ParentOrganizationName%22%2C%22LogoURI%22%2C%22OrganizationShortName%22%2C%22CareerLevel.Name%22%2C%22JobSector.Name%22%2C%22PositionIndustry.Name%22%2C%22PublicationCode%22%2C%22UserAreaEsaApprenticeship%22%2C%22UserAreaEsaApprenticeshipLocation%22%5D%7D%2C%22SearchCriteria%22%3A%5B%5D%7D"
# url <- "https://api-deutschebank.beesite.de/search/?data={%22LanguageCode%22:%22DE%22,%22SearchParameters%22:{%22FirstItem%22:1,%22CountItem%22:10,%22Sort%22:[{%22Criterion%22:%22PublicationStartDate%22,%22Direction%22:%22DESC%22}],%22MatchedObjectDescriptor%22:[%22PositionID%22,%22PositionTitle%22,%22PositionURI%22,%22ScoreThreshold%22,%22OrganizationName%22,%22PositionFormattedDescription.Content%22,%22PositionLocation.CountryName%22,%22PositionLocation.CountrySubDivisionName%22,%22PositionLocation.CityName%22,%22PositionLocation.Longitude%22,%22PositionLocation.Latitude%22,%22PositionIndustry.Name%22,%22JobCategory.Name%22,%22CareerLevel.Name%22,%22PositionSchedule.Name%22,%22PositionOfferingType.Name%22,%22PublicationStartDate%22,%22UserArea.GradEduInstCountry%22,%22PositionImport%22,%22PositionHiringYear%22]},%22SearchCriteria%22:[]}"
# url <- "https://careers.leidos.com/search/jobs/in?page=2"
# url <- "https://careers-martinmarietta.icims.com/jobs/search?pr=1&schemaId=&o="


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

