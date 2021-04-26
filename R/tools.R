#-------------------------------------------------------------------------------
# Program: tools.R
# Objective: functions to facilitate manage data from http call
# Author: A. Charleroy
# Creation: 03/09/2019
# Update: 07/10/2019 by A. Charleroy
#-------------------------------------------------------------------------------

##' @title getDataAndShowStatus
##'
##' @description Retreive httr response status and data linked to it 
##' @param responseObject objet de reponse HTTP httr
##' @keywords internal
getDataAndShowStatus <- function(responseObject) {
  status = NULL
  json = jsonlite::fromJSON(
    httr::content(
      responseObject,
      as = "text",
      encoding = "UTF-8")
    )
  msg <- showStatus(responseObject)
  if (responseObject$status_code >= 400) {
    if (!is.null(json$metadata$status) &&
        length(json$metadata$status) > 0) {
      status = json$metadata$status
      logging::loginfo("Additional Request information :")
      logging::loginfo(status)
    }
    
    response <- list(
      currentPage = NULL,
      totalCount = NULL,
      totalPages = NULL,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = NULL
    )
  } else {
    if (!is.null(json$metadata$status) &&
        length(json$metadata$status) > 0) {
      logging::loginfo("Additional Request information :")
      if (logging::getLogger()$level >  get("DEBUG_LEVEL", configWS)[["NOTSET"]] &&
          logging::getLogger()$level <=  get("DEBUG_LEVEL", configWS)[["INFO"]]) {
          logging::loginfo(json$metadata$status)
      }
      
      status = json$metadata$status
    }
  
    logging::loginfo(msg)
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = c(json$result, json$metadata$datafiles)
    )
  }
  class(response) <- append(class(response), "WSResponse")
  return(response)
}

#' @title getDataAndMetadataFromResponse
##'
##' @description Retreive httr response status and data linked to it 
##' @param responseObject objet de reponse HTTP httr
##' @export
getDataAndMetadataFromResponse <- function(responseObject) {
  status = NULL
  json = jsonlite::fromJSON(
    httr::content(
      responseObject,
      as = "text",
      encoding = "UTF-8")
  )
  msg <- showStatus(responseObject)
  if (responseObject$status_code >= 400) {
    if (!is.null(json$metadata$status) &&
        length(json$metadata$status) > 0) {
      status = json$metadata$status
      logging::loginfo("Additional Request information :")
      logging::loginfo(status)
    }
    
    response <- list(
      currentPage = NULL,
      totalCount = NULL,
      totalPages = NULL,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = NULL
    )
  } else {
    if (!is.null(json$metadata$status) &&
        length(json$metadata$status) > 0) {
      logging::loginfo("Additional Request information :")
      if (logging::getLogger()$level >  get("DEBUG_LEVEL", configWS)[["NOTSET"]] &&
          logging::getLogger()$level <=  get("DEBUG_LEVEL", configWS)[["INFO"]]) {
        logging::loginfo(json$metadata$status)
      }
      
      status = json$metadata$status
    }
    
    logging::loginfo(msg)
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = c(json$result, json$metadata$datafiles)
    )
  }
  class(response) <- append(class(response), "WSResponse")
  return(response)
}

##' @title showStatus
##'
##' @description Show status
##' @param responseObject objet de reponse HTTP httr
##' @return character, message describes the response 
##' @keywords internal
showStatus <- function(responseObject) {
  msg <- ""
  if (responseObject$status_code >= 400) {
    if (responseObject$status_code >= 500) {
      msg = "WebService internal error"
      logging::logerror(msg)
    }
    if (responseObject$status_code == 401) {
      msg = "User not authorized"
      logging::logerror(msg)
    }
    if (responseObject$status_code == 404) {
      msg = "Data not found"
      logging::logwarn(msg)
    }
    if (responseObject$status_code != 401 &&
        responseObject$status_code != 404 &&
        responseObject$status_code < 500) {
      msg = "Bad user request"
      logging::logerror(msg)
    }
  }
  if (responseObject$status_code >= 200 &&
      responseObject$status_code < 300) {
    msg = "Query executed and data recovered"
  }
  return(msg);
}

##' @title ObjectType
##' @param obj an object
##' @description Returns the type of object received by R Development function
##' @return string
##' @keywords internal
ObjectType <- function(obj) {
  return(utils::capture.output(utils::str(obj)))
}

##'@title setLogLevel
##'@description Allows to retreive a particular level debugging messages
##'@seealso https://docs.python.org/3/library/logging.html#levels
##'@param level character, Default value values "INFO", Allowed values "CRITICAL"," ERROR", "WARNING", "INFO", "DEBUG", "NOTSET"
##'@export
setLogLevel <- function(level = "INFO") {
  debugLevelList <- get("DEBUG_LEVEL", configWS)
  if (!is.null(level) && exists(level, where = debugLevelList)) {
    logging::setLevel(debugLevelList[[level]])
    print(paste("Log level set to", level))
  } else{
    logging::setLevel(debugLevelList[["INFO"]])
    logging::loginfo(paste(level, "not found 'INFO' level used instead"))
  }
}

##' @title setLoginUserInformations
##' @param tokenData S3 class, saves informations extract from WS getToken response
##' @description Save information in config environment
##' @keywords internal
setLoginUserInformations <-
  function(username,
           password,
           tokenData
           ) {
    # save user parameters in config environment
    assign("TOKEN_VALUE", tokenData$data, configWS)
    assign("USERNAME", username, configWS)
    assign("PASSWORD", password, configWS)
    assign("USER_VALID", TRUE, configWS)
    assign("TOKEN_VALID", TRUE, configWS)
  
    #debug
    logging::logdebug(paste("BASE_PATH", get("BASE_PATH", configWS)))
    logging::logdebug(paste("USERNAME", get("USERNAME", configWS)))
    logging::logdebug(paste("TOKEN_VALUE", get("TOKEN_VALUE", configWS)))
    logging::logdebug(paste("USER_VALID", get("USER_VALID", configWS)))
    logging::logdebug(paste("TOKEN_VALID", get("USER_VALID", configWS)))
   }


##' @title getApi
##' @description show useful informations from opensilex api
##' @return an object with raw informations from opensilex api
##' @export
getApi <- function() {
  if (is.null(get("OPENSILEX_API", configWS)))
    stop("Connect first using connectionToOpenSILEXWS() function")
   
  return(get("OPENSILEX_API", configWS))
}

##' @title getSchemas
##' @description show model schema from opensilex api
##' @return an object with raw informations about model schemas from opensilex api
##' @export
getSchemas <- function() {
  if (is.null(get("OPENSILEX_SCHEMAS", configWS)))
    stop("Connect first using connectionToOpenSILEXWS() function")
  
  return(get("OPENSILEX_SCHEMAS", configWS))
}

##' @title getOperations
##' @description show operations from opensilex api
##' @return an object with raw informations about operations from opensilex api
##' @export
getOperations <- function() {
  if (is.null(get("OPENSILEX_OPERATIONS", configWS)))
    stop("Connect first using connectionToOpenSILEXWS() function")
   
  return(get("OPENSILEX_OPERATIONS", configWS))
}

##' @title getConfigInformations
##' @description show useful informations from config environment
##' @return a dataframe with informations from config environment
##' @export
getUserInformations <- function() {
  if (is.null(get("TOKEN_VALUE", configWS)))
    stop("Connect first using connectionToOpenSILEXWS() function")
  
  df <- data.frame(
    "BASE_PATH" = get("BASE_PATH", configWS),
    "USERNAME" = get("USERNAME", configWS),
    "TOKEN_VALUE" = get("TOKEN_VALUE", configWS)
   )
  return(df)
}
