#-------------------------------------------------------------------------------
# Program: getResponseFromWS.R
# Objective: functions to facilitate requests on a OpenSILEX web service
#             * getResponseFromWS: Dispatch responses for WS1 or WS2
#             * getResponseFromWS1: retreive data for WS1
#             * getResponseFromWS2: retreive data for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 03/09/2019 by I.Sanchez
#-------------------------------------------------------------------------------

##' @title getResponseFromWS retrieves the data of a service from the WS
##'
##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
##' @param responseObject object HTTP httr
##' @export
getResponseFromWS<-function(resource,paramPath = NULL,attributes,wsVersion,type="application/json"){
    if(!get("USER_VALID",configWS)) stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
    
    if(!get("TOKEN_VALID",configWS) && get("RECONNECT_ON_DISCONNECTION",configWS)){
      logging::loginfo("Token expired. Reconnecting to WS ... ")
      connectToOpenSILEXWS("ws_private",get("USERNAME",configWS),get("PASSWORD",configWS),get("BASE_PATH",configWS))
      if(!get("TOKEN_VALID",configWS)) stop("You cannot use this service on this OpenSILEX Instance")
      logging::loginfo("Reconnected to WS ... ")
    }else{
      stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
    }
    
    if(wsVersion != get("WS_VERSION",configWS)) stop("You cannot use this service on this OpenSILEX Instance")
    
    # set Page
    if(exists(page, where=attributes) && is.null(attributes[["page"]])){
       attributes[["page"]]  <- get("DEFAULT_PAGE", configWS)
    }
    
    # set pageSize
    if(exists(pageSize, where=attributes) && is.null(attributes[["pageSize"]])){
     attributes[["pageSize"]]  <- get("DEFAULT_PAGESIZE", configWS)
    }
   
   if(wsVersion == 1){
      responseWS <- getResponseFromWS1(resource = resource, paramPath= paramPath, attributes = attributes, type= type)
    }
  
  if(wsVersion == 2){
      responseWS <- getResponseFromWS2(resource = resource, paramPath= paramPath, attributes = attributes, type= type)
    }
  return(responseWS)
}


##' @title getResponseFromWS1 retrieves the data of a service from the WS1
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the GET request
##' @param type character, the type of the output, default to application/json
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @return  responseObject object HTTP httr
##' @keywords internal
getResponseFromWS1<-function(resource,paramPath = NULL,attributes,type="application/json"){
  attributes[["sessionId"]] = get("TOKEN_VALUE",configWS)
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # creation de l'url
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    # chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
      # nombres
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
      # autres
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }
  ptm <- proc.time()
  r <- httr::GET(finalurl)
  #debug
  logging::logdebug("Request Time : " )
  if( logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 
  
  if(r$status_code >= 500){
    logging::logerror("WebService internal error")
  }
  if(r$status_code == 401){
    logging::logerror("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    logging::logerror("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    logging::loginfo("Query executed and data recovered")
  }
  
  return(getDataAndShowStatus(r))
}

#----------------------------------------------------------------------#
##' @title getResponseFromWS2 retrieves the data of a service from the WS2
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the GET request
##' @param type character, the type of the output, default to application/json
##' @return  responseObject object HTTP httr
##' @keywords internal
getResponseFromWS2 <- function(resource, paramPath = NULL, attributes, type = "application/json"){
  # test ws type
  if( get("WS_VERSION", configWS))
  
  webserviceBaseUrl <- get("BASE_PATH", configWS)
  urlParams <- ""
  # url concatenation
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams, "&")
    }
    #     character arguments
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams, attribut, "=", utils::URLencode(attributes[[attribut]], reserved = TRUE))
      #   numeric arguments
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams, attribut, "=", format(attributes[[attribut]], scientific = FALSE))
      #   other arguments
    } else {
      urlParams <- paste0(urlParams, attribut, "=", attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource , "/", paramPath, "?", urlParams)
  }
  
  ptm <- proc.time()
  r <- httr::GET(finalurl, config = httr::add_headers(Authorization=paste("Bearer ",get("TOKEN_VALUE",configWS), sep = "")))
  #debug
  logging::logdebug("Request Time : " )
  if(logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 
  
  if(r$status_code >= 500){
    logging::logerror("WebService internal error")
  }
  if(r$status_code == 401){
    logging::logerror("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    logging::logerror("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    logging::loginfo("Query executed and data recovered")
  }
  
  return(getDataAndShowStatus(r))
}
