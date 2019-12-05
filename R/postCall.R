#-------------------------------------------------------------------------------
# Program: postCall.R
# Objective: functions to facilitate POST requests on a OpenSILEX web service
#             * postResponseFromWS: Dispatch responses for WS1 or WS2
#             * postResponseFromWS1: send data to WS1
#             * postResponseFromWS2: send data to WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/09/2019
# Update: 
#-------------------------------------------------------------------------------

##' @title postResponseFromWS retrieves the data of a service from the WS
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the POST request
##' @param type character, the type of the output, default to application/json
##' @param wsVersion numeric, the version of the webservice
##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
##' @return  responseObject object HTTP httr
##' @export
postResponseFromWS<-function(resource, paramPath = NULL, attributes, wsVersion, type="application/json"){
  if(!get("USER_VALID",configWS)) stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
  
  if(!get("TOKEN_VALID",configWS)){ #  token not valid 
    if(get("RECONNECT_ON_DISCONNECTION",configWS)){ # try to reconnect
      logging::loginfo("Token expired. Reconnecting to WS ... ")
      connectToOpenSILEXWS(get("USERNAME",configWS),get("PASSWORD",configWS),get("BASE_PATH",configWS))
      if(!get("TOKEN_VALID",configWS)) stop("The reconnection has failed")
      logging::loginfo("Reconnected to WS ... ")
    }else{ 
      stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
    }
  }
  
  if(wsVersion != get("WS_VERSION",configWS)) stop("You cannot use this service on this OpenSILEX Instance")
  
  # set Page
  if(exists("page", where=attributes) && is.null(attributes[["page"]])){
    attributes[["page"]]  <- get("DEFAULT_PAGE", configWS)
  }
  
  # set pageSize
  if(exists("pageSize", where=attributes) && is.null(attributes[["pageSize"]])){
    attributes[["pageSize"]]  <- get("DEFAULT_PAGESIZE", configWS)
  }
  
  if(wsVersion == 1){
    responseWS <- postResponseFromWS1(resource = resource, paramPath= paramPath, attributes = attributes, type= type)
  }
  
  if(wsVersion == 2){
    responseWS <- postResponseFromWS2(resource = resource, paramPath= paramPath, attributes = attributes, type= type)
  }
  return(responseWS)
}


#----------------------------------------------------------------------#
##' @title postResponseFromWS1 retrieves the data of a service from the WS1
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the POST request
##' @param type character, the type of the output, default to application/json
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @return  responseObject object HTTP httr
##' @keywords internal
postResponseFromWS1<-function(resource,paramPath = NULL,attributes,type="application/json"){
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
  r <- httr::POST(finalurl)
  #debug
  logging::logdebug("Request Time : " )
  if( logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  }
  
  return(getDataAndShowStatus(r))
}

#----------------------------------------------------------------------#
##' @title postResponseFromWS2 retrieves the data of a service from the WS2
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the POST request
##' @param type character, the type of the output, default to application/json
##' @return responseObject object HTTP httr
##' @keywords internal
postResponseFromWS2 <- function(resource, paramPath = NULL, attributes, type = "application/json"){
  # test ws type
  if( get("WS_VERSION", configWS))
  webserviceBaseUrl <- get("BASE_PATH", configWS)
  
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource , "/", paramPath)
  }
  if(!is.data.frame(attributes)) attributes = list(attributes)
    
  ptm <- proc.time()
    r <- httr::POST(finalurl, config = httr::add_headers(Authorization=paste("Bearer ",get("TOKEN_VALUE",configWS), sep = "")), body = attributes, encode = "json")
  print(r)
  #debug
  logging::logdebug("Request Time : " )
  if(logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 
  
  return(getDataAndShowStatus(r))
}
