  #-------------------------------------------------------------------------------
# Program: getCall.R
# Objective: functions to facilitate GET requests on a OpenSILEX web service
#             * getResponseFromWS: Dispatch responses for WS1 or WS2
#             * getResponseFromWS1: retreive data for WS1
#             * getResponseFromWS2: retreive data for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 03/09/2019 by I.Sanchez, 07/10/2019 by A. Charleroy
#-------------------------------------------------------------------------------

##' @title getResponseFromWS retrieves the data of a service from the WS
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the GET request
##' @param type character, the type of the output, default to application/json
##' @param wsVersion numeric, the version of the webservice
##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
##' @return  responseObject object HTTP httr
##' @export
getResponseFromWS<-function(resource,paramPath = NULL,attributes,wsVersion,type="application/json"){
    if(!get("USER_VALID",configWS)) stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
   
    # set Page
    if(exists("page", where=attributes) && is.null(attributes[["page"]])){
       attributes[["page"]]  <- get("DEFAULT_PAGE", configWS)
    }
    
    # set pageSize
    if(exists("pageSize", where=attributes) && is.null(attributes[["pageSize"]])){
     attributes[["pageSize"]]  <- get("DEFAULT_PAGESIZE", configWS)
    }
   
    responseWS <- getResponse(resource = resource, paramPath= paramPath, attributes = attributes, type= type)
    
    return(responseWS)
}



#----------------------------------------------------------------------#
##' @title getResponse retrieves the data of a service from the WS
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param resource character, the name of the service to call
##' @param paramPath character, the extension of the service to call, default to NULL
##' @param attributes character, the list of attributes to give to the GET request
##' @param type character, the type of the output, default to application/json
##' @return responseObject object HTTP httr
##' @keywords internal
getResponse <- function(resource, paramPath = NULL, attributes, type = "application/json"){
  # test ws type
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
  
  return(getDataAndShowStatus(r))
}
