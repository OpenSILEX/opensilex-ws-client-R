#-------------------------------------------------------------------------------
# Program: deleteCall.R
# Objective: functions to facilitate delete requests on OpenSILEX web service
# Author: A. Charleroy
# Creation: 19/03/2018
# Update: 04/04/2019 by I.Sanchez - 03/09/2019 by A.Charleroy
#-------------------------------------------------------------------------------

##' @title deleteResponseFromWS
##'
##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
##' @param resource character, the name of the webservice resource
##' @param paramPath character, path URL encoded parameter
##' @param attributes query parameters
##' @param encode character, type of encodage
##' @param requestBody body data which will be send
##' @return WSResponse WSResponse class instance
##' @export
deleteResponseFromWS<-function(resource, requestBody, paramPath = NULL, attributes = list(),  encode ="json"){
  #configWS<-connectToOpenSILEXWS()
  webserviceBaseUrl <- configWS[["BASE_PATH"]]
  urlParams = ""
  # create the l'url
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams = paste0(urlParams,"&")
    }
    #     chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams = paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
      #       nombres
    } else if (is.numeric(attributes[[attribut]])){
      urlParams = paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
      # autres
    } else {
      urlParams = paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl = paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl = paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }

  ptm <- proc.time()
  r <- httr::DELETE(finalurl, body = requestBody,encode="json")
  
  # debug
  logging::logdebug("Request Time : " )
  if(logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 

  if(r$status_code >= 500){
    logging::logerror("WebService internal error")
  }
  if(r$status_code == 401){
    logging::logwarn("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    logging::logerror("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    logging::loginfo("Query executed and data recovered")
  }
  return(getDataAndShowStatus(r))
}
