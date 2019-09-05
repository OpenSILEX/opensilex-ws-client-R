#-------------------------------------------------------------------------------
# Program: getToken.R
# Objective: generic function to requests OpenSILEX WebService
# Author: A. Charleroy
# Creation: 12/08/2016
# Update: 29/10/2018 (by I.Sanchez) - 03/09/2019 (by  A. Charleroy)
#-------------------------------------------------------------------------------

##' @title retrieves a user identifier for connexion to the web service 
##'
##' @description Retrieves a user identifier for connexion to the WebService (WS)
##' @param login login of the user to create the token
##' @param password password of the user to create the token

##' @return a session token user identifier in the WS
##' @examples
##' \donttest{
##' connectToOpenSILEXWS("guestphis@supagro.inra.fr","guestphis",
##'          "http://147.100.179.156:8080/phenomeapi/resources/")
##' aToken <- getToken("guestphis@supagro.inra.fr","guestphis")
##' aToken$data
##' }
##' @export
getToken<-function(login,password){
  attributes<-list(username = login, password = password)

  # Try 1 on first web service
  tokenResp1<-getTokenResponseWS(resource = get("TOKEN",configWS), attributes = attributes)

  # Try 2 on second web service
  tokenResp2<-getTokenResponseWS2(resource = get("BRAPITOKEN",configWS), attributes = attributes)

  # Test which WS is OK
  if (tokenResp1$status_code >= 200 && tokenResp1$status_code < 300 && tokenResp2$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp1, as = "text", encoding = "UTF-8"))
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = tokenResp1$status_code,
      codeHttpMessage = "Query executed and data recovered - WS1",
      codeStatusMessage = json$metadata$status,
      data = json$session_token,
      expiresIn = 1200, # @see http://147.100.179.156:8080/phenomeapi/api-docs/
      webserviceVersion=1)
    # set WS TYPE in config environment
    logging::loginfo(response[["codeHttpMessage"]])
    
  } else if (tokenResp2$status_code >= 200 && tokenResp2$status_code < 300 && tokenResp1$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp2, as = "text", encoding = "UTF-8"))

    response <- list(
      codeHttp = tokenResp2$status_code,
      codeHttpMessage = "Query executed and data recovered - WS2",
      codeStatusMessage = json$metadata$status,
      data = json$access_token,
      expiresIn = as.integer(json$expires_in),
      webserviceVersion=2)
    # set WS TYPE in config environment
    logging::loginfo(response[["codeHttpMessage"]])
  } else if(tokenResp1$status_code == 500 || tokenResp2$status_code == 500){
       logging::logerror("WebService internal error")
  } else if(tokenResp1$status_code == 401 || tokenResp2$status_code == 401){
       logging::logerror("User not authorized")
  } else if(tokenResp1$status_code == 404 || tokenResp2$status_code == 404){
       logging::logerror("Not found")
  } else if((tokenResp1$status_code >= 400 && tokenResp1$status_code != 401 &&
             tokenResp1$status_code != 404 && tokenResp1$status_code < 500) &&
            (tokenResp2$status_code >= 400 && tokenResp2$status_code != 401 &&
             tokenResp2$status_code != 404 && tokenResp2$status_code < 500)){
       logging::logwarn("Bad user request")
  }

  if (tokenResp1$status_code > 250 && tokenResp2$status_code > 250){
       logging::logwarn("No web service available! Check your login/password and/or your url...")
  }

  # define class S3 and return the list if exists
  if (exists("response")){
    class(response) <- append(class(response),"WSResponse")
    return(response)
  }else{
    return(NULL)
  }
}


##' @title getTokenResponseWS
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password

##' @details This function is OK for the first version of the web service
##'  (a GET call with a visible request)
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @return responseObject an object HTTP httr
##' @keywords internal
getTokenResponseWS<-function(resource,paramPath=NULL,attributes,type = "application/json"){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # create the URL
  for (attribut in names(attributes)) {
    if(urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    if(is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],  reserved = TRUE))
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if(is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }
  
  ptm <- proc.time()
  r <- httr::GET(finalurl)
  
  # debug
  logging::logdebug("Request Time : " )
  if( logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 
  return(r)
}


##' @title getTokenResponseWS2
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password

##' @details This function is OK for the second version of the web service
##'  (a POST call with an invisible request using a correct JSON list in a body)
##' @seealso https://brapi.docs.apiary.io/#introduction/structure-of-the-response-object
##' @return responseObject an object HTTP httr
##' @importFrom openssl md5
##' @keywords internal
getTokenResponseWS2<-function(resource,attributes,type = "application/json"){
  # create the URL
  finalurl <- paste0(get("BASE_PATH",configWS),resource)
  
  # Create the body JSON list with the attributes
  # take care that httr::POST function doesn't allow to md5 object
  # I had to convert the md5 object into a string one with the toString() function
  finalbody<-list(grant_type="password",
                  username= attributes[[1]],
                  password=toString(md5(attributes[[2]])))
  
  # call
  ptm <- proc.time()
  r <- httr::POST(url=finalurl,body = finalbody,encode="json")
  
  # debug
  logging::logdebug("Request Time : " )
  if( logging::getLogger()$level == get("DEBUG_LEVEL",configWS)[["DEBUG"]]){
    print(proc.time() - ptm)
    print(r)
  } 
  
  return(r)
}