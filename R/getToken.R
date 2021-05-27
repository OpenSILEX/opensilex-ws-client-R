#-------------------------------------------------------------------------------
# Program: getToken.R
# Objective: generic function to requests OpenSILEX WebService
# Author: A. Charleroy
# Creation: 12/08/2016
# Update: 09/09/2019 (by I.Sanchez) - 03/09/2019 (by  A. Charleroy)
#-------------------------------------------------------------------------------

##' @title retrieves a user identifier for connexion to the web service
##'
##' @description Retrieves a user identifier for connexion to the WebService (WS)
##' @param identifier character, login of the user to create the token
##' @param password character, password of the user to create the token

##' @return a session token user identifier in the WS
##' @keywords internal
getToken <- function(identifier, password) {
  attributes <- list(identifier = identifier, password = password)
  
  tokenResp <-
    getTokenResponse(resource = get("AUTHENTICATION", configWS),
                     attributes = attributes)
 
  response <- list()
  if (tokenResp$status_code >= 200 && tokenResp$status_code < 400) {
    json = jsonlite::fromJSON(httr::content(tokenResp, as = "text", encoding = "UTF-8"))
    
    response <- list(
      codeHttp = tokenResp$status_code,
      codeHttpMessage = "Query executed and data recovered",
      codeStatusMessage = json$metadata$status,
      data = json$result$token
    )
    # set WS TYPE in config environment
    logging::loginfo(response[["codeHttpMessage"]])
  } else if (tokenResp$status_code > 500 ) {
    logging::logerror("WebService internal error")
  } else if (tokenResp$status_code == 401 ) {
    logging::logerror("User not authorized")
  } else if (tokenResp$status_code == 404 ) {
    logging::logerror("Not found")
  } else if (tokenResp$status_code > 400 ) {
    logging::logwarn("Bad user request")
  }
  
  if (tokenResp$status_code > 250 && tokenResp$status_code > 250) {
    logging::logwarn("No web service available! Check your login/password and/or your url...")
  }
 
  # define class S3 and return the list if exists
  if (length(response)) {
    class(response) <- append(class(response), "WSResponse")
    return(response)
  } else{
    return(NULL)
  }
}


##' @title getTokenResponse
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
getTokenResponse <-
  function(resource, attributes, type = "application/json") {
    # create the URL
    finalurl <- paste0(get("BASE_PATH", configWS), "/", resource)
    
    # Create the body JSON list with the attributes
    # take care that httr::POST function doesn't allow to md5 object
    # I had to convert the md5 object into a string one with the toString() function
    finalbody <- list(identifier = attributes[["identifier"]],
                      password = attributes[["password"]])
    
    # call
    ptm <- proc.time()
 
    r <- httr::POST(url = finalurl,
                    body = finalbody,
                    encode = "json")
    
    # debug
    logging::logdebug("Request Time : ")
    if (logging::getLogger()$level == get("DEBUG_LEVEL", configWS)[["DEBUG"]]) {
      print(proc.time() - ptm)
      print(r)
    }
    
    return(r)
  }