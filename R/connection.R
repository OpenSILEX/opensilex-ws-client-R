#-------------------------------------------------------------------------------
# Program: connection.R
# Objective: functions to connect to OpenSILEX instance
# Author: A. Charleroy
# Creation: 03/09/2019
# Update: 09/09/2019
#-------------------------------------------------------------------------------

##' @title connectToOpenSILEXWS
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @param username character, login of the user to create the token
##' @param password character, password of the user to create the token
##' @param reconnection logical, to force the client reconnection
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' Demonstration instances:
##' \describe{
##' \item{WS1}{connectToOpenSILEXWS(apiID="ws_1_public","guestphis@supagro.inra.fr","guestphis",
##'                            url = "http://147.100.179.156:8080/phenomeapi/resources/")}
##' \item{WS2}{connectToOpenSILEXWS(apiID="ws_private",username="guest@opensilex.org",
##'           password="guest", url = "http://www.opensilex.org/openSilexAPI/rest/")}
##' }
##' @export
connectToOpenSILEXWS<-function(username, password, url, reconnection = TRUE){
  
  if (is.null(username) || username == "") {
    stop("Please, give a username")
  }
  if (is.null(password) ||  password == "") {
    stop("Please, give a user password")
  }  
  if (is.null(url) || url == "") {
    stop("Please, give a URL")
  }  
  
  # set url
  assign("BASE_PATH", url, configWS)
  
  # get token
  tokenData = opensilexWSClientR::getToken(username,password)
  
  if(!is.null(tokenData) && length(tokenData) > 0) {
    setLoginUserInformations(username, password, tokenData, reconnection)
  }else{
    stop("Not able to connect to the specified OpenSILEX WS")
  }
} 


##' @title connectToOpenSILEXWSWithToken
##' @description Save information in config environment
##' @param token character, set a token without reconnection
##' @param wsVersion numeric, the version of the webservice
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @export
connectToOpenSILEXWSWithToken<-function(token, url, wsVersion = 2){
  # save user parameters in config environment
  assign("BASE_PATH", url, configWS)
  assign("TOKEN_VALUE", token, configWS)
  assign("USERNAME", "", configWS)
  assign("PASSWORD", "", configWS)
  assign("WS_VERSION", wsVersion, configWS)
  assign("TOKEN_VALID",TRUE,configWS)
  assign("USER_VALID",TRUE,configWS)
  
  # set reconnection variable
  assign("RECONNECT_ON_DISCONNECTION", FALSE)
  
  #debug
  logging::logdebug(paste("BASE_PATH",get("BASE_PATH", configWS)))
  logging::logdebug(paste("USERNAME",get("USERNAME",configWS)))
  logging::logdebug(paste("TOKEN_VALUE",get("TOKEN_VALUE",configWS)))
  logging::logdebug(paste("WS_VERSION",get("WS_VERSION",configWS)))
  logging::logdebug(paste("TOKEN_VALID_TIME",get("TOKEN_VALID_TIME",configWS)))
}