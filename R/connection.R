#-------------------------------------------------------------------------------
# Program: connection.R
# Objective: functions to connect to OpenSILEX instance
# Author: A. Charleroy
# Creation: 03/09/2019
# Update: 
#-------------------------------------------------------------------------------

##' @title connectToOpenSILEXWS
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @param username login of the user to create the token
##' @param password password of the user to create the token
##' @param reconnection to force the client reconnection
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' Demonstration instances
##' WS1 - connectToOpenSILEXWS(apiID="ws_1_public","guestphis@supagro.inra.fr","guestphis",
##'                            url = "http://147.100.179.156:8080/phenomeapi/resources/")
##' WS2 - connectToOpenSILEXWS(apiID="ws_private",username="guest@opensilex.org",
##'           password="guest", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' @export
connectToOpenSILEXWS<-function(username, password, url, reconnection = TRUE){
  
  if (is.null(username) || username == "") {
    stop("Please, give an username")
  }
  if (is.null(password) ||  password == "") {
    stop("Please, give an user password")
  }  
  if (is.null(url) || url == "") {
    stop("Please, give an url")
  }  
  
  assign("BASE_PATH", url, configWS)
  
  # get token
  tokenData = opensilexWSClientR::getToken(username,password)
  
  if(!is.null(tokenData) && length(tokenData) > 0) {
    setLoginUserInformations(username, password, tokenData, reconnection)
  }else{
    stop("Not able to connect to the specified OpenSILEX WS")
  }
} 