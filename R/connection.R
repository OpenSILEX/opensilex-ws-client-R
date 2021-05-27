#-------------------------------------------------------------------------------
# Program: connection.R
# Objective: functions to connect to OpenSILEX instance
# Author: A. Charleroy
# Creation: 03/09/2019
# Update: 09/09/2019
#-------------------------------------------------------------------------------

##' @title connectToOpenSILEXWS
##' @param identifier character, login of the user to create the token
##' @param password character, password of the user to create the token
##' @param url character
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' Demonstration instances:
##' \describe{
##' connectToOpenSILEXWS(
##'           identifier="guest@opensilex.org",
##'           password="guest", 
##'           url = "http://www.opensilex.org/rest/")
##' }
##' @import rapiclient
##' @export
connectToOpenSILEXWS<-function(identifier, password, url){
  
  if (is.null(identifier) || identifier == "") {
    stop("Please, give a identifier")
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
  tokenData = getToken(identifier,password)
   
  if(!is.null(tokenData) && length(tokenData) > 0) {
      setLoginUserInformations(identifier, password, tokenData)
    
      opensilex_api <- rapiclient::get_api(url = paste0(url,"/swagger.json"))
      # add /rest to opensilex endpoint
      opensilex_api$host = paste0(opensilex_api$host, "/rest")
      operations <- rapiclient::get_operations(opensilex_api, .headers = c("Authorization" = paste("Bearer",get("TOKEN_VALUE",configWS))) )
      schemas <- rapiclient::get_schemas(opensilex_api)
      assign("OPENSILEX_API", opensilex_api, configWS)
      assign("OPENSILEX_SCHEMAS", schemas, configWS)
      assign("OPENSILEX_OPERATIONS", operations, configWS)
  }else{
    stop("Not able to connect to the specified OpenSILEX WS")
  }
} 


##' @title connectToOpenSILEXWSWithToken
##' @description Save information in config environment
##' @param token character, set a token without reconnection
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the full url with the protocol. e.g. 'http://www.opensilex.org/openSilexAPI/rest/'
##' @export
connectToOpenSILEXWSWithToken<-function(token, url){
  
  # TODO : get user informations
  # save user parameters in config environment
  assign("BASE_PATH", url, configWS)
  assign("TOKEN_VALUE", token, configWS)
  assign("IDENTIFIER", "", configWS)
  assign("PASSWORD", "", configWS)
  assign("TOKEN_VALID",TRUE,configWS)
  assign("USER_VALID",TRUE,configWS)
  
  opensilex_api <- rapiclient::get_api(url = paste0(url,"/swagger.json"))
  # add /rest to opensilex endpoint
  opensilex_api$host = paste0(opensilex_api$host, "/rest")
  operations <- rapiclient::get_operations(opensilex_api, .headers = c("Authorization" = paste("Bearer",get("TOKEN_VALUE",configWS))) )
  schemas <- rapiclient::get_schemas(opensilex_api)
  assign("OPENSILEX_API", opensilex_api, configWS)
  assign("OPENSILEX_SCHEMAS", schemas, configWS)
  assign("OPENSILEX_OPERATIONS", operations, configWS) 
  
  # set reconnection variable
  
  #debug
  logging::logdebug(paste("BASE_PATH",get("BASE_PATH", configWS)))
  logging::logdebug(paste("IDENTIFIER",get("IDENTIFIER",configWS)))
  logging::logdebug(paste("TOKEN_VALUE",get("TOKEN_VALUE",configWS)))
  logging::logdebug(paste("WS_VERSION",get("WS_VERSION",configWS)))
  logging::logdebug(paste("TOKEN_VALID_TIME",get("TOKEN_VALID_TIME",configWS)))
}