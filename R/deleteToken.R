#-------------------------------------------------------------------------------
# Program: deleteToken.R
# Objective: generic function to requests OpenSILEX WebService
# Author: A. Charleroy
# Creation: 03/09/2019
# Update: 09/09/2019
#-------------------------------------------------------------------------------

##' @title disconnect a user identifier for connexion to the web service

##' @return a boolean TRUE if succeeeded
##'                   FALSE if failed
##' @examples
##' \donttest{
##' disconnectFromOpenSILEXWS()
##' }
##' @export
disconnectFromOpenSILEXWS<-function(){
  if(!get("USER_VALID",configWS)) stop("You must first connect to an OpenSILEX Instance using connectToOpenSILEXWS() function")
  
  # delete WebService user session
  
  deleteResponseFromWS(resource = get("DISCONNECTION",configWS))
  
  
  assign("BASE_PATH","", configWS)
  assign("IDENTIFIER","", configWS)
  assign("PASSWORD","", configWS)
  assign("TOKEN_VALID", FALSE,configWS)
  assign("USER_VALID", FALSE,configWS)
}