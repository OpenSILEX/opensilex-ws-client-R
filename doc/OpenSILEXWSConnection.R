## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  library(opensilexWSClientR)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to an OpenSILEX web service, you have to insert the address of the WS and the port
  connectToOpenSILEXWS(username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")

## ----echo=TRUE,eval=FALSE,message=FALSE, warning=FALSE-------------------
#    setLogLevel("DEBUG")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  connectToOpenSILEXWS(username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")

## ----echo=TRUE,eval=FALSE,message=FALSE, warning=FALSE-------------------
#    # If you want to access to a private web service, you have to insert the address of the WS and the port
#    connectToOpenSILEXWS("guestphis@supagro.inra.fr","guestphis",url = "147.99.7.5:8080/phenomeapi/resources/")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  connectToOpenSILEXWS(username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")

## ----session,echo=FALSE,message=FALSE, warning=FALSE---------------------
  sessionInfo()

