# opensilexWSClientR

OpenSILEX WS client R

A set of basic functions to connect R to an OpenSILEX web service. Public access is allowed with specific login as well as private access if the user has an account on an instance of the OpenSILEX system information.

This package is used by other packages.

- [phisWSClientR](https://github.com/OpenSILEX/phisWSClientR/tree/master)

## Installation

To install the **opensilexWSClientR** package, the easiest is to install it directly from Github. Open an R session and run the following commands:

```R
library(remotes)
install_github("OpenSILEX/opensilex-ws-client-r", build_vignettes=TRUE,ref="1.1.1")
```

You can also download a tar.gz archive of "[1.1.1](https://github.com/OpenSILEX/opensilex-ws-client-r/tree/1.1.1") version and install it with _install_packages()_.

This package use [Semantic Versioning Specification](https://semver.org/) for versionning tags.

## Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(opensilexWSClientR)
help(package="opensilexWSClientR")
```

## Use this R package in another R package from github

How to had a github R package to description file : [dependencies](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd)

## Use this R package in another a shiny application to retreive parameters
set a connection
```R
##' @title connectShinyAppToOpenSILEX
##' @description Retreive connection parameters automatically 
##'              from the shiny app url
##' @param wsVersion numeric, version of the web service by default 
##'        it's equals to 2, (the latest version of OpenSILEX
##'        webservice) 
##' @export
connectShinyAppToOpenSILEX<-function(wsVersion = 2){
    # Here you read the URL parameter from session$clientData$url_search
    shiny::observe({
      query <- shiny::parseQueryString(shiny::session$clientData$url_search)
      if (!is.null(query[['token']]) && !is.null(query[['wsUrl']])) {
        connectToOpenSILEXWSWithToken(query[['token']], query[['wsUrl']], wsVersion = wsVersion)
      }else{
        stop("Can't find requested parameters \"wsUrl\" and \"token\" in URL")
      }
    })
}
```

## Test

You can give a test to the package using the available vignettes (/doc directory) and use the documentation. if you have some difficulties to retrieve the html vignettes, you can use https://rawgit.com on the github file paths:

- [OpenSILEXWSConnection.html](https://github.com/OpenSILEX/opensilex-ws-client-r/blob/master/doc/OpenSILEXWSConnection.html)

## Citation

You should cite the **opensilexWSClientR** package:

```R
citation("opensilexWSClientR")
```

See also citation() for citing R itself.
