# opensilexWSClientR

OpenSILEX WS client R

A set of basic functions to connect R to an OpenSILEX web service. Public access is allowed with specific login as well as private access if the user has an account on an instance of the OpenSILEX system information.

This package is used by other packages.

- [phisWSClientR](https://github.com/OpenSILEX/phisWSClientR/tree/master)

## Installation

To install the **opensilexWSClientR** package, the easiest is to install it directly from Github. Open an R session and run the following commands:

```R
library(remotes)
install_github("OpenSILEX/opensilex-ws-client-r", build_vignettes=TRUE,ref="3.0.0")
```

You can also download a tar.gz archive of "[3.0.0](https://github.com/OpenSILEX/opensilex-ws-client-r/tree/3.0.0") version and install it with _install_packages()_.

This package use [Semantic Versioning Specification](https://semver.org/) for versionning tags.

## Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(opensilexWSClientR)
help(package="opensilexWSClientR")
```

## Get Information from opensilex API

```R
library(opensilexWSClientR)
opensilexClientToolsR::connectToOpenSILEX(identifier = "guest@opensilex.org", password = "guest",url = "http://localhost:8666/rest")
sc <- getSchemas()
sc$ProjectGetDTO
#ProjectGetDTO(uri, name, shortname, start_date, end_date, description, objective, financial_funding, website,
#    related_projects, coordinators, scientific_contacts, administrative_contacts, experiments)

op <- getOperations()
res <- op$searchProjects()
result <- getDataAndMetadataFromResponse(res)
s <- data.frame(d<-unlist(result$data),names(d))
result$data
# $uri
# [1] "http://www.opensilex.org/demo/PHENOME-FPPN" "test-prj:abt_test"                          "test-prj:test"                             
# [4] "test-prj:test/1"                           
# .....

```

## Use this R package in another R package from github

How to had a github R package to description file : [dependencies](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd)

## Use this R package in another a shiny application to retreive parameters
set a connection
```R
##' @title connectShinyAppToOpenSILEX
##' @description Retreive connection parameters automatically 
##'              from the shiny app url
##' @export
connectShinyAppToOpenSILEX<-function(){
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
