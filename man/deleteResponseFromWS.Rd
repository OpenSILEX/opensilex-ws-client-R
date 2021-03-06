% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleteCall.R
\name{deleteResponseFromWS}
\alias{deleteResponseFromWS}
\title{deleteResponseFromWS}
\usage{
deleteResponseFromWS(resource, paramPath = NULL, attributes = list())
}
\arguments{
\item{resource}{character, the name of the webservice resource}

\item{paramPath}{character, path URL encoded parameter}

\item{attributes}{query parameters}
}
\value{
WSResponse WSResponse class instance
}
\description{
Create a URL to call the WS and return a formatted response of WSResponse class.
}
