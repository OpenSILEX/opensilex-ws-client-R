# opensilexWSClientR

OpenSILEX WS client R

A set of basic functions to connect R to an OpenSILEX web service. Public access is allowed with specific login as well as private access if the user has an account on an instance of the OpenSILEX system information.

This package is used by other packages.

- [phisWSClientR](https://github.com/OpenSILEX/phisWSClientR/tree/master)

# Installation

To install the **opensilexWSClientR** package, the easiest is to install it directly from Github. Open an R session and run the following commands:

```R
library(remotes)
install_github("OpenSILEX/opensilex-ws-client-r", build_vignettes=TRUE)
```

You can also download a tar.gz archive of "[master](https://github.com/OpenSILEX/opensilex-ws-client-r/tree/master)" version and install it with _install_packages()_.

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(opensilexWSClientR)
help(package="opensilexWSClientR")
```

# Use this R package in another R package from github

How to had a github R package to description file : [dependencies](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd)

# Test

You can give a test to the package using the available vignettes (/doc directory) and use the documentation. if you have some difficulties to retrieve the html vignettes, you can use https://rawgit.com on the github file paths:

- https://github.com/OpenSILEX/opensilex-ws-client-r/blob/master/doc/OpenSILEXWSConnection.html

# Citation

You should cite the **opensilexWSClientR** package:

```R
citation("opensilexWSClientR")
```

See also citation() for citing R itself.
