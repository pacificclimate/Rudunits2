\name{udunits2-package}
\alias{udunits2-package}
\alias{udunits2}
\docType{package}
\title{udunits-2 bindings for R}
\description{This package provides simple bindings to version 2 of Unidata's udunits library}
\details{
\tabular{ll}{
Package: \tab udunits2\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2011-02-11\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
This package provides simple bindings to the version 2 API of Unidata's
udunits library.  While the entire API is not supported, we have chosen
to boil it down to a few simple functions to be able to exploit the most
useful functionality that the library provides.  This package provides
the following functions:
\itemize{
  \item \code{ud.is.parseable}
  \item \code{ud.get.name}
  \item \code{ud.get.symbol}
  \item \code{ud.are.convertible}
  \item \code{ud.convert}
}
Please see the respective function help pages for further details and usage.
}
\author{
  James Hiebert <hiebert@uvic.ca>

Maintainer:   James Hiebert <hiebert@uvic.ca>
}
\references{
  Unidata's udunits web page:
  \url{http://www.unidata.ucar.edu/software/udunits/}
}
\keyword{unitdata}
\keyword{units}
\keyword{climate}
\keyword{meteorology}
\seealso{
  \code{\link{ud.is.parseable} \link{ud.get.name} \link{ud.get.symbol}
    \link{ud.are.convertible} \link{ud.convert}}
}
