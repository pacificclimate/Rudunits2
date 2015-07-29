\name{ud.get.name}
\alias{ud.get.name}
\alias{ud.get.symbol}
\title{Retrieve the udunits name or symbol from the database for a given
  units string}
\description{
  Retrieve the udunits name or symbol from the database for a given
  units string.
}
\usage{
ud.get.name(unit.string)
}
\arguments{
  \item{unit.string}{A character string which is parseable into a udunits compatible unit.}
}
\details{
  This function retrieves the udunits name or symbol from the udunits
  database and returns it.  It uses the udunits functions ut_get_name
  and ut_get_symbol respectively.
}
\value{
  Returns a character string stating the udunits's name/symbol for the
  given unit, or an empty character string if the unit does not map to a
  name/symbol for the default character set.
  If the unit is unparseable, the function raises an error.
}
\references{
  Unidata's udunits reference:
  \url{http://www.unidata.ucar.edu/software/udunits/}
  API guide for ut_get_name:
  \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-ut_005fget_005fname-66}
  API guide for ut_get_symbol: \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-ut_005fget_005fsymbol-67}
}
\author{James Hiebert \email{hiebert@uvic.ca}}

\note{
  More often than not units do not have names or symbols that are
  returned by the base functions.  This depends entirely on what is
  defined in the units data base, which is--as of API version 2--an XML
  database which ships with the library.  See Unidata's website for more
  information about the XML database:
  \url{http://www.unidata.ucar.edu/software/udunits/udunits-2-units.html}.
  All in all, don't put too much stock in them, for they are for
  convenience only. If your application \emph{requires} certain names
  and symbols to be present, the XML database is local and editable.
}

\examples{
units.to.display <- c("celsius", # has no name, messed up symbol (maybe a bug in R?)
                      "kg",
                      "hr",      # has no symbol
                      "K",
                      "degrees",
                      "m",
                      "ohm")

for (u in units.to.display) {
  print(ud.get.name(u))
  print(ud.get.symbol(u))
}
}
