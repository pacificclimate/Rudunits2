\name{ud.is.parseable}
\alias{ud.is.parseable}
\title{Determine whether a unit string is parseable by the udunits library}
\description{
  Determine whether a unit string is parseable and recognized by the udunits library.
}
\usage{
ud.is.parseable(unit.string)
}
\arguments{
  \item{unit.string}{A character string representing a type of units
    which may be parseable by the udunits library}
}
\details{
  \code{ud.is.parseable} uses udunit's function \code{ut_parse} to
  determine whether or not the given unit string is parseable.  If
  \code{ut_parse} returns NULL, then \code{ud.is.parseable} will return \code{FALSE}.
}
\value{
  Returns a logical: \code{True} if the units is parseable and
  recognized by the udunits library, \code{False} otherwise.
}
\references{
  Unidata's udunits reference:
  \url{http://www.unidata.ucar.edu/software/udunits/}
  API guide for ut_parse:
  \url{http://www.unidata.ucar.edu/software/udunits/udunits-2/udunits2lib.html#index-ut_005fparse-43}
}
\author{
  James Hiebert \email{hiebert@uvic.ca}
}
\note{
  There is a note in the \code{ut_parse} docs about how the
  argument string must have no leading or trailing whitespace.  We make
  sure in this package to always call \code{ut_trim} on any strings
  before they are passed to \code{ut_parse}.  The package user need not
  strip whitespace before-hand.
}

\seealso{
  \code{\link{ud.are.convertible}}
}
\examples{
ud.is.parseable("K")             # TRUE
ud.is.parseable("  K  ")         # TRUE
ud.is.parseable("miles")         # TRUE
ud.is.parseable("Not parseable") # FALSE
}
