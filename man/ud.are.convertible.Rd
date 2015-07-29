\name{ud.are.convertible}
\alias{ud.are.convertible}
\title{Determine whether two units may be converted between each other}
\description{
  This function takes udunits compatible strings and determines whether
  or not it is possible to convert between them.
}
\usage{
ud.are.convertible(u1, u2)
}
\arguments{
  \item{u1}{A character string which is parseable into a udunits compatible unit.}
  \item{u2}{Another character string which is also parseable into a udunits compatible unit.}
}
\details{
  Even if two units are parseable and recognized by the udunits library,
  it may or may not be possible to convert from one to another.  For
  example, it makes sense to convert from celsius to kelvin, however not
  from celsius to kilograms.  This function allows the user to check if
  two units are of the same system and if there exists a defined conversion between
  the two.
}
\value{
  Returns a logical: \code{True} if the units can be converted between
  each other, \code{False} if either of the arguments is not parseable
  by udunits, or if no conversion is possible.
}
\references{
  See the udunits function ut_are_convertible:
  \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#ut_005fare_005fconvertible_0028_0029}
  and the main uninits webpage: \url{http://www.unidata.ucar.edu/software/udunits/}
}
\author{
  James Hiebert \email{hiebert@uvic.ca}
}
\seealso{
  \code{\link{ud.is.parseable}}
}
\examples{
ud.are.convertible("miles", "km")        # TRUE
ud.are.convertible("grams", "kilograms") # TRUE
ud.are.convertible("celsius", "grams")   # FALSE
ud.are.convertible("not", "parseable")   # FALSE
}
