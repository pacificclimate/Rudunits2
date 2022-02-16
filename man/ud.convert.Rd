\name{ud.convert}
\alias{ud.convert}
\title{Convert numeric types from one unit to another}
\description{This function takes the numeric argument \code{x},
  quantified in units \code{u1} and converts it to be of units \code{u2}.
}
\usage{
ud.convert(x, u1, u2)
}
\arguments{
  \item{x}{Some argument which is convertible to a numeric type by \code{as.double}.}
  \item{u1}{A character string which is parseable into a udunits compatible unit.}
  \item{u2}{Another character string which is also parseable into a
    udunits compatible unit and for which there exists a defined
    transformation from the units represented by u1.}
}
\details{
  This function uses the udunits function \code{cv_convert_doubles} to
  convert the argument from one set of units to another.
}
\value{
  Returns a numeric type having converted from one unit to another.  The
  attributes of the original argument \code{x} (e.g. class, dimensions,
  etc.) are preserved and then re-applied to the return value of the
  transformation as such:
  \code{attributes(rv) <- attributes(x)}
  If either of unit \code{u1} or \code{u2} is unparseable, or there does
  not exist a conversion from one to the other the function raises an error.
}
\references{
  Unidata's udunits reference:
  \url{https://www.unidata.ucar.edu/software/udunits/}
  API guide for cv_convert_doubles:
  \url{https://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-cv_005fconvert_005fdoubles-39}
}
\author{
  James Hiebert \email{hiebert@uvic.ca}
}
\seealso{
  \code{\link{ud.are.convertible}}
}
\examples{
x <- seq(10)
ud.convert(x, "miles", "km")                   # c(1.609344, 3.218688, 4.828032, ...)
x <- c(-40, 0, 100)
ud.convert(x, "celsius", "degree_fahrenheit")  # c(-40, 32, 212)
err <- try(ud.convert(100,"miles", "grams"))   # Error
err <- try(ud.convert(NA, "not", "parseable")) # Error
}
