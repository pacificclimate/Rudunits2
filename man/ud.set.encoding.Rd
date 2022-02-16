\name{ud.set.encoding}
\alias{ud.set.encoding}
\title{Set the udunits package level encoding type}
\description{
  This function sets the encoding type parameter which is global to the
  R udunits2 package.
}
\usage{
ud.set.encoding(enc.string)
}
\arguments{
  \item{enc.string}{A character string representing the encoding type.
    Valid strings are \code{utf8},\code{ascii},\code{iso-8859-1},and
    \code{latin1} (an alias for ISO-8859-1).}
}
\details{
  Encoding type is a parameter to nearly all of the functions in the
  udunits library.  By default, the R udunits2 pacakge sets the encoding
  type to UTF-8, however this package allows the user to set other
  encoding types which are supported by the udunits library.  It
  presently suports UTF-8, ASCII, and ISO-8859-1
}
\value{
  Returns no value.  Raises an error if it is not given a valid encoding string.
}
\references{
  Unidata's udunits reference:
  \url{https://www.unidata.ucar.edu/software/udunits/}
  API guide chapter on data types:
  \url{https://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#Types}
}
\author{James Hiebert \email{hiebert@uvic.ca}}

\examples{
valid.enc.strings <- c('utf8', 'ascii', 'iso-8859-1', 'latin1')
lapply(valid.enc.strings, ud.set.encoding)
err <- try(ud.set.encoding("This will fail"))
}
