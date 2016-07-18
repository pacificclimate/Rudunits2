\name{ud.have.unit.system}
\alias{ud.have.unit.system}
\title{Determine whether udunits has loaded its units database}
\description{
  This function check wether or not udunits has successfully found and
  loaded its run-time XML units database.
}
\usage{
ud.have.unit.system()
}
\details{
  At package load time, Rudunits attempts to load a unit system from an
  XML units database from the file system. This might be installed with
  the system library (e.g. through apt or yum), or the user can use
  their own. The file-system location is configured using the
  UDUNITS2_XML_PATH environment variable.

  This package will attempt to load the path contained in
  UDUNITS2_XML_PATH. If it's empty, it will attempt to load it from the
  system library. Failing that it will attempt to load its own XML
  database that ships with the package (from udunits source).

  One can call \code{ud.have.unit.system} to confirm that the units
  database has been loaded successfully.
}
\value{
  Returns a logical: \code{True} if udunits has successfully found and
  loaded the XML units database, \code{False} otherwise.
}
\author{
  James Hiebert \email{hiebert@uvic.ca}
}
\examples{
ud.have.unit.system() # TRUE
}
