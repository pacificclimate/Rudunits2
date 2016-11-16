.onLoad <- function(libname, pkgname) {
  ## By default, configure udunits with path set (presumably) by the
  ## user through the UDUNITS2_XML_PATH environment variable
  .C('R_ut_init', as.integer(0))
  if (!ud.have.unit.system()) {
    ## Failing that, override it with the in-package XML file
    p0 <- system.file("share/udunits2.xml", package="udunits2")
    Sys.setenv(UDUNITS2_XML_PATH=p0)
    .C('R_ut_init', as.integer(1))
    ## If *that* fails, give the user some instructions for how to remedy
    ## the problem
    if (!ud.have.unit.system()) {
      warning(
	  "Failed to read udunits system database: udunits2 will not work properly.\nPlease set the UDUNITS2_XML_PATH environment variable before attempting to load the package")
    }
  }
}

.onAttach <- function(libname, pkgname) {
    msg <- "udunits system database read"
    p0 <- Sys.getenv("UDUNITS2_XML_PATH")
    if (p0 != "") {
        msg <- paste(msg, "from", p0)
    } 
    packageStartupMessage(msg)
}

ud.are.convertible <-
function(u1, u2) {
  if (! (ud.is.parseable(u1) && ud.is.parseable(u2))) {
    return(FALSE)
  }
  rv <- .C('R_ut_are_convertible',
           as.character(u1),
           as.character(u2),
           convertible=logical(1))
  return(rv$convertible)
}

ud.convert <-
function(x, u1, u2) {
  if (! ud.are.convertible(u1, u2)) {
    stop(paste("Units", u1, "and", u2, "are not convertible"))
  }
  ## Filter out NA's before passing them to the C function
  ## since it can't handle them
  rv <- rep(NA, length(x))
  i <- which(! is.na(x))

  len <- length(i)
  c.rv <- .C('R_ut_convert',
           as.double(x)[i],
           as.integer(len),
           as.character(u1),
           as.character(u2),
           converted=double(len)
           )
  rv[i] <- c.rv$converted
  ## If it's a matrix/vector or anything else, convert it back to it's original type
  attributes(rv) <- attributes(x)
  return(rv)
}

ud.get.name <-
function(unit.string) {
  stopifnot(ud.is.parseable(unit.string))
  rv <- .C('R_ut_get_name',
           as.character(unit.string),
           ud.name=character(length=1))
  return(rv$ud.name)
}

ud.get.symbol <-
function(unit.string) {
  stopifnot(ud.is.parseable(unit.string))
  rv <- .C('R_ut_get_symbol',
           as.character(unit.string),
           ud.symbol=character(length=1))
  return(rv$ud.symbol)
}

ud.is.parseable <-
function(unit.string) {
  rv <- .C('R_ut_is_parseable',
           as.character(unit.string),
           parseable=logical(1))
  return(rv$parseable)
}

ud.set.encoding <-
function(enc.string) {
  .C('R_ut_set_encoding',
     as.character(enc.string))
  return()
}

ud.have.unit.system <-
function() {
  rv <- .C('R_ut_has_system',
           exists=logical(1))
  return(rv$exists)
}
