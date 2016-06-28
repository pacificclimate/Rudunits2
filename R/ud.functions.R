.onLoad <- function(libname, pkgname) {
  if (!(ud.is.parseable("m"))) {
    p0 = system.file("share/udunits2.xml", package="udunits2")
    .C(R_ut_reinit, as.character(p0))
    if (ud.is.parseable("m"))
      packageStartupMessage("udunits2 system database not loaded; reading shipped version from ", p0)
    else
      packageStartupMessage("failed to load system file ", p0, " : udunits2 will not work properly")
  }
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
