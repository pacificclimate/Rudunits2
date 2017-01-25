.onLoad <- function(libname, pkgname) {
  ## By default, configure udunits with path set (presumably) by the
  ## user through the UDUNITS2_XML_PATH environment variable
  .C('R_ut_init', as.character(''), as.character(''), as.integer(0))
  if (!ud.have.unit.system("default")) {
    ## Failing that, override it with the in-package XML file
    p0 <- system.file("share/udunits2.xml", package="udunits2")
    Sys.setenv(UDUNITS2_XML_PATH=p0)
    .C('R_ut_init',as.character(''), as.character(''), as.integer(1))
    ## If *that* fails, give the user some instructions for how to remedy
    ## the problem
    if (!ud.have.unit.system("default")) {
      warning(
	  "Failed to read udunits system database: udunits2 will not work properly.\nPlease set the UDUNITS2_XML_PATH environment variable before attempting to load the package")
    }
  }
}

.onAttach <- function(libname, pkgname) {
    msg <- "udunits system database read"
    p0 <- Sys.getenv("UDUNITS2_XML_PATH")
    if (p0 != "") {
      .C('R_ut_init', as.character(''), as.character(''), as.integer(0))
        msg <- paste(msg, "from", p0)
    }
    packageStartupMessage(msg)
}

.onUnload <- function(libpath){
  .C('R_ut_system_cleanup') #To delete all the created systems.
}


ud.are.convertible <-
function(u1, u2, system.name = "default") {
  if (! (ud.is.parseable(u1, system.name) && ud.is.parseable(u2, system.name))) {
    return(FALSE)
  }
  rv <- .C('R_ut_are_convertible',
           as.character(u1),
           as.character(u2),
           convertible=logical(1),
           as.character(system.name))
  return(rv$convertible)
}

ud.convert <-
function(x, u1, u2, system.name = "default") {
  if (! ud.are.convertible(u1, u2, system.name)) {
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
           converted=double(len),
           as.character(system.name)
           )
  rv[i] <- c.rv$converted
  ## If it's a matrix/vector or anything else, convert it back to it's original type
  attributes(rv) <- attributes(x)
  return(rv)
}

ud.get.name <-
function(unit.string, system.name = "default") {
  stopifnot(ud.is.parseable(unit.string, system.name))
  rv <- .C('R_ut_get_name',
           as.character(unit.string),
           ud.name=character(length=1),
           as.character(system.name))
  return(rv$ud.name)
}

ud.get.symbol <-
function(unit.string, system.name = "default") {
  stopifnot(ud.is.parseable(unit.string, system.name))
  rv <- .C('R_ut_get_symbol',
           as.character(unit.string),
           ud.symbol=character(length=1),
           as.character(system.name))
  return(rv$ud.symbol)
}

ud.is.parseable <-
function(unit.string, system.name = "default") {
  rv <- .C('R_ut_is_parseable',
           as.character(unit.string),
           parseable=logical(1),
           as.character(system.name))
  return(rv$parseable)
}

ud.set.encoding <-
function(enc.string, system.name = "default") {
  .C('R_ut_set_encoding',
     as.character(enc.string),
     as.character(system.name))
  return()
}

ud.have.unit.system <-
function(system.name) {
  rv <- .C('R_ut_has_system',
           exists=logical(1),
           as.character(system.name))
  return(rv$exists)
}

ud.free.system <- 
function(system.name){
  rv <- .C('R_ut_free_system',
           as.character(system.name),
           deleted = logical(1))
  return(rv$deleted)
}

ud.add.system <- 
  function(file.name, system.name){
    rv <- .C('R_ut_init', 
             as.character(file.name),
             as.character(system.name),
             as.integer(1))
  }


ud.list.systems <-
  function(){
    rv <- .C('R_ut_system_count',
             count = integer(1))
    if(rv$count != 0){
      rv <- .C('R_ut_list_systems',
               systems = character(rv$count))

      return(rv$systems)
    }else{
      return(NULL)
    }

  }
