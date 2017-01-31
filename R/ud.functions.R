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

#Function to check if a system is loaded or not
#Returns true if the system specified by system.name is present, otherwise returns false.
ud.have.unit.system <-
function(system.name) {
  rv <- .C('R_ut_has_system',
           exists=logical(1),
           as.character(system.name))
  return(rv$exists)
}

#Function to delete the system given by the user
#Returns true is system is successfully deleted, otherwise returns an error.
ud.free.system <- 
function(system.name){
  rv <- .C('R_ut_free_system',
           as.character(system.name),
           deleted = logical(1))
  return(rv$deleted)
}

#Function to add a system, if file.name is blank, the default xml is used, 
#if system.name is null the the name of the system is set to "default" 
ud.add.system <- 
  function(file.name, system.name){
    rv <- .C('R_ut_init', 
             as.character(file.name),
             as.character(system.name),
             as.integer(1))
  }

#Function to list the different systems
#Returns a character vector containing the system names
#If no systems have been loaded it returns NULL
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

#Function to set the conversion rule
#The function.name is set to scale by default, other functions allowed are offset,log and invert.
#Returns true if conversion setting is successful otherwise returns an error
ud.set.conversion <- 
  function(unit1.text, unit2.text, system.name = "default", function.name = "scale"){
    units <- extract.unit.parts(c(unit1.text, unit2.text))
    rv <- .C('R_ut_set_conversion', 
             as.character(units$text[1]),
             as.character(units$text[2]),
             as.double(units$value[1]),
             as.double(units$value[2]),
             as.character(system.name),
             as.character(function.name),
             set = logical(1)
    )
    return(rv$set) 
  }

#Function to remove a unit from a loaded system
#Returns true if the unit is successfully removed otherwise returns an error
ud.remove.unit <- 
  function(unit.name, system.name = "default"){
    rv <- .C("R_ut_remove_unit",
             as.character(unit.name),
             removed = logical(1),
             as.character(system.name))
    return(rv$removed)
  }

#Function to extract the text and vqalue part from the unit text
#This function is called from ud.set.conversion
extract.unit.parts <- function(unitdef, assumed.value = 1) {
  if (is.factor(unitdef)) {
    unitdef <- as.character(unitdef)
  }
  if (!is.character(unitdef)) {
    stop("unitdef must be a character scalar or vector")
  }
  pattern.unit <- "^[[:blank:]]*([[:digit:]]*\\.?[[:digit:]]+)?[[:blank:]]*([[:alnum:]_]+)[[:blank:]]*$"
  mask.unit <- grepl(pattern.unit, unitdef, perl=TRUE)
  if (any(!mask.unit)) {
    stop("Invalid unit: ", paste0('"', unitdef[!mask.unit], '"', collapse=", "), ". ",
         "Units must follow either the pattern (nonnegative number, optional spaces, alphanumeric or underscore string) or (alphanumeric or underscore string).")
  }
  unit.value <- as.numeric(gsub(pattern.unit, "\\1", unitdef))
  unit.value[is.na(unit.value)] <- assumed.value
  unit.text <- gsub(pattern.unit, "\\2", unitdef)
  list(value=unit.value,
       text=unit.text)
}

