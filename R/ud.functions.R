#' @useDynLib udunits2 R_ut_init
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

#' @useDynLib udunits2 R_ut_init
.onAttach <- function(libname, pkgname) {
    msg <- "udunits system database read"
    p0 <- Sys.getenv("UDUNITS2_XML_PATH")
    if (p0 != "") {
      .C('R_ut_init', as.character(''), as.character(''), as.integer(0))
        msg <- paste(msg, "from", p0)
    }
    packageStartupMessage(msg)
}

#' @useDynLib udunits2 R_ut_system_cleanup
.onUnload <- function(libpath){
  # .C('R_ut_system_cleanup') #To delete all the created systems.
  library.dynam.unload("udunits2", libpath)
}

#' @title Determine whether two units may be converted between each
#'   other
#'
#' @description This function takes udunits compatible strings and
#'   determines whether or not it is possible to convert between them.
#'
#' @param u1 A character string which is parseable into a udunits 
#'   compatible unit.
#' @param u2 Another character string which is also parseable into a 
#'   udunits compatible unit.
#' @param system.name The unit system name to use.
#' @details Even if two units are parseable and recognized by the 
#'   udunits library, it may or may not be possible to convert from one 
#'   to another.  For example, it makes sense to convert from celsius to
#'   kelvin, however not from celsius to kilograms.  This function 
#'   allows the user to check if two units are of the same system and if
#'   there exists a defined conversion between the two.
#' @return  Returns a logical: \code{TRUE} if the units can be converted
#'   between each other, \code{FALSE} if either of the arguments is not 
#'   parseable by udunits, or if no conversion is possible.
#' @references   See the udunits function ut_are_convertible: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#ut_005fare_005fconvertible_0028_0029}
#'    and the main uninits webpage: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/}
#' @author James Hiebert \email{hiebert@uvic.ca}
#' @seealso \code{\link{ud.is.parseable}}
#' @examples
#' ud.are.convertible("miles", "km")        # TRUE
#' ud.are.convertible("grams", "kilograms") # TRUE
#' ud.are.convertible("celsius", "grams")   # FALSE
#' ud.are.convertible("not", "parseable")   # FALSE
#' @export
#' @useDynLib udunits2 R_ut_are_convertible
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

#' @title Convert numeric types from one unit to another
#' @description This function takes the numeric argument \code{x}, 
#'   quantified in units \code{u1} and converts it to be of units 
#'   \code{u2}.
#' @param x Some argument which is convertible to a numeric type by 
#'   \code{as.double}.
#' @param u1 A character string which is parseable into a udunits 
#'   compatible unit.
#' @param u2 Another character string which is also parseable into a 
#'   udunits compatible unit and for which there exists a defined 
#'   transformation from the units represented by u1.
#' @param system.name The unit system name to use.
#' @details This function uses the udunits function 
#'   \code{cv_convert_doubles} to convert the argument from one set of 
#'   units to another.
#' @return Returns a numeric type having converted from one unit to 
#'   another.  The attributes of the original argument \code{x} (e.g. 
#'   class, dimensions, etc.) are preserved and then re-applied to the 
#'   return value of the transformation as such:
#'   
#'   \code{attributes(rv) <- attributes(x)}
#'   
#'   If either of unit \code{u1} or \code{u2} is unparseable, or there 
#'   does not exist a conversion from one to the other the function 
#'   raises an error.
#' @references Unidata's udunits reference:
#' \url{http://www.unidata.ucar.edu/software/udunits/}
#' 
#' API guide for cv_convert_doubles:
#' \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-cv_005fconvert_005fdoubles-39}
#' @author James Hiebert \email{hiebert@uvic.ca}
#' @seealso \code{\link{ud.are.convertible}}
#' @examples
#' x <- seq(10)
#' ud.convert(x, "miles", "km")                   # c(1.609344, 3.218688, 4.828032, ...)
#' x <- c(-40, 0, 100)
#' ud.convert(x, "celsius", "degree_fahrenheit")  # c(-40, 32, 212)
#' err <- try(ud.convert(100,"miles", "grams"))   # Error
#' err <- try(ud.convert(NA, "not", "parseable")) # Error
#' @export
#' @useDynLib udunits2 R_ut_convert
ud.convert <-
function(x, u1, u2, system.name="default") {
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
             as.character(system.name))
  rv[i] <- c.rv$converted
  ## If it's a matrix/vector or anything else, convert it back to it's original type
  attributes(rv) <- attributes(x)
  return(rv)
}

#' @title Retrieve the udunits name or symbol from the database for a 
#'   given units string
#' @description Retrieve the udunits name or symbol from the database 
#'   for a given units string.
#' @param unit.string A character string which is parseable into a 
#'   udunits compatible unit.
#' @param system.name The unit system name to use.
#' @details This function retrieves the udunits name or symbol from the 
#'   udunits database and returns it.  It uses the udunits functions 
#'   ut_get_name and ut_get_symbol respectively.
#' @return Returns a character string stating the udunits's name/symbol 
#'   for the given unit, or an empty character string if the unit does 
#'   not map to a name/symbol for the default character set. If the unit
#'   is unparseable, the function raises an error.
#' @references Unidata's udunits reference: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/}
#'   
#'   API guide for ut_get_name: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-ut_005fget_005fname-66}
#'   
#'   
#'   API guide for ut_get_symbol: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-ut_005fget_005fsymbol-67}
#'   
#' @author James Hiebert \email{hiebert@uvic.ca}
#' @note   More often than not units do not have names or symbols that
#'   are returned by the base functions.  This depends entirely on what
#'   is defined in the units data base, which is--as of API version
#'   2--an XML database which ships with the library.  See Unidata's
#'   website for more information about the XML database:
#'   \url{http://www.unidata.ucar.edu/software/udunits/udunits-2-units.html}.
#'   All in all, don't put too much stock in them, for they are for
#'   convenience only. If your application \emph{requires} certain names
#'   and symbols to be present, the XML database is local and editable.
#' @examples 
#' units.to.display <- c("celsius", # has no name, messed up symbol (maybe a bug in R?)
#'                       "kg",
#'                       "hr",      # has no symbol
#'                       "K",
#'                       "degrees",
#'                       "m",
#'                       "ohm")
#' for (u in units.to.display) {
#'   print(ud.get.name(u))
#'   print(ud.get.symbol(u))
#' }
#' @export
#' @useDynLib udunits2 R_ut_get_name
ud.get.name <-
function(unit.string, system.name="default") {
  stopifnot(ud.is.parseable(unit.string, system.name))
  rv <- .C('R_ut_get_name',
           as.character(unit.string),
           ud.name=character(length=1),
           as.character(system.name))
  return(rv$ud.name)
}

#' @rdname ud.get.name
#' @export
#' @useDynLib udunits2 R_ut_get_symbol
ud.get.symbol <-
function(unit.string, system.name="default") {
  stopifnot(ud.is.parseable(unit.string, system.name))
  rv <- .C('R_ut_get_symbol',
           as.character(unit.string),
           ud.symbol=character(length=1),
           as.character(system.name))
  return(rv$ud.symbol)
}

#' @title Determine whether a unit string is parseable by the udunits 
#'   library
#' @description Determine whether a unit string is parseable and 
#'   recognized by the udunits library.
#' @param unit.string A character string representing a type of units 
#'   which may be parseable by the udunits library.
#' @param system.name The unit system name to use.
#' @details \code{ud.is.parseable} uses udunit's function 
#'   \code{ut_parse} to determine whether or not the given unit string 
#'   is parseable.  If \code{ut_parse} returns NULL, then 
#'   \code{ud.is.parseable} will return \code{FALSE}.
#' @return Returns a logical: \code{TRUE} if the units is parseable and 
#'   recognized by the udunits library, \code{FALSE} otherwise.
#' @references Unidata's udunits reference: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/}
#'   
#'   API guide for ut_parse: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#index-ut_005fparse-43}
#'   
#' @author James Hiebert \email{hiebert@uvic.ca}
#' @note There is a note in the \code{ut_parse} docs about how the
#'   argument string must have no leading or trailing whitespace.  We
#'   make sure in this package to always call \code{ut_trim} on any
#'   strings before they are passed to \code{ut_parse}.  The package
#'   user need not strip whitespace before-hand.
#' @seealso \code{\link{ud.are.convertible}}
#' @examples 
#' ud.is.parseable("K")             # TRUE
#' ud.is.parseable("  K  ")         # TRUE
#' ud.is.parseable("miles")         # TRUE
#' ud.is.parseable("Not parseable") # FALSE
#' @export
#' @useDynLib R_ut_is_parseable
ud.is.parseable <-
function(unit.string, system.name = "default") {
  rv <- .C('R_ut_is_parseable',
           as.character(unit.string),
           parseable=logical(1),
           as.character(system.name))
  return(rv$parseable)
}

#' @title Set the udunits package level encoding type
#' @description Sets the encoding type parameter for the current 
#'   \code{system.name}.
#' @param enc.string A character string representing the encoding type. 
#'   Valid strings are \code{utf8},\code{ascii},\code{iso-8859-1},and 
#'   \code{latin1} (an alias for ISO-8859-1).
#' @param system.name The unit system name to use.
#' @details Encoding type is a parameter to nearly all of the functions 
#'   in the udunits library.  By default, the R udunits2 pacakge sets 
#'   the encoding type to UTF-8, however this package allows the user to
#'   set other encoding types which are supported by the udunits 
#'   library.  It presently suports UTF-8, ASCII, and ISO-8859-1.
#' @return Returns no value.  Raises an error if it is not given a valid
#'   encoding string.
#' @references Unidata's udunits reference: 
#'   \url{http://www.unidata.ucar.edu/software/udunits/}
#'   
#'   API guide chapter on data types: 
#'   \url{https://www.unidata.ucar.edu/software/udunits/udunits-2.1.24/udunits2lib.html#Types}
#' @author James Hiebert \email{hiebert@uvic.ca}
#' @examples
#' valid.enc.strings <- c('utf8', 'ascii', 'iso-8859-1', 'latin1')
#' lapply(valid.enc.strings, ud.set.encoding)
#' err <- try(ud.set.encoding("This will fail"))
#' @export
#' @useDynLib udunits2 R_ut_set_encoding
ud.set.encoding <-
function(enc.string, system.name="default") {
  .C('R_ut_set_encoding',
     as.character(enc.string),
     as.character(system.name))
  return()
}

#' Check if a system is loaded.
#' @param system.name The unit system name to check.
#' @return \code{TRUE} if the system specified by system.name is
#'   present, otherwise \code{FALSE}.
#' @seealso \code{\link{ud.add.system}}
#' @examples
#' ud.have.unit.system("default") # TRUE
#' ud.have.unit.system("notloaded") # FALSE
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @export
#' @useDynLib udunits2 R_ut_has_system
ud.have.unit.system <-
function(system.name) {
  rv <- .C('R_ut_has_system',
           exists=logical(1),
           as.character(system.name))
  return(rv$exists)
}

#' Delete the system given by the user
#' @param system.name The unit system name to use.
#' @return \code{TRUE} if system.name is successfully deleted, otherwise
#'   returns an error.
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @export
#' @useDynLib udunits2 R_ut_free_system
ud.free.system <-
function(system.name){
  rv <- .C('R_ut_free_system',
           as.character(system.name),
           deleted = logical(1))
  return(rv$deleted)
}

#' Add a unit system
#' @param file.name The XML file to read for unit information.  If 
#'   \code{file.name} is blank, the system default xml is used.
#' @param system.name The unit system name to use.  If system.name is
#'   null the the name of the system is set to "default".
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @seealso \code{\link{ud.list.systems}}, \code{\link{ud.free.system}}
#' @export
#' @useDynLib udunits2 R_ut_init
ud.add.system <- function(file.name, system.name){
  rv <- .C('R_ut_init', 
           as.character(file.name),
           as.character(system.name),
           as.integer(1))
}

#' List the loaded systems
#' @return A character vector containing the system names.  If no 
#'   systems have been loaded it returns \code{NULL}.
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @seealso \code{\link{ud.add.system}}, \code{\link{ud.free.system}}
#' @export
#' @useDynLib udunits2 R_ut_system_count R_ut_list_systems
ud.list.systems <- function(){
  rv <- .C('R_ut_system_count',
           count = integer(1))
  if(rv$count != 0){
    rv <- .C('R_ut_list_systems',
             systems = character(rv$count))
    
    return(rv$systems)
  } else {
    return(NULL)
  }
}

#' Set a conversion rule between two units
#' @param unit1.text,unit2.text The text to define the units.  The text 
#'   may take the form of "optional blank, optional floating point 
#'   nonnegative number, optional blank, unit text, optional blank". The
#'   blanks are ignored; if the floating point number is not given, then
#'   it is assumed to be \code{1}.
#' @param system.name The unit system name to use.
#' @param function.name The function to use to convert between 
#'   \code{unit1.text} and \code{unit2.text}.  See the details for more 
#'   information.
#' @details The functions are applied so that if the units are given as
#'   \code{unit1.text = "a x"} and \code{unit2.text = "b y"} where "a"
#'   and "b" are numbers:
#'   \itemize{
#'     \item{scale}{x = b/a * y}
#'     \item{offset}{x = (b - a) * y}
#'     \item{log}{x = log(a/b)(y); log(a/b) is log base a/b}
#'     \item{invert}{x = 1/y}
#'   }
#'
#'   If both unit text names are not defined, then both are created and
#'   they are mapped to each other.  If one is not defined, it is created
#'   and mapped to the other.  If both are defined, then an error is raised.
#' @return \code{TRUE} if conversion setting is successful, otherwise 
#'   returns an error.
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @examples
#' \dontrun{
#' ud.set.conversion("1 moleaspirin", "180.157 gram") # Convert grams to moles of aspirin
#' }
#' @export
#' @useDynLib udunits2 R_ut_set_conversion
ud.set.conversion <- function(unit1.text, unit2.text, system.name="default",
                              function.name=c("scale", "offset", "log", "invert")) {
  function.name <- match.arg(function.name)
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

#' Remove a unit from a loaded system
#' @param unit.name The unit name to remove from a system
#' @param system.name The unit system name to use.
#' @return \code{TRUE} if the unit is successfully removed, otherwise
#'   returns an error.
#' @author Swechhya Bista \email{Swechhya.Bista@cytel.com}
#' @examples
#' \dontrun{
#' ud.remove.unit("miles")
#' }
#' @export
#' @useDynLib udunits2 R_ut_remove_unit
ud.remove.unit <- function(unit.name, system.name="default"){
  rv <- .C("R_ut_remove_unit",
           as.character(unit.name),
           removed = logical(1),
           as.character(system.name))
  return(rv$removed)
}

#' Extract the text and value part from the unit text
#' @param unitdef A unit definition in the form of "optional blank, 
#'   optional floating point nonnegative number, optional blank, unit 
#'   text, optional blank".  Blanks (e.g. spaces, tabs, etc.) are 
#'   ignored.
#' @param assumed.value The value to assume if the optional floating 
#'   point nonnegative number is not given in the \code{unitdef}.
#' @return A list with two elements:  \code{value} and \code{text}.
#' @note This function is called from ud.set.conversion
#' @author William Denney \email{wdenney@humanpredictions.com}
extract.unit.parts <- function(unitdef, assumed.value=1) {
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
