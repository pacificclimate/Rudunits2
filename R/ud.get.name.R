ud.get.name <-
function(unit.string) {
  stopifnot(ud.is.parseable(unit.string))
  rv <- .C('R_ut_get_name',
           as.character(unit.string),
           ud.name=character(length=1),
           PACKAGE='udunits2')
  return(rv$ud.name)
}
