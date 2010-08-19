ud.get.symbol <-
function(unit.string) {
  stopifnot(ud.is.parseable(unit.string))
  rv <- .C('R_ut_get_symbol',
           as.character(unit.string),
           ud.symbol=character(length=1),
           PACKAGE='udunits2')
  return(rv$ud.symbol)
}
