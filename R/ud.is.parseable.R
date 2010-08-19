ud.is.parseable <-
function(unit.string) {
  rv <- .C('R_ut_is_parseable',
           as.character(unit.string),
           parseable=logical(1),
           PACKAGE='udunits2')
  return(rv$parseable)
}
