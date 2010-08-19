ud.are.convertible <-
function(u1, u2) {
  if (! (ud.is.parseable(u1) && ud.is.parseable(u2))) {
    return(FALSE)
  }
  rv <- .C('R_ut_are_convertible',
           as.character(u1),
           as.character(u2),
           convertible=logical(1),
           PACKAGE='udunits2')
  return(rv$convertible)
}
