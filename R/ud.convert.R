ud.convert <-
function(x, u1, u2) {
  if (! ud.are.convertible(u1, u2)) {
    stop(paste("Units", u1, "and", u2, "are not convertible"))
  }
  len <- length(x)
  rv <- .C('R_ut_convert',
           as.double(x),
           as.integer(len),
           as.character(u1),
           as.character(u2),
           converted=double(len),
           PACKAGE='udunits2'
           )
  rv <- rv$converted
  # If it's a matrix/vector or anything else, convert it back to it's original type
  attributes(rv) <- attributes(x)
  return(rv)
}
