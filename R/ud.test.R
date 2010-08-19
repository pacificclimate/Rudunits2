ud.test <-
function() {
  .C('test', PACKAGE='udunits2')
  return("test")
}

