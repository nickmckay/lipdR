#
#This function should be called each time the package is loaded

.onLoad <- function(libname, pkgname){
  updateStandardTables()
}
#

