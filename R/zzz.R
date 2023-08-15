#
#This function should be called each time the package is loaded

.onLoad <- function(libname, pkgname){
  .lipdRquery <<- new.env()
  .lipdRquery$queryUpdated <- 0
  updateStandardTables()
}
#
# #Update the query table
#
# update_queryTable <- function(){
#
#   #download queryTable
#   queryTable <- newQueryTable()
#   #Replace local copy
#   usethis::use_data(queryTable, overwrite = TRUE, compress = "xz")
# }
#
# newQueryTable <- function(){
#   query_url <- "https://github.com/DaveEdge1/lipdverseQuery/raw/main/queryZip.zip"
#   temp <- tempdir()
#   zip_dir <- paste0(temp, "/queryTable.zip")
#   download.file(query_url, zip_dir)
#   unzip(zip_dir, exdir = temp)
#   fPth <- paste0(temp, "/queryTable.csv")
#   queryTable <<- read.csv(fPth)
#   # unlink(temp)
#   #assign("queryTable", queryTable, envir = lipdEnv)
#   return(queryTable)
# }
