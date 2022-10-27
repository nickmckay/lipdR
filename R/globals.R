#' random imports
#'
#' @param x does nothing
#'
#' @importFrom stats median quantile
#' @importFrom utils askYesNo packageVersion
#'
abc123 <- function(x){
  median(x)
  quantile(x)
  askYesNo(x)
  packageVersion(x)
}

utils::globalVariables(c("depth", "agelimityounger", "agelimitolder", "chroncontrolage", "chroncontroltype", "uncertainty",
                         "chroncontrolid", "thickness", "age", "everything", "collunitid", "paleoData_variableName", "paleoData_values",
                         "queryTable"))
