#' Create a data table from a lipd-ts-tibble object
#'
#' @param ts Any lipd class
#' @param time.var What's the name of the time var (default = "age")
#' @importFrom dplyr select 
#' @importFrom tidyr pivot_wider unchop
#' @importFrom magrittr `%>%`
#' @return
#' @export
#'
#' @examples
tabulateTs <- function(ts,time.var = "age"){
  ts <- as.lipdTsTibble(ts)
  out <- dplyr::select(ts,{{time.var}},paleoData_variableName,paleoData_values) %>%
    tidyr::unchop(c(time.var,"paleoData_values")) %>%
    tidyr::pivot_wider(id_cols = {{time.var}},names_from = paleoData_variableName, values_from = paleoData_values)
  
  return(out)
  
}