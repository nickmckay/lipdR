#' Query Table for LiPDverse datasets
#'
#' A table of metadata describing the time series available in LiPDverse
#'
#' @format ## `queryTable`
#' A data frame with 98,054 rows and 16 columns:
#' \describe{
#'   \item{archiveType}{Country name}
#'   \item{datasetId}{Lipdverse dataset Id}
#'   \item{dataSetName}{dataset name}
#'   \item{earliestYear}{first year of record}
#'   \item{geo_latitude}{latitude}
#'   \item{geo_longitude}{longitude}
#'   \item{interpretation1_seasonality}{Interpreted seasonality of proxy}
#'   \item{lipdverseLink}{LiPDverse URL}
#'   \item{mostRecentYear}{Last year of record}
#'   \item{paleoData_mostRecentCompilations}{Most recent compilations}
#'   \item{paleoData_variableName}{paleo-data variable name}
#'   \item{country2}{Country name}
#'   \item{continent}{Continent name}
#'   \item{auth}{Publication info}
#'   \item{interp_Vars}{Paleodata interpretation variable}
#'   \item{interp_Details}{Paleodata interpretation detail}
#' }
#' @source <https://github.com/DaveEdge1/lipdverseQuery/raw/main/queryZip.zip>
"queryTable"
