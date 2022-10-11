

#' download a multilipd from lipdverse based on query parameters
#'
#' @param fuzzy 
#' @param variable.name 
#' @param archive.type 
#' @param longitude 
#' @param age.min 
#' @param age.max 
#' @param pub.info 
#' @param country 
#' @param ocean 
#' @param interpretation 
#' 
#' @importFrom tidyr %>%
#' @importFrom dplyr filter
#'
#' @return
#' @export
#'
#' @examples
queryGet <- function(variable.name = NULL, #vector of variable names from c(d18O, d13C, treeRing, )
                     archive.type = NULL, #vector of archive types. see unique(queryTable1$archiveType)
                     coord = c(-90,90,-180,180), #lat/lon extent of interest c(latMin, latMax, lonMin, lonMax)
                     age.min = NULL, #Sampling covers at least this recent extent, age BP
                     age.max = NULL, #Sampling covers at least this old extent, age BP
                     pub.info = NULL, #last name of author, DOI, unique word from title, etc. all lowercase, based on associated publications
                     country = NULL, #Coutnry origin of dataset from unique(queryTable1$country2), based on lat/lon
                     continent = NULL, #Continent origin of dataset from unique(queryTable1$continent), based on lat/lon
                     ocean = FALSE, #Gather datasets from the marine environment, based on lat/lon
                     interpretation = NULL #vector of interpretations from c(temp, precip, SST)
                     ){

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  #get remote query table
  # print("Fetching the query table")
  # query_url <- "https://github.com/DaveEdge1/lipdverseQuery/raw/main/queryZip.zip"
  # temp <- tempfile()
  # download.file(query_url, temp)
  # filePath <- unzip(temp, list = TRUE)$Name
  # unzip(temp, filePath)
  # queryTable1 <- read.csv(filePath)
  # unlink(temp)
  
  #get local query table
  # filePath <- unzip(temp, list = TRUE)$Name
  # unzip(temp, filePath)
  # queryTable1 <- read.csv(filePath)
  #get('queryTable', envir=lipdEnv)
  
  queryTable1 <- lipdEnv$queryTable
  
  print(dim(queryTable1), "\n\n")

  #Filter by user coordinates
  queryTable1 <- queryTable1[queryTable1$geo_latitude >= coord[1],]
  queryTable1 <- queryTable1[queryTable1$geo_latitude <= coord[2],]
  queryTable1 <- queryTable1[queryTable1$geo_longitude >= coord[3],]
  queryTable1 <- queryTable1[queryTable1$geo_longitude <= coord[4],]
  
  cat("Series remaining after coord filter: ", nrow(queryTable1), "\n\n")
  
  #Marine samples?
  if(ocean==TRUE){
    if (!is.null(country) | !is.null(continent)){
      stop("Cannot process ocean=TRUE alongside continent/country inputs. 
           Use only coord input if both marine and terrestrial datasets are desired.")
    }
    queryTable1 <- queryTable1[is.na(queryTable1$country2)==TRUE,]
  }
  
  cat("Series remaining after marine filter: ", nrow(queryTable1), "\n\n")
  
  #Filter by continent
  if (!is.null(continent)){
    queryTable1 <- queryTable1[queryTable1$continent == continent,]
  }
  cat("Series remaining after continent filter: ", nrow(queryTable1), "\n\n")

  #Filter by country
  if (!is.null(country)){
    queryTable1 <- queryTable1[queryTable1$country2 == country,]
    queryTable1 <- queryTable1[!is.na(queryTable1$country2),]
  }
  
  cat("Series remaining after country filter: ", nrow(queryTable1), "\n\n")
  
  #Filter by time
  if (!is.null(age.max)){
    queryTable1 <- queryTable1[queryTable1$earliestYear < age.max,]
  }
  if (!is.null(age.min)){
    queryTable1 <- queryTable1[queryTable1$mostRecentYear > age.min,]
  }
  
  cat("Series remaining after time filter: ", nrow(queryTable1), "\n\n")
  
  #Filter by archive.type
  if(!is.null(archive.type)){
    for (i in 1:length(archive.type)){
      queryTable1 <- queryTable1[queryTable1$archiveType %in% archive.type[i],]
    }
  }
  
  cat("Series remaining after archive.type filter: ", nrow(queryTable1), "\n\n")
  
  #Filter for desired variable names
  if (!is.null(variable.name)){
    for (i in variable.name){
      queryTable1 <- queryTable1[queryTable1$varTags %in% i,]
    }
  } 
  
  cat("Series remaining after variable.name filter: ", nrow(queryTable1), "\n\n")
  
  
  #pub.info
  if (!is.null(pub.info)){
    authorRows <- grep(pub.info, tolower(queryTable1$auth))
    
    queryTable1 <- queryTable1[authorRows,]
  }
  
  cat("Series remaining after pub.info filter: ", nrow(queryTable1), "\n\n")
  

  
  
  #Final tally and check-in
  cat("Based on your query parameters, there are", nrow(queryTable1), "available time series in", length(unique(queryTable1$dataSetName)), "datasets\n")
  if(nrow(queryTable1) < 1){
    stop_quietly()
  }
  
  ans1 <- askYesNo("Would you like to download them?")
  if(ans1==FALSE){
    message("Exiting without data")
    stop_quietly()
  }
  
  
  #download and create a multilipd
  URLs <- unique(queryTable1$lipdverseLink)
  URLs <- URLs[!is.na(URLs)]
  URLs <- gsub(".html", ".lpd", URLs)
  
  D <- list()
  for (i in 1:length(URLs)){
    D[[i]] <- readLipd(URLs[i])
  }
  
  D <- lipdR:::new_multiLipd(D)
  
  print("\n")
  
  print(summary(D))
  
  invisible(D)
}


