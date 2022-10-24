

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
#' @return
#' @export
queryGet <- function(variable.name = NULL, #vector of variable names from c(d18O, d13C, treeRing, )
                     archive.type = NULL, #vector of archive types. see unique(queryTable1$archiveType)
                     coord = c(-90,90,-180,180), #lat/lon extent of interest c(latMin, latMax, lonMin, lonMax)
                     age.min = NULL, #Sampling covers at least this recent extent, age BP
                     age.max = NULL, #Sampling covers at least this old extent, age BP
                     pub.info = NULL, #last name of author, DOI, unique word from title, etc. all lowercase, based on associated publications
                     country = NULL, #Coutnry origin of dataset from unique(queryTable1$country2), based on lat/lon
                     continent = NULL, #Continent origin of dataset from unique(queryTable1$continent), based on lat/lon
                     ocean = FALSE, #Gather datasets from the marine environment, based on lat/lon
                     seasonality = NULL, #list of seasons where items within a list are treated with "AND" logic and
                     #separate lists are treated with "OR" logic ie. list(list("July", "August"), list("7,8"), list("summer"))
                     season.not = NULL, #seasons not desired with input format identical to seasonality
                     interp.vars = NULL, #vector of interpretation variables ie. c("SST", "upwelling"), see possible: unique(queryTable$interp_Vars)
                     interp.details = NULL, #vector of interpretation variables ie. c("sea@surface", "elNino"), see possible: unique(queryTable$interp_Details),
                     interactive.feedback = TRUE #offer details of filters and get feedback from user
                     ){

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  queryTable1 <- queryTable
  # 
  # print(dim(queryTable1), "\n\n")

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
    queryTable1 <- queryTable1[queryTable1$country2 %in% country,]
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
  if (!is.null(archive.type)){
    archiveTypeIndex <- c()
    for (i in archive.type){
      archiveTypeIndex <- c(archiveTypeIndex, which(grepl(tolower(i), tolower(queryTable1$archiveType))))
    }
    queryTable1 <- queryTable1[archiveTypeIndex,]
  }

  cat("Series remaining after archive.type filter: ", nrow(queryTable1), "\n\n")
  
  #Filter for desired variable names
  if (!is.null(variable.name)){
    varNameInex <- c()
    for (i in variable.name){
      varNameInex <- c(varNameInex, which(grepl(tolower(i), tolower(queryTable1$paleoData_variableName))))
    }
    queryTable1 <- queryTable1[varNameInex,]
  } 
  
  cat("Series remaining after variable.name filter: ", nrow(queryTable1), "\n\n")
  
  #Filter for desired interpretation variables
  if (!is.null(interp.vars)){
    interpVarInex <- c()
    for (i in interp.vars){
      interpVarInex <- c(interpVarInex, which(grepl(i, queryTable1$interp_Vars)))
    }
    if(!is.null(interp.details)){
      queryTable2 <- queryTable1[interpVarInex,]
    }else{
      queryTable1 <- queryTable1[interpVarInex,]
    }
  } 
  
  cat("Series remaining after interp.vars filter: ", nrow(queryTable1), "\n\n")
  
  #Filter for desired interpretation details
  if (!is.null(interp.details)){
    interpDetailsIndex <- c()
    for (i in interp.details){
      interpDetailsIndex <- c(interpDetailsIndex, which(grepl(i, queryTable1$interp_Details)))
    }
    if(!is.null(interp.vars)){
      queryTable1 <- queryTable1[interpDetailsIndex,]
      queryTable1 <- rbind(queryTable1, queryTable2)
    }else{
      queryTable1 <- queryTable1[interpDetailsIndex,]
    }
  } 
  
  cat("Series remaining after interp.details filter: ", nrow(queryTable1), "\n\n")
  
  
  #Seasonality
  seasonalityListsCount <- sum(unlist(lapply(seasonality, function(x) methods::is(x, "list"))))
  if(seasonalityListsCount < 1){
    seasonalityListsCount <- 1
  }
  
  if(!is.null(seasonality)){
    seasons1 <- unique(queryTable1$interpretation1_seasonality[!is.na(queryTable1$interpretation1_seasonality)])
    catchSeasons <- list()
    for (k in 1:seasonalityListsCount){
      
      x1 <- tolower(seasonality[[k]])
      y1 <- seasons1
      
      test1 <- data.frame(matrix(ncol = length(x1), nrow = length(y1)))
      for (i in 1:length(x1)){
        test1[,i] <- grepl(x1[i],gsub(pattern = "[^a-zA-Z0-9]", replacement = "", x = tolower(y1)))
      }
      testAll <- rowSums(test1)==ncol(test1)
      catchSeasons[[k]] <- seasons1[testAll]
    }
    
    results1 <- unlist(lapply(catchSeasons, function(x) which(queryTable1$interpretation1_seasonality %in% x,)))
    results1
    
    if(!is.null(season.not)){
      catchseason.not <- list()
      for (k in 1:length(season.not)){
        x1 <- tolower(season.not[[k]])
        y1 <- seasons1
        
        test1 <- data.frame(matrix(ncol = length(x1), nrow = length(y1)))
        for (i in 1:length(x1)){
          test1[,i] <- grepl(x1[i],gsub(pattern = "[^a-zA-Z0-9]", replacement = "", x = tolower(y1)))
        }
        testAll <- rowSums(test1)==ncol(test1)
        catchseason.not[[k]] <- seasons1[testAll]
      }
      results2 <- unlist(lapply(catchseason.not, function(x) which(queryTable1$interpretation1_seasonality == x,)))
      results2
      results1 <- results1[!results1 %in% results2]
    }
    queryTable1 <- queryTable1[results1,]
  }
  
  cat("Series remaining after seasonality filter: ", nrow(queryTable1), "\n\n")
  
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


