#' Query the LiPDverse
#'
#' @param variable.name vector of variable names from c(d18O, d13C, treeRing, etc.)
#' @param archive.type vector of archive types. see unique(queryTable1$archiveType)
#' @param coord lat/lon extent of interest c(latMin, latMax, lonMin, lonMax)
#' @param age.min Sampling covers at least this recent extent, age BP
#' @param age.max Sampling covers at least this old extent, age BP
#' @param pub.info last name of author, DOI, unique word from title, etc. all lowercase, based on associated publications
#' @param country Coutnry origin of dataset from unique(queryTable1$country2), based on lat/lon
#' @param continent Continent origin of dataset from unique(queryTable1$continent), based on lat/lon
#' @param ocean Gather datasets from the marine environment, based on lat/lon
#' @param seasonality list of seasons where items within a list are treated with "AND" logic and separate lists are treated with "OR" logic ie. list(list("July", "August"), list("7,8"), list("summer"))
#' @param season.not seasons not desired with input format identical to seasonality
#' @param interp.vars vector of interpretation variables ie. c("SST", "upwelling"), see possible: unique(queryTable$interp_Vars)
#' @param interp.details vector of interpretation variables ie. c("sea@surface", "elNino"), see possible: unique(queryTable$interp_Details),
#' @param compilation #ompilation name as character vector from unique(queryTable$paleoData_mostRecentCompilations), eg. c("temp12k", "wnam")
#' @param verbose offer details of filters and get feedback from user
#' @param skip.update remove update prompt
#'
#' @return filtered query table
#' @export
#'
queryLipdverse <- function(variable.name = NULL,
                     archive.type = NULL,
                     coord = c(-90,90,-180,180),
                     age.min = NULL,
                     age.max = NULL,
                     pub.info = NULL,
                     country = NULL,
                     continent = NULL,
                     ocean = FALSE,
                     seasonality = NULL,
                     season.not = NULL,
                     interp.vars = NULL,
                     interp.details = NULL,
                     compilation = NULL,
                     verbose = FALSE,
                     skip.update = FALSE
                     ){

  if (skip.update == TRUE){
    .lipdRquery$queryUpdated <- 1
  }
  if(.lipdRquery$queryUpdated == 0){
    ans1 <- askYesNo("Would you like to update the query table (recommended)?")
    if(ans1){
      update_queryTable()
    }

    .lipdRquery$queryUpdated <- 1
  }


  queryTable1 <- queryTable
  #
  if(verbose){cat("Series available before filtering: ", nrow(queryTable1), "\n\n")}

  #Filter by user coordinates
  queryTable1 <- queryTable1[queryTable1$geo_latitude >= coord[1],]
  queryTable1 <- queryTable1[queryTable1$geo_latitude <= coord[2],]
  queryTable1 <- queryTable1[queryTable1$geo_longitude >= coord[3],]
  queryTable1 <- queryTable1[queryTable1$geo_longitude <= coord[4],]

  if(verbose){cat("Series remaining after coord filter: ", nrow(queryTable1), "\n\n")}

  #Marine samples?
  if(ocean==TRUE){
    if (!is.null(country) | !is.null(continent)){
      stop("Cannot process ocean=TRUE alongside continent/country inputs.
           Use only coord input if both marine and terrestrial datasets are desired.")
    }
    queryTable1 <- queryTable1[is.na(queryTable1$country2)==TRUE,]
  }

  if(verbose){cat("Series remaining after marine filter: ", nrow(queryTable1), "\n\n")}

  #Filter by continent
  if (!is.null(continent)){
    queryTable1 <- queryTable1[queryTable1$continent == continent,]
  }
  if(verbose){cat("Series remaining after continent filter: ", nrow(queryTable1), "\n\n")}

  #Filter by country
  if (!is.null(country)){
    queryTable1 <- queryTable1[queryTable1$country2 %in% country,]
    queryTable1 <- queryTable1[!is.na(queryTable1$country2),]
  }

  if(verbose){cat("Series remaining after country filter: ", nrow(queryTable1), "\n\n")}

  #Filter by time
  if (!is.null(age.max)){
    queryTable1 <- queryTable1[queryTable1$earliestYear < age.max,]
  }
  if (!is.null(age.min)){
    queryTable1 <- queryTable1[queryTable1$mostRecentYear > age.min,]
  }

  if(verbose){cat("Series remaining after time filter: ", nrow(queryTable1), "\n\n")}

  #Filter by archive.type
  if (!is.null(archive.type)){
    archiveTypeIndex <- c()
    for (i in archive.type){
      archiveTypeIndex <- c(archiveTypeIndex, which(grepl(tolower(i), tolower(queryTable1$archiveType))))
    }
    queryTable1 <- queryTable1[archiveTypeIndex,]
  }

  if(verbose){cat("Series remaining after archive.type filter: ", nrow(queryTable1), "\n\n")}

  #Filter for desired variable names
  if (!is.null(variable.name)){
    varNameInex <- c()
    for (i in variable.name){
      varNameInex <- c(varNameInex, which(grepl(tolower(i), tolower(queryTable1$paleoData_variableName))))
    }
    queryTable1 <- queryTable1[varNameInex,]
  }

  if(verbose){cat("Series remaining after variable.name filter: ", nrow(queryTable1), "\n\n")}

  #Filter for desired interpretation variables
  if (!is.null(interp.vars)){
    interpVarInex <- c()
    for (i in interp.vars){
      interpVarInex <- c(interpVarInex, which(grepl(tolower(i), tolower(queryTable1$interp_Vars))))
    }
    if(!is.null(interp.details)){
      queryTable2 <- queryTable1[interpVarInex,]
    }else{
      queryTable1 <- queryTable1[interpVarInex,]
    }
  }

  if(verbose){cat("Series remaining after interp.vars filter: ", nrow(queryTable1), "\n\n")}

  #Filter for desired interpretation details
  if (!is.null(interp.details)){
    interpDetailsIndex <- c()
    for (i in interp.details){
      interpDetailsIndex <- c(interpDetailsIndex, which(grepl(tolower(i), tolower(queryTable1$interp_Details))))
    }
    if(!is.null(interp.vars)){
      queryTable1 <- queryTable1[interpDetailsIndex,]
      queryTable1 <- rbind(queryTable1, queryTable2)
    }else{
      queryTable1 <- queryTable1[interpDetailsIndex,]
    }
  }

  if(verbose){cat("Series remaining after interp.details filter: ", nrow(queryTable1), "\n\n")}

  #Filter for desired compilation
  if (!is.null(compilation)){
    compilationIndex <- c()
    for (i in compilation){
      compilationIndex <- c(compilationIndex, which(grepl(tolower(i), tolower(queryTable1$paleoData_mostRecentCompilations))))
    }
    queryTable1 <- queryTable1[compilationIndex,]
  }

  if(verbose){cat("Series remaining after compilation filter: ", nrow(queryTable1), "\n\n")}


  #Seasonality
  if(!is.null(seasonality)){
    seasonalityListsCount <- sum(unlist(lapply(seasonality, function(x) methods::is(x, "list"))))
    if(seasonalityListsCount < 1){
      seasonalityListsCount <- 1
    }

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
      seasonNotListsCount <- sum(unlist(lapply(season.not, function(x) methods::is(x, "list"))))
      if(seasonNotListsCount < 1){
        seasonNotListsCount <- 1
      }

      catchseason.not <- list()
      for (k in 1:seasonNotListsCount){
        x1 <- tolower(season.not[[k]])
        y1 <- seasons1

        test1 <- data.frame(matrix(ncol = length(x1), nrow = length(y1)))
        for (i in 1:length(x1)){
          test1[,i] <- grepl(x1[i],gsub(pattern = "[^a-zA-Z0-9]", replacement = "", x = tolower(y1)))
        }
        testAll <- rowSums(test1)==ncol(test1)
        catchseason.not[[k]] <- seasons1[testAll]
      }
      results2 <- unlist(lapply(catchseason.not, function(x) which(queryTable1$interpretation1_seasonality %in% x,)))
      results2
      results1 <- results1[!results1 %in% results2]
    }
    queryTable1 <- queryTable1[results1,]
  }

  if(verbose){cat("Series remaining after seasonality filter(s): ", nrow(queryTable1), "\n\n")}

  #pub.info
  if (!is.null(pub.info)){

    pubIndex <- c()
    for (i in pub.info){
      pubIndex <- c(pubIndex, grep(tolower(i), tolower(queryTable1$auth)))
    }

    # authorRows <- grep(tolower(pub.info), tolower(queryTable1$auth))

    queryTable1 <- queryTable1[pubIndex,]
  }

  if(verbose){cat("Series remaining after pub.info filter: ", nrow(queryTable1), "\n\n")}

  #Final tally and check-in
  cat("Based on your query parameters, there are", nrow(queryTable1), "available time series in", length(unique(queryTable1$dataSetName)), "datasets\n\n")
  if(nrow(queryTable1) < 1){
    grab.data <- FALSE
  }

  return(queryTable1)

  #cat(unique(queryTable1$dataSetName), "\n\n")

  # if(grab.data==TRUE){
  #
  #   #download and create a multilipd
  #   URLs <- unique(queryTable1$lipdverseLink)
  #   URLs <- URLs[!is.na(URLs)]
  #   URLs <- gsub(".html", ".lpd", URLs)
  #
  #   D <- list()
  #   for (i in 1:length(URLs)){
  #     D[[i]] <- readLipd(URLs[i])
  #   }
  #
  #   D <- lipdR:::new_multiLipd(D)
  #
  #   print("\n")
  #
  #   print(summary(D))
  #
  #   invisible(D)
  # }else{
  #   cat("Exiting without LiPD download\n")
  # }
}


