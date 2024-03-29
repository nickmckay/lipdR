

#' print formatted citations to stdout
#'
#' @param pub a pub object from a lipd
#' @importFrom crayon bold
#' @family summary
#'
#' @author David Edge
#'
printCitations <- function(pub){
  numPubs <- length(pub)

  numPrint <- if(numPubs < 3){
    numPubs}else{
      3
    }


  cat(crayon::bold("### Publications (printing first", numPrint, "citations of ", numPubs, " total) ###\n\n"))
  for (i in 1:numPubs){
    if ("citation" %in% attributes(pub[[i]])$names){
      cat(pub[[i]]$citation, "\n\n")
    }else{
      pubFormd <- formatCitation(pub[[i]])
      cat(pubFormd, "\n\n")
    }
  }
}

################################################################################################
################################################################################################
################################################################################################

#' Format citation from 'pub' portion of LiPD file
#'
#' @param pub pub object from a lipd
#' @family summary
#'
#' @return formatted APA citation
#' @export
#' @author David Edge
#'
formatCitation <- function(pub){
  if ("author" %in% attributes(pub)$names){
    if(!is.null(pub$author)){
      auths1 <- unname(unlist(pub$author))
      authsAll <- auths1[1]
      if ((length(auths1)-1)>0){
        for (i in 2:length(auths1)){
          authsAll <- paste0(authsAll, ", ", auths1[i])
        }
      }
      authsAll <- paste0(authsAll, " ")
      returns1 <- authsAll
    }
  }else(
    returns1 <- NULL
  )
  if ("year" %in% attributes(pub)$names){
    if(!is.null(pub$year)){
      returns1 <- paste0(returns1, "(", pub$year, "). ")
    }
  }
  if ("title" %in% attributes(pub)$names){
    if(!is.null(pub$title)){
      returns1 <- paste0(returns1, pub$title, ". ")
    }
  }
  if ("journal" %in% attributes(pub)$names){
    if(!is.null(pub$journal)){
      returns1 <- paste0(returns1, pub$journal, ". ")
    }
  }
  if ("doi" %in% attributes(pub)$names){
    if(!is.null(pub$doi)){
      returns1 <- paste0(returns1, pub$doi, ". ")
    }
  }
  return(returns1)
}


################################################################################################
################################################################################################
################################################################################################
#' Extends data frames to conserve all columns when merging
#'
#' @param df1 data.frame
#' @param df2 data.frame
#' @family summary
#' @author David Edge
#'
#' @return data frames with all rows/columns conserved
#'
matchCols <- function(df1,df2){
  newCol1 <- names(df2)[!(names(df2) %in% names(df1))]
  if(length(newCol1) > 0){
    for (ii in 1:length(newCol1)){
      df1[[as.character(newCol1[ii])]] <- NA
    }

  }
  newCol2 <- names(df1)[!(names(df1) %in% names(df2))]
  if (length(newCol2) > 0){
    for (ii in 1:length(newCol2)){
      df2[[as.character(newCol2[ii])]] <- NA
    }
  }
  return(list(df1,df2))
}

################################################################################################
################################################################################################
################################################################################################

#' Print summary data from Paleo/Chron measurement tables
#'
#' @param dataIn paleoData or chronData objects from a lipd
#' @param run.quiet suppress printing
#' @param skip.table print the table?
#' @family summary
#' @importFrom crayon bold
#' @importFrom tibble as_tibble
#' @author David Edge
#'
#' @return measurement tables
#'
printSummaryData <- function(dataIn, run.quiet = FALSE, skip.table = FALSE){

  PDnum <- length(dataIn)

  allTables <- list()

  tableCount <- 0

  if (skip.table == TRUE){
    for (i in 1:PDnum){

      measTabNum <- length(dataIn[[i]]$measurementTable)

      for (j in 1:measTabNum){

        tableCount <- tableCount + 1

        measTab <- dataIn[[i]]$measurementTable[[j]]

        valuesIndex <- unlist(lapply(measTab, function(x) "values" %in% attributes(x)$names), use.names = FALSE)

        varsOnly <- measTab[valuesIndex]

        paleoMeasTableDF <- as.data.frame(sapply(varsOnly, "[", "values"))
        names(paleoMeasTableDF) <- sub("\\.values.*", "", names(paleoMeasTableDF))

        hasUnits <- unlist(lapply(measTab, function(x) "units" %in% attributes(x)$names), use.names = FALSE)
        unitsOnly <- measTab[hasUnits]
        tableVars <- as.data.frame(sapply(unitsOnly, "[", "units"))
        names(tableVars) <- sub("\\.units.*", "", names(tableVars))

        hasDescr <- unlist(lapply(measTab, function(x) "description" %in% attributes(x)$names), use.names = FALSE)
        if(sum(hasDescr)>0){
          descrOnly <- measTab[hasDescr]
          tableVarDescr <- as.data.frame(sapply(descrOnly, "[", "description"))
          names(tableVarDescr) <- sub("\\.description.*", "", names(tableVarDescr))

          unitDescrMatch <- matchCols(tableVars, tableVarDescr)

          tableVars <- merge(unitDescrMatch[[1]], unitDescrMatch[[2]], all = TRUE)
        }


        numObs <- dim(paleoMeasTableDF)[1]
        numVars <- dim(paleoMeasTableDF)[2]
        if(run.quiet==FALSE){
          cat(crayon::bold(paste0("\nSummary data for object ", i, ","), "Measurement Table", j, "of", paste0(measTabNum, ":\n")))
          cat("Measurement table contains", numObs, "observations of", numVars, "variables\n\n")
        }

        if (measTabNum == 0){
          stop("Could not read measurement table, check format\n")
        }
      }
    }
  }else{
    for (i in 1:PDnum){

      measTabNum <- length(dataIn[[i]]$measurementTable)

      for (j in 1:measTabNum){

        tableCount <- tableCount + 1

        measTab <- dataIn[[i]]$measurementTable[[j]]

        valuesIndex <- unlist(lapply(measTab, function(x) "values" %in% attributes(x)$names), use.names = FALSE)

        varsOnly <- measTab[valuesIndex]

        paleoMeasTableDF <- as.data.frame(sapply(varsOnly, "[", "values"))
        names(paleoMeasTableDF) <- sub("\\.values.*", "", names(paleoMeasTableDF))

        hasUnits <- unlist(lapply(measTab, function(x) "units" %in% attributes(x)$names), use.names = FALSE)
        unitsOnly <- measTab[hasUnits]
        tableVars <- as.data.frame(sapply(unitsOnly, "[", "units"))
        names(tableVars) <- sub("\\.units.*", "", names(tableVars))

        hasDescr <- unlist(lapply(measTab, function(x) "description" %in% attributes(x)$names), use.names = FALSE)
        if(sum(hasDescr)>0){
          descrOnly <- measTab[hasDescr]
          tableVarDescr <- as.data.frame(sapply(descrOnly, "[", "description"))
          names(tableVarDescr) <- sub("\\.description.*", "", names(tableVarDescr))

          unitDescrMatch <- matchCols(tableVars, tableVarDescr)

          tableVars <- merge(unitDescrMatch[[1]], unitDescrMatch[[2]], all = TRUE)
        }


        numObs <- dim(paleoMeasTableDF)[1]
        numVars <- dim(paleoMeasTableDF)[2]
        if(run.quiet==FALSE){
          cat(crayon::bold(paste0("\nSummary data for object ", i, ","), "Measurement Table", j, "of", paste0(measTabNum, ":\n")))
          cat("Measurement table contains", numObs, "observations of", numVars, "variables\n\n")
        }

        if (measTabNum == 0){
          stop("Could not read measurement table, check format\n")
        }

        varsTableMatch <- matchCols(paleoMeasTableDF, tableVars)
        paleoMeasTableDF <- varsTableMatch[[1]]
        tableVars <- varsTableMatch[[2]]


        paleoMeasTableDF <- paleoMeasTableDF[ ,names(tableVars)]

        allTables[[tableCount]] <- paleoMeasTableDF

        #numericCols <- rep(NA, ncol(paleoMeasTableDF))
        minCol <- rep(NA, ncol(paleoMeasTableDF))
        medianCol <- rep(NA, ncol(paleoMeasTableDF))
        maxCol <- rep(NA, ncol(paleoMeasTableDF))

        for (k4 in 1:ncol(paleoMeasTableDF)){
          if (is.numeric(paleoMeasTableDF[,k4])){
            paleoMeasTableDF[,k4] <- as.numeric(paleoMeasTableDF[,k4])
            minCol[k4] <- min(paleoMeasTableDF[,k4], na.rm = TRUE)
            medianCol[k4] <- median(paleoMeasTableDF[,k4], na.rm = TRUE)
            maxCol[k4] <- max(paleoMeasTableDF[,k4], na.rm = TRUE)
          }else{
            minCol[k4] <- sort(paleoMeasTableDF[,k4])[1]
            medianCol[k4] <- sort(paleoMeasTableDF[,k4])[round(length(paleoMeasTableDF[,k4])/2)]
            maxCol[k4] <- sort(paleoMeasTableDF[,k4])[length(paleoMeasTableDF[,k4])]
          }
        }

        tableVars <- rbind(tableVars, minCol)
        tableVars <- rbind(tableVars, medianCol)
        tableVars <- rbind(tableVars, maxCol)


        if(sum(hasDescr)>0){
          tableVars <- cbind(c("units", "description", "min", "median", "max"),tableVars)
        }else{
          tableVars <- cbind(c("units", "min", "median", "max"),tableVars)
        }
        colnames(tableVars)[1] <- " "
        tableVars <- tibble::as_tibble(tableVars)

        if(dim(tableVars)[1]>0){
          if (run.quiet==FALSE){
            print(tableVars, row.names = FALSE)
          }
        }
      }
    }
    return(allTables)

  }

  }


################################################################################################
################################################################################################
################################################################################################

#' Extract and print model info
#'
#' @param chron.model model object from chron data of lipd object
#' @author David Edge
#'
#'
printModel <- function(chron.model){

  numModels <- length(chron.model)

  if(numModels > 0){
    cat(crayon::bold(numModels, "model(s) found"), "\n\n")
  }

  for (bbb in 1:numModels){
    cat("Model", bbb, "contains: ")
    cat(attributes(chron.model[[bbb]])$names, "\n\n")
    if (!is.null(chron.model[[bbb]]$method$algorithm)){
      cat("Model algorithm:", chron.model[[bbb]]$method$algorithm, "\n")
    }
    if (!is.null(chron.model[[bbb]]$method$parameters)){
      params1 <- chron.model[[bbb]]$method$parameters
      paramNames <- names(params1)
      paramVals <- unlist(params1)
      cat("Model parameters:\n")
      for(ttt in 1:length(params1)){
        cat(paramNames[ttt], paramVals[ttt], "\n")
      }
      cat("\n")
    }
    if (!is.null(chron.model[[bbb]]$ensembleTable)){
      numTables <- length(chron.model[[bbb]]$ensembleTable)
      cat("Model contains", numTables, "ensemble table(s)\n")
      for (hhh in 1:numTables){
        ensemble1 <- chron.model[[bbb]]$ensembleTable[[hhh]]
        indexTable <- unlist(lapply(ensemble1, function(x) "values" %in% attributes(x)$names), use.names = FALSE)
        tableLoc <- ensemble1[indexTable]
        ensembleDF <- as.data.frame(sapply(tableLoc, "[", "values"))
        tableDim <- dim(ensembleDF)
        cat("dimensions of ensemble table",hhh, "of",numTables,":", tableDim[1],"x",tableDim[2], "\n\n")
      }
      cat("\n")
    }
  }
}



################################################################################################
################################################################################################
################################################################################################

#' Print a summary of the contents of a Multi-LiPD
#'
#' @param multi.lipd a multi_lipd object
#' @param print.length length of printed summary table
#' @param time.units reporting preference if data contain both "year" and "age"
#' @param skip.table print a short summary without the table
#' @importFrom glue glue
#' @importFrom crayon bold
#' @author David Edge
#' @return a summary table
#' @export
#' @family summary
#'
multiLipdSummary <- function(multi.lipd, print.length=20, time.units="AD", skip.table = FALSE){



  if (is.null(multi.lipd)){
    stop("multi.lipd input required")
  }

  numLipd <- length(multi.lipd)

  cat(crayon::bold("Multi LiPD contains", numLipd, "LiPD files.\n\n"))


  archiveTypes <- list()
  lons <- list()
  lats <- list()
  dataCounts <- data.frame(matrix(ncol = 8, nrow = numLipd, data=NA))
  colnames(dataCounts) <- c("Dataset", "Archive Type", "NumPalTabs", "NumChrTabs", "NumEns","AgeMin","AgeMax", "Paleo Vars")

  for (qqq in 1:numLipd){
    L <- multi.lipd[[qqq]]

    if (is.null(L$paleoData) & is.null(L$chronData)){
      warning("LiPD", qqq, "in multi_lipd object missing paleoData and chronData\n")
      next
    }

    dataCounts[qqq,1] <- as.character(L$dataSetName)

    #number of each unique archive type
    archiveTypes[[qqq]] <- L$archiveType
    dataCounts[qqq,2] <- L$archiveType

    #geographic region as bounded by the min/max lat/lon
    lats[[qqq]] <- L$geo$latitude
    lons[[qqq]] <- L$geo$longitude

    #paleodata
    totalPD <- 0
    paleoVars <- character()
    if (!is.null(L$paleoData)){
      for (fff in 1:length (L$paleoData)){
        if(!is.null(L$paleoData[[fff]]$measurementTable)){
          numTables <- length(L$paleoData[[fff]]$measurementTable)
          totalPD <- totalPD + numTables
          getTable <- printSummaryData(L$paleoData, run.quiet=TRUE)

          for (ddd in 1:numTables){
            paleoVars <- c(paleoVars, attributes(getTable[[ddd]])$names)
          }
        }
      }
    }
    allPaleoVars <- unlist(strsplit(paleoVars,", "))
    dataCounts[qqq,8] <- paste(allPaleoVars, collapse = ', ')
    dataCounts[qqq,3] <- totalPD


    #chronData
    totalCD <- 0
    if (!is.null(L$chronData)){
      for (fff in 1:length (L$chronData)){
        if(!is.null(L$chronData[[fff]]$measurementTable)){
          totalCD <- totalCD + length(L$chronData[[fff]]$measurementTable)
        }
      }
    }
    dataCounts[qqq,4] <- totalCD

    #age info
    if (!is.null(L$paleoData[[1]]$measurementTable[[1]]$year$values)){
      if (time.units == "AD"){
        dataCounts[qqq,6] <- paste(round(max(L$paleoData[[1]]$measurementTable[[1]]$year$values), 2), L$paleoData[[1]]$measurementTable[[1]]$year$units, sep=' ')
        dataCounts[qqq,7] <- paste(round(min(L$paleoData[[1]]$measurementTable[[1]]$year$values), 2), L$paleoData[[1]]$measurementTable[[1]]$year$units, sep=' ')
      }
    }else if (!is.null(L$paleoData[[1]]$measurementTable[[1]]$age$values)){
      dataCounts[qqq,6] <- paste(round(min(L$paleoData[[1]]$measurementTable[[1]]$age$values), 2), L$paleoData[[1]]$measurementTable[[1]]$age$units, sep=' ')
      dataCounts[qqq,7] <- paste(round(max(L$paleoData[[1]]$measurementTable[[1]]$age$values), 2), L$paleoData[[1]]$measurementTable[[1]]$age$units, sep=' ')
    }else{
      message("No age data found for:", L$dataSetName ,"\n")
    }

    #models
    ensTables <- 0
    if (!is.null(L$chronData)){
      for (fff in 1:length (L$chronData)){
        if(!is.null(L$chronData[[fff]]$model)){
          for (vvv in 1:length (L$chronData[[fff]]$model)){
            if(!is.null(L$chronData[[fff]]$model[[vvv]])){
              ensTables <- ensTables + length(L$chronData[[fff]]$model[[vvv]]$ensembleTable)
            }
          }
        }
      }
    }
    dataCounts[qqq,5] <- ensTables
  }

  uniqueArchives <- unique(unlist(archiveTypes))
  numUniqueArchives <- length(uniqueArchives)

  #archiveType
  cat(crayon::bold(glue::glue("### Archive Types ###\n\n")))
  for (uuu in 1:numUniqueArchives){
    cat(sum(unlist(archiveTypes) %in% uniqueArchives[uuu]), uniqueArchives[uuu], "records\n")
  }
  cat("\n")
  cat(crayon::bold(glue::glue("### Geographic Bounds ###\n\n")))

  maxLat <- max(unlist(lats))
  minLat <- min(unlist(lats))
  maxLon <- max(unlist(lons))
  minLon <- min(unlist(lons))
  cat("All sites located between", paste0(minLat,"N"), "to", paste0(maxLat, "N"), "and", paste0(minLon, "E"), "to", paste0(maxLon, "E"), "\n\n")


  dataCounts <- tibble::as_tibble(dataCounts)

  if (skip.table == TRUE){
    z=1
  }else{
    cat(crayon::bold(glue::glue("### Measurement tables and models ###\n\n")))
    cat("*Age values gathered from PaleoData Object 1, Measurement Table 1\n")
    print(dataCounts, n=print.length)
    invisible(dataCounts)
  }
}


################################################################################################
################################################################################################
################################################################################################

#' Print a summary of the contents of a lipd_ts or lipd_ts_tibble
#'
#' @param ts.object a lipd_ts or lipd_ts_tibble
#' @param print.length length of printed summary table
#' @param add.variable include additional variables from ts object in summary
#' @param skip.table print a short summary without the table
#'
#' @author David Edge
#' @return a summary table
#' @export
#' @family summary
#'
lipdTSSummary <- function(ts.object, print.length=10, add.variable = NULL, skip.table = FALSE){


  time.variable <- "Age"

  if (is.lipdTs(ts.object)){
    ts.object <- ts2tibble(ts.object)
  }else if (is.lipdTsTibble(ts.object)){
    abtl1 <- 1
  }else{
    stop("ts.object is not a lipd_ts or lipd_ts_tibble, check class(ts.object)")
  }

  allYear <- FALSE
  allAge <- FALSE
  totCols <- 10
  numTS <- nrow(ts.object)

  cat("LiPD TS object containing",length(unique(ts.object$datasetId)), "datasets\n")
  cat(numTS, "variables and", ncol(ts.object), "data and metadata fields.\n\n")



  #look for "age" and "year" variables, do all TS have one or the other?
  hasYear <- rep(NA, numTS)
  hasAge <- rep(NA, numTS)
    for (i in 1:nrow(ts.object)){
      hasYear[i] <- !is_blank(unlist(ts.object$year[i]))
      hasAge[i] <- !is_blank(unlist(ts.object$age[i]))
    }

    totYear <- sum(hasYear,na.rm = TRUE)
    totAge <- sum(hasAge,na.rm = TRUE)

  if (totYear < numTS & totAge < numTS){
    cat("TS object contains a mixture of age (BP) and year (AD) units.\n")
    cat(totYear, "variables contain year (AD), while", totAge, "contain age (BP).\n\n")
    if (totYear > totAge){
      time.variable <- "Year"
    }else{
      time.variable <- "Age"
    }
  }else if (totYear == numTS & totAge == numTS){
    allYear <- TRUE
    allAge <- TRUE
    cat("All variables contain both age (BP) and year (AD) data.\n\n")
    if (is.null(time.variable)){
      time.variable <- "Year"
    }
  }else if (totYear == numTS & totAge != numTS){
    allYear <- TRUE
    cat("All variables contain year (AD) data,", totAge, "variables contain age (BP) data.\n\n")
    if (is.null(time.variable)){
      time.variable <- "Year"
    }
  }else{
    allAge <- TRUE
    cat("All variables contain age (BP) data,", totYear, "variables contain year (AD) data.\n\n")
    if (is.null(time.variable)){
      time.variable <- "Age"
    }
  }



  #calculate time overlap

  maxAges <- max(unlist(ts.object$age[i]), na.rm = TRUE)
  minAges <- min(unlist(ts.object$age[i]), na.rm = TRUE)

  if (allYear == FALSE & allAge ==FALSE){
    cat("Can not provide interval of time overlap, units not standardized.\n\n")
  }

  if(time.variable == "Year" & allYear == TRUE){
    lastOverlap <- min(minAges)
    firstOverlap <- max(maxAges)
    if (lastOverlap < firstOverlap){
      cat("No common time interval.\n\n")
    }else{
      cat("All variables contain data in the interval", firstOverlap, "(AD) to", lastOverlap, "(AD).\n\n")
    }
  }
  if (time.variable == "Age" & allAge == TRUE){
    lastOverlap <- max(minAges)
    firstOverlap <- min(maxAges)
    if (lastOverlap > firstOverlap){
      cat("No common time interval.\n\n")
    }else{
      cat("All variables contain data in the interval", firstOverlap, "(BP) to", lastOverlap, "(BP).\n\n")
    }
  }

  if (skip.table == FALSE){
    #Start TS summary df with variable names as factors
    if(!is.null(add.variable)){
      totCols <- 2 + length(add.variable)
    }

    factorizedVarnames <- as.factor(ts.object$paleoData_variableName)

    frequency <- sort(summary(factorizedVarnames), decreasing = TRUE)
    ageYearIndices <- c(grep("age", tolower(names(frequency))), grep("year", tolower(names(frequency))), grep("depth", tolower(names(frequency))))
    varFreq <- frequency[!1:length(frequency) %in% ageYearIndices]

    #return tibble with: dataset name, TSid, lat, lon, min age, max age, paleodat variable name, min paleodata val, mean paleodata val, max paleodata val
    TSsummary <- data.frame("paleoData_variableName" = names(varFreq),
                            "varName_freq" = varFreq)


    #Extend TS summary with character variables as factors
    #Print numeric variables separately
    if (length(add.variable)>0){
      for (jkl in 1:length(add.variable)){
        newVar <- unlist(ts.object[add.variable[jkl]])
        goodIndex <- is.valid.vec(newVar)
        isNumOrNA <- !is.na(as.numeric(newVar[goodIndex]))

        if (sum(isNumOrNA) > (sum(goodIndex) * 0.9)){
          quantileInfo <- quantile(as.numeric(newVar[goodIndex]))
          cat(add.variable[jkl], " min:", quantileInfo[1], " 25%:", quantileInfo[2], " median:", quantileInfo[3], " 75%:", quantileInfo[4], " max:", quantileInfo[5], "\n\n")
        }else{
          factorizedVarnames1 <- as.factor(newVar)

          frequency1 <- sort(summary(factorizedVarnames1), decreasing = TRUE)
          ageYearIndices <- c(grep("age", tolower(names(frequency1))), grep("year", tolower(names(frequency1))), grep("depth", tolower(names(frequency1))))
          varFreq1 <- frequency1[!1:length(frequency1) %in% ageYearIndices]
          TSsummaryNew <- data.frame(names(varFreq1), varFreq1)
          colnames(TSsummaryNew) <- c(as.character(add.variable[jkl]), paste0(as.character(add.variable[jkl]), "_freq"))
          TSsummary <- cbind.NA(TSsummary, TSsummaryNew)
        }
      }
    }


    #Format TS summary as tibble for printing

    tibSummary <- tibble::as_tibble(TSsummary)
  }



  if (skip.table == TRUE){
    z=1
  }else {
    print(tibSummary, n=print.length)
    invisible(tibSummary)
  }

}

################################################################################################
################################################################################################
################################################################################################

#' Print a summary of the contents of a LiPD file
#'
#' @param L a lipd object
#' @param skip.table print a shorter summary without the table
#' @importFrom glue glue
#' @importFrom crayon bold
#' @author David Edge
#' @export
#' @family summary
#'
lipdSummary <- function(L, skip.table = FALSE){
  #Name
  cat("################################################################################################\n")
  cat(crayon::bold(glue::glue("\n{L$dataSetName}\n")))
  cat(crayon::bold(glue::glue("\n\n{L$datasetId}\n")))
  cat(crayon::bold(glue::glue("\n\nv.{L$changelog[[length(L$changelog)]]$version}\n")))
  cat("\n################################################################################################\n\n")

  #archiveType
  cat(crayon::bold(glue::glue("\n### Archive Type ###\n")))
  cat("\n")
  cat(glue::glue("{L$archiveType}\n\n\n"))

  #lipdverseUrl
  cat(glue::glue("{L$lipdverseUrl}\n\n"))

  #geo

  cat(crayon::bold("### Geographic Metadata ###\n"))
  gcmd <- L$geo$location

  if(!is.null(gcmd)){
    cat(glue::glue("{L$geo$siteName} ({gcmd})\n"))
  }else{
    cat(glue::glue("{L$geo$siteName} "))
  }

  if(!is.finite(L$geo$elevation)){
    cat(glue::glue("({L$geo$latitude}N, {L$geo$longitude}E)"))
  }else{
    cat(glue::glue("({L$geo$latitude}N, {L$geo$longitude}E), {L$geo$elevation} masl"))
  }


  #pub
  if (!is.null(L$pub)){
    cat("\n\n")
    printCitations(L$pub)
  }



  #paleodata

  if (!is.null(L$paleoData)){
    cat(crayon::bold("### Paleo Data ###\n"))
    #printPD(L$paleoData)
    a1 <- printSummaryData(L$paleoData, skip.table = skip.table)
  }
  #chronData

  if (!is.null(L$chronData)){
    cat(crayon::bold("\n### Chron Data ###\n"))
    if (length(L$chronData[[1]]$measurementTable) > 0){
      b1 <- printSummaryData(L$chronData, skip.table = skip.table)
    }else{
      cat("Chron Data does not include measurement table\n\n")
    }
    #model
    otherAttr <- attributes(L$chronData[[1]])$names
    otherAttr <- otherAttr[!otherAttr %in% "measurementTable"]

    if (length(otherAttr) > 0){
      if ("model" %in% otherAttr){
        printModel(L$chronData[[1]]$model)
      }
    }
  }

  cat("\n")
}


