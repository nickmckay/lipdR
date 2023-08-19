#' Get Standard tables
#'
getStandardTables <- function(){
  if(exists("standardTables",envir = lipdEnv)){
    standardTables <- get("standardTables",envir = lipdEnv)
  }else{
    standardTables <- updateStandardTables()
  }
  return(standardTables)
}

#' grab metaData for given key from standard tables
#'
#' @param lipdTS a lipd TS object
#' @param key the key to check
#'
#' @return list
#' @export
updateMetaDataFromStandardTables <- function(lipdTS, key){

  standardTables <- getStandardTables()

  TSorig <- lipdTS


  #Check for appropriate key
  if (!key %in% names(standardTables)){
    stop(paste("key must be one of: ", paste(names(standardTables),collapse = ", ")))
  }

  #Which metadata needs updated for the given key?
  if (key == "paleoData_variableName"){
    meta_keys <- c("paleoData_isAssemblage", "paleoData_datum", "paleoData_summaryStatistic", "paleoData_measurementMaterial",
                   "paleoData_inferredMaterial", "paleoData_method", "paleoData_isPrimary")
  }else if (key == "paleoData_proxy"){
    meta_keys <- c("paleoData_proxyGeneral", "paleoData_measurementMaterial")
  }else if (key == "paleoData_units"){
    meta_keys <- c("paleoData_datum")
  }else{
    returns <- list("TS" = lipdTS, "ChangesDF" = NULL)

    return(returns)
  }

  #Check that the metadata keys and primary key exist, if not, add them
  if(!sum(unlist(lapply(lipdTS, function(x) all(meta_keys %in% names(x))))) == length(lipdTS)){
    #add keys where necessary
    allKeys <- c(meta_keys, key)
    for (i in 1:length(lipdTS)){
      needAdded <- allKeys[!allKeys %in% names(lipdTS[[i]])]
      for (j in needAdded){
        lipdTS[[i]][eval(j)] <- NA
      }
    }
  }

  #update metadata
  #move through each TS, for each metadata field
  newValues <- data.frame(matrix(nrow = length(lipdTS), ncol = length(meta_keys)+1))
  names(newValues) <- c("rowNum", meta_keys)
  for (i in 1:length(lipdTS)){
    #find TS synonym in spreadsheet
    synonymLoc <- which(unlist(unname(lipdTS[[i]][eval(key)])) == standardTables[[eval(key)]]['synonym'])
    newValues[i,1] <- i
    for (j in 1:length(meta_keys)){
      #If the field is blank, check the spreadsheet for metadata based on the synonym in the TS
      if (is_blank(lipdTS[[i]][eval(meta_keys[j])])){
        newVal <- unlist(standardTables[[eval(key)]][synonymLoc,eval(meta_keys[j])])
        if (length(newVal)==0){
          newVal <- NA
        }
        if (is.null(newVal)){
          newVal <- NA
        }
        newValues[i,(j+1)] <- newVal
        lipdTS[[i]][eval(meta_keys[j])] <- newVal
      }
    }
  }

  # getTsVar <- function(TS1, keyNow, TSnum){
  #   tryCatch(
  #     {
  #       pullTsVariable(TS1[[TSnum]], keyNow)
  #     },
  #     error=function(cond){
  #       return(NA)
  #     },
  #     warning=function(cond){
  #       message(cond)
  #     }
  #   )
  # }
  #
  # #compare original TS to new TS
  # compDF <- data.frame(matrix(nrow = length(lipdTS), ncol = length(meta_keys)))
  # names(compDF) <- meta_keys
  # for (i in 1:length(lipdTS)){
  #   for (j in 1:length(meta_keys)){
  #     currentKey <- meta_keys[j]
  #     origVal <- getTsVar(TSorig, currentKey, i)
  #     newVal <- getTsVar(lipdTS, currentKey, i)
  #     if (is.null(newVal) | is.null(origVal) | length(newVal)==0 | length(origVal)==0){
  #       compDF[i,eval(currentKey)] <- NA
  #     }else{
  #       compDF[i,eval(currentKey)] <- newVal == origVal
  #     }
  #
  #   }
  #   # if (length(comps)==0){
  #   #   compDF[i,] <- rep(NA, length(meta_keys))
  #   # }else{
  #   #   if (length(comps) != ncol(compDF)){
  #   #     stop("Problem at: ", i, " in compDF due to comps = ", paste(comps, collapse = ", "))
  #   #   }
  #   #   compDF[i,] <- comps
  #   # }
  # }

  returns <- list("TS" = lipdTS, "ChangesDF" = newValues)

  return(returns)

}



#Checking function
#check the validity in the appropriate spreadsheet
#' Check all values of a given key for validity in appropriate spreadsheet
#'
#' @param lipdTS a lipdTS object
#' @param key key name
#' @param keyGeneral generalized key name
#'
#' @return invalidDF
#'
checkKey <- function(lipdTS, key, keyGeneral){
  TSvalsO <- pullTsVariable(lipdTS, key,strict.search = TRUE)

  #TSvals <- tolower(TSvalsO)
  numVals <- length(TSvalsO)

#standardVals0 <- unique(standardTables[[eval(keyGeneral)]][["lipdName"]])
standardVals <- unique(standardTables[[eval(keyGeneral)]][["lipdName"]])

# validCheck0 <- TSvalsO %in% standardVals0 |
#   is.na(TSvals) |
#   is.null(TSvals)

validCheck <- TSvalsO %in% standardVals |
              is.na(TSvalsO) |
              is.null(TSvalsO)

#capDiff <- which(validCheck != validCheck0)



#
#   validCheck <- unlist(lapply(tolower(TSvals), function(x) x %in%
#                                 tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["lipdName"]))))) |
#     unlist(lapply(tolower(TSvals), function(x) is.na(x))) |
#     unlist(lapply(tolower(TSvals), function(x) is.null(x)))


  numInvalid <- sum(!validCheck)
  invalidI <- which(!validCheck)
  message("Found ", numInvalid, " invalid keys from ", length(unlist(validCheck)), " total entries for ", key, "\n")

  invalidDF <- as.data.frame(matrix(nrow = numInvalid, ncol = 5))
  countA <- 0

  TSid <- pullTsVariable(lipdTS,"paleoData_TSid",strict.search = TRUE)
  datasetId <- pullTsVariable(lipdTS,"datasetId",strict.search = TRUE)
  dataSetName <- pullTsVariable(lipdTS,"dataSetName",strict.search = TRUE)


  invalidDF <- data.frame(rowNum = invalidI,
                          TSid = TSid[invalidI],
                          dataSetName = dataSetName[invalidI],
                          datasetId = datasetId[invalidI],
                          keyTBD = TSvalsO[invalidI])


  names(invalidDF)[5] <- eval(key)

  return(invalidDF)
}



#' make sure all terms under a controlled key are valid
#'
#' @param lipdTS a lipd-ts object
#' @param key the key that you want to check
#'
#' @return invalid keys df
#' @export

isValidValue <- function(lipdTS, key = NA){

  standardTables <- getStandardTables()

  if (!methods::is(lipdTS, "lipd-ts")){
    lipdTs <- as.lipdTs(lipdTS)
  }


  if (!key %in% names(standardTables)){
    stop(paste("key must be one of: ", paste(names(standardTables),collapse = ", ")))
  }


  #interpretation keys (seasonality and variable)
  interpretation <- FALSE
  if (grepl("interpretation", key)){
    interpretation <- TRUE
  }

  #look for all keys that include "interpretation" and c("seasonaility" or "variable")
  if (interpretation){

    keyID <- sub(".*_", "", key)
    possibleKeys <- lapply(1:100, function(x) paste0("interpretation", x, "_", keyID))
    keysAll <- lapply(lipdTS, function(x) names(x)[names(x) %in% possibleKeys])


      keyGeneral <- gsub('[0-9]+', '', key)


    #how many of these keys exist for each record, save the max number
    #numKeysMax <- max(unlist(lapply(keysAll, function(x) length(x))))

    uniqueKeys <- unique(unlist(keysAll))

    #run check for each unique, eg. interpretation1_seasonality
    returns <- lapply(uniqueKeys, function(x) checkKey(lipdTS, x, keyGeneral))
  }else{

    returns <- checkKey(lipdTS, key, key)
  }

  # if (!verbose){
  #   returns <- NULL
  # }

  invisible(returns)
}




#' standardize terms automatically based on known synonyms
#'
#' @param lipdTS a lipd TS object
#' @param key the key to check
#'
#' @return updated TS
#' @export

standardizeValue <- function(lipdTS, key = NA){


  standardTables <- getStandardTables()

  if (!methods::is(lipdTS, "lipd-ts")){
    lipdTs <- as.lipdTs(lipdTS)
  }

  invalidDF <- isValidValue(lipdTS, key)


  #interpretation keys (seasonality and variable)
  interpretation <- FALSE
  if (grepl("interpretation", key)){
    interpretation <- TRUE
    uniqueKeys <- lapply(invalidDF, function(x) names(x)[5])
    message("Standardizing ", length(invalidDF), " unique keys:\n", paste(uniqueKeys, collapse = "\n"))
  }

  #Find and replace synonyms
  replaceSynonyms <- function(TS=lipdTS, key=NA, invalidDF=NA, interpretation = interpretation){
    noSynonym <- data.frame("TSid" = NA, "key" = NA)

    if (nrow(invalidDF) > 0){
      #add a "keyGeneral" variable for interps
      if(interpretation){
        keyGeneral <- gsub('[0-9]+', '', key)
      }else{
        keyGeneral <- key
      }
      #find synonyms based on "synonym" and "pastName"
      possibleSynonyms <- unique(c(tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["synonym"]))),
                                   tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["paleoData_pastName"])))))
      possibleSynonyms <- possibleSynonyms[!is.na(possibleSynonyms)]

      #Are the current values in the TS for this key valid (are they known lipdNames)

      validCheck2 <- lapply(lipdTS, function(x) tolower(unname(unlist(x[eval(key)]))) %in%
                             tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["lipdName"]))))

      #Build a data frame to show replacement on known synonyms with valid lipdNames
      synonymDF <- as.data.frame(matrix(nrow = sum(!unlist(validCheck2)), ncol = 5))
      names(synonymDF) <- c("rowNum", "dataSetName", "dataSetId", as.character(key), "New")
      countEm <- 0
      for (i in 1:nrow(invalidDF)){

        TSrowNum <- invalidDF$rowNum[[i]]

        synonymLoc <- which(tolower(invalidDF[i,5]) == possibleSynonyms)

        if(length(synonymLoc) == 0){
          nosynonym1 <- data.frame("TSid" = lipdTS[[TSrowNum]]$paleoData_TSid,
                                   "key" = key)
          noSynonym <- rbind.data.frame(noSynonym, nosynonym1)
        }else{
          countEm <- countEm + 1
          #find the known synonym
          synonym <- possibleSynonyms[synonymLoc]

          #locate the corresponding lipdName
          synonymTableLoc <- which(synonym == tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["synonym"]))))
          lipdName <- unname(unlist(standardTables[[eval(keyGeneral)]]["lipdName"]))[synonymTableLoc[1]]

          synonymDF[countEm,1] <- TSrowNum
          synonymDF[countEm,2] <- lipdTS[[as.numeric(TSrowNum)]]$dataSetName
          synonymDF[countEm,3] <- lipdTS[[as.numeric(TSrowNum)]]$datasetId
          synonymDF[countEm,4] <- invalidDF[i,5]
          synonymDF[countEm,5] <- lipdName
        }

        #print(invalidDF[i,5])
        #newRow <- c(TSrowNum, lipdTS[[as.numeric(TSrowNum)]]$dataSetName, lipdTS[[as.numeric(TSrowNum)]]$datasetId, invalidDF[i,5], lipdName)

        # if (length(newRow) != 5){
        #   stop("Problem with TS ", newRow, " for ", key)
        # }



        #lipdTS[[as.numeric(TSrowNum)]][eval(key)] <<- lipdName

      }

      synonymDForig <- synonymDF
      print(tibble::tibble(synonymDF))

      # userResp <- askYesNo(msg = "Replace all?" , default = TRUE)
      #
      # # if(userResp){
      # for (i in 1:nrow(synonymDF)){
      #   lipdTS[[as.numeric(synonymDF$rowNum[i])]][eval(key)] <<- synonymDF[synonymDF$rowNum[i],5]
      # }
      #}

      if(interpretation){
        keyGeneral <- gsub('[0-9]+', '', key)
      }else{
        keyGeneral <- key
      }


      # message("Rerunning check for invalid keys in: ", key)
      # if (key %in% lapply(isValidValue(lipdTS, keyGeneral), function(x) names(x)[5])){
      #   invalidDFnew <- checkKey(lipdTS, key)
      #   print(tibble::tibble(invalidDFnew))
      # }

      if (nrow(noSynonym) > 1){
        noSynonym <- noSynonym[-1,]
      }



      returns1 <- list("synonymDF" = synonymDForig)
      returns2 <- list("noSynonym" = noSynonym)
      returns <- list(returns1=returns1, returns2=returns2)
    }else{
      returns <- NULL
    }


    return(returns)
  }

  returns <- list()
  if (interpretation){
    for (i in 1:length(invalidDF)){
      if (nrow(invalidDF[[i]]>0)){
        returns[[i]] <- replaceSynonyms(lipdTS, key=names(invalidDF[[i]])[5], invalidDF[[i]], interpretation)
      }

    }
    #returns <- lapply(invalidDF, function(x) replaceSynonyms(lipdTS, key=names(x)[5], x, interpretation))
  }else{
    returns <- replaceSynonyms(lipdTS, key, invalidDF, interpretation)
  }

  deleteTheseTS <- c()


  if (!sum(grepl("returns1", names(returns)))>0){
    df0 <- list()
    df10 <- list()
    for (i in 1:length(returns)){
      df1 <- returns[[i]]$returns1$synonymDF
      df0[[i]] <- df1
      df2 <- returns[[i]]$returns2$noSynonym
      df10[[i]] <- df2
      if (length(df1) > 0){
        if(!is.null(nrow(df1))){
          for (j in 1:nrow(df1)){
            rowNum <- df1$rowNum[j]
            newVal <- df1$New[j]
            if(!is.na(rowNum) & length(rowNum) > 0){
              if (grepl("delete", newVal)){
                if(grepl("variableName", key)){
                  deleteTheseTS <- c(deleteTheseTS, lipdTS[[as.numeric(rowNum)]]$paleoData_TSid)
                }else{
                  lipdTS[[as.numeric(rowNum)]][eval(names(df1)[4])] <- NULL
                }
              }else{
                lipdTS[[as.numeric(rowNum)]][eval(names(df1)[4])] <- newVal
              }
            }
          }
        }
      }
    }
  }else{
    df0 <- returns$returns1$synonymDF
    df10 <- returns$returns2$noSynonym
    for (j in 1:nrow(df0)){
      rowNum <- df0$rowNum[j]
      newVal <- df0$New[j]
      if(!is.na(rowNum) & length(rowNum) > 0){
        if (grepl("delete", newVal)){
          if(grepl("variableName", key)){
            deleteTheseTS <- c(deleteTheseTS, lipdTS[[as.numeric(rowNum)]]$paleoData_TSid)
          }else{
            lipdTS[[as.numeric(rowNum)]][eval(names(df0)[4])] <- NULL
          }
        }else{
          lipdTS[[as.numeric(rowNum)]][eval(names(df0)[4])] <- newVal
        }
      }
    }
  }
#
#   allTerms <- pullTsVariable(lipdTS, eval(names(df1)[4]))
#
#     for (i in 1:length(allTerms)){
#     if(grepl("delete", allTerms[i])){
#       TS2$TS[[i]] <- NULL
#     }
#   }





  returns <- list("TS"=lipdTS, "synonymDF" = df0, "deleteTheseTS" = deleteTheseTS, "noSynonym" = df10)

  return(returns)
}


#Delete misnamed terms and TS with bad variableName




#Update notes based on results from `updateMetaDataFromStandardTables()` and `standardizeValue()`


#metadataChangesDF <- updateMetaDataFromStandardTables(TS, "paleoData_variableName")$ChangesDF
#standardizeSynonymDF <- standardizeValue(TS, "paleoData_variableName")$synonymDF
#updateNotes(TS, metadataChangesDF, standardizeSynonymDF)

#' Update notes and paleoData_notes
#'
#' @param lipdTS a LiPD TS object
#' @param metadataChangesDF a data frame listing the changes to the metadata
#' @param standardizeSynonymDF standardizing synonyms dataframe
#' @param key which key
#' @param deleteTheseTS what to delete
#'
#' @return lipdTS
updateNotes <- function(lipdTS, key=NA, metadataChangesDF=NA, standardizeSynonymDF=NA, deleteTheseTS=NA){


  standardTables <- getStandardTables()

  #choose the appropriate notes
  #for each invalid value from metadataChangesDF, update notes
  if(is.na(key)){
    if(all(is.na(standardizeSynonymDF))){
      stop("Must explicitly enter key if not entering standardizeSynonymDF!")
    }
    key=names(standardizeSynonymDF)[4]
  }
  if(!all(is.na(standardizeSynonymDF)) & length(standardizeSynonymDF)>0 & !is.null(standardizeSynonymDF)){
    if(nrow(standardizeSynonymDF)>0){

    standardizeSynonymDF <-
      standardizeSynonymDF[apply(standardizeSynonymDF,
                                 1,
                                 function(x) sum(is.na(x)))!=ncol(standardizeSynonymDF),]
    }
  }

  if (sub("\\_.*", "", key) == "paleoData") {



    if(!all(is.na(standardizeSynonymDF))){

      for (i in 1:nrow(standardizeSynonymDF)){
        if(is.null(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes)){
          lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes <- NA
        }
        newNote <- paste(sub("\\Orig.*", "", names(standardizeSynonymDF)[4]),
                         "updated",
                         sep = " ")
        lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes <-
          paste(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes,
                newNote,
                sep = "; ")

      }
    }

    if(!all(is.na(metadataChangesDF))){
      for (i in 1:nrow(metadataChangesDF)){
        if(is.null(lipdTS[[as.numeric(metadataChangesDF$rowNum[i])]]$paleoData_notes)){
          lipdTS[[as.numeric(metadataChangesDF$rowNum[i])]]$paleoData_notes <- NA
        }
        #Look at each row of the changesDF for values that are not NA or NULL
        colNames <- names(metadataChangesDF)
        for (j in 2:(ncol(metadataChangesDF))){
          valsChanged <- metadataChangesDF[i,j]
          if (!is.na(valsChanged) & length(valsChanged)>0){
            valsChanged <- colNames[j]
            #valsChanged <- paste(valsChanged, collapse=" ")
            newNote <- paste(valsChanged,
                             "has been standardized",
                             sep = " ")
            lipdTS[[as.numeric(i)]]$paleoData_notes <-
              paste(lipdTS[[as.numeric(i)]]$paleoData_notes,
                    newNote,
                    sep = "; ")
          }
        }
      }
    }


    message("Updated metadata for ", nrow(metadataChangesDF), " TS objects,
            Standardized values for ", nrow(standardizeSynonymDF), " TS Objects")

  }else{


    if(!all(is.na(standardizeSynonymDF))){

      for (i in 1:nrow(standardizeSynonymDF)){
        if(is.null(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes)){
          lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes <- NA
        }
        newNote <- paste(sub("\\Orig.*", "", names(standardizeSynonymDF)[4]),
                         "updated",
                         sep = " ")
        lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes <-
          paste(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes,
                newNote,
                sep = "; ")

      }
    }



  }

  numDelete <- 0
  for (i in 1:length(lipdTS)){
    if(i > length(lipdTS)){
    }else{
      if (lipdTS[[i]]$paleoData_TSid %in% deleteTheseTS){
        numDelete <- numDelete+1
        lipdTS[[i]] <- NULL
      }
    }
  }

  for (i in 1:length(lipdTS)){
    lipdTS[[i]]$paleoData_notes <- sub("^; ", "", lipdTS[[i]]$paleoData_notes)
    lipdTS[[i]]$paleoData_notes <- sub("^NA; ", "", lipdTS[[i]]$paleoData_notes)
    lipdTS[[i]]$Notes <- sub("^; ", "", lipdTS[[i]]$Notes)
    lipdTS[[i]]$Notes <- sub("^NA; ", "", lipdTS[[i]]$Notes)
  }

  message("Updated metadata for ", nrow(metadataChangesDF), " TS objects,\n",
          "Standardized values for ", nrow(standardizeSynonymDF), " TS Objects,\n",
          "Deleted ", numDelete, " objects\n")


  return(lipdTS)
}

getAllTsNames <- function(TS){
  return(sort(unique(unlist(purrr::map(TS,names)))))
}

standardizeAll <- function(TS,allKeys = names(standardTables)){
notesOut <- list()
  an <- getAllTsNames(TS)


  toStandardize <- setdiff(allKeys,"paleoData_proxyGeneral")

  for(tc in toStandardize){

    TS1 <- updateMetaDataFromStandardTables(TS, tc)
    if(sum(unlist(lapply(TS1$TS, function(x) sum(is.na(names(x))))))>0){
      stop("NA is a TS key")
    }
    TS2 <- standardizeValue(lipdTS=TS1$TS, key=tc)
    if(sum(unlist(lapply(TS2, function(x) sum(is.na(names(x))))))>0){
      stop("NA is a TS key")
    }
    TS <- TS2$TS

    if(tc == "interpretation_seasonality"){
      tci <- an[stringr::str_detect(an,"interpretation\\d{1,}_seasonality$")]
    }else if(tc == "interpretation_variable"){
      tci <- an[stringr::str_detect(an,"interpretation\\d{1,}_variable$")]
    }else{
      tci <- tc
    }


    for(tcii in tci){


      if(length(tci) > 1){
        an1 <- purrr::map_chr(TS2$synonymDF,\(x) names(x)[[4]])
        ws <- which(an1 == tcii)

        ssDF <- TS2$returns1[[ws]]$synonymDF
        nsDF <- TS2$returns2[[ws]]$noSynonym
      }else{
        ssDF <- TS2$synonymDF$synonymDF
        nsDF <- TS2$noSynonym$noSynonym
      }


      TS <- updateNotes(key = tcii,
                        lipdTS = TS,
                        metadataChangesDF = TS1$ChangesDF,
                        deleteTheseTS = TS2$deleteTheseTS,
                        standardizeSynonymDF=ssDF)
      if(sum(unlist(lapply(TS, function(x) sum(is.na(names(x))))))>0){
        stop("NA is a TS key")
      }

      notesOut[[tcii]] <- list(metadata.changes = TS1$ChangesDF, deleted.ts = TS2$deleteTheseTS, standardized.synonym = ssDF, noSynonym = nsDF)

      if (length(nsDF)>0){
        message("Could not find valid names for ", nrow(nsDF), " values in ", tcii)
      }


    }
    validNew <- isValidValue(TS, tc)

    # unrec <- sum(unlist(lapply(notesOut, function(x) nrow(x$noSynonym))))
    #
    #
    # if (unrec > 0){
    #   message(unrec, " values not recognized for: ", tc)
    # }
    #
    # if (methods::is(validNew, "list")){
    #   invalid1 <- sum(unlist(lapply(validNew, function(x) nrow(x))))
    # }else{
    #   invalid1 <- nrow(validNew)
    # }
    #
    #
    # if (invalid1 != unrec){
    #   warning(paste0("Some invalid values not accounted for:\n",
    #                  "noSynonym = ", unrec, "\n",
    #                  "invalid = ", invalid1, "\n",
    #                  "for key: ", tc))
    # }





  }
  returns <- list(TS, notesOut)
  return(returns)
}

#' Are all the parameters valid?
#'
#' @param TS lipd-ts object
#' @param report report out? T/F
#' @param allKeys what keys to check
#'
#' @return boolean
#' @export
isValidAll <- function(TS,report = TRUE,allKeys = names(standardTables)){
  vo <- list()
  an <- getAllTsNames(TS)

  toStandardize <- setdiff(allKeys,"paleoData_proxyGeneral")

  for(tc in toStandardize){
    print(tc)
    vo[[tc]] <- isValidValue(TS, tc)
  }

  if(report){
    return(vo)
  }else{
    return(purrr::map_dbl(vo,numInvalid))
  }
}


numInvalid <- function(x){
  if(is(x,"data.frame")){
    return(nrow(x))
  }else{
    return(sum(map_dbl(x,nrow)))
  }
}





