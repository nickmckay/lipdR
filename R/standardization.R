#' Auto-update Standard tables
#'
checkStandardTables <- function(){
  if(.standardTables$tablesUpdated == 0){
    ans1 <- askYesNo("Would you like to update the standard tables (recommended)?")
    if(ans1){
      updateStandardTables()
    }

    .standardTables$tablesUpdated <- 1
  }
}




#' grab metaData for given key from standard tables
#'
#' @param lipdTS
#' @param key
#'
#' @return list
#' @export
updateMetaDataFromStandardTables <- function(lipdTS, key){

  checkStandardTables()


  TSorig <- lipdTS


  #Check for appropriate key
  if (!key %in% names(standardTables)){
    stop(paste("key must be one of: ", paste(names(standardTables),collapse = ", ")))
  }

  #Which metadata needs updated for the given key?
  if (key == "paleoData_variableName"){
    meta_keys <- c("paleoData_isAssemblage", "paleoData_datum", "paleoData_summaryStatistic", "paleoData_measurementMaterial",
                   "paleoData_inferrredMaterial", "paleoData_method", "paleoData_isPrimary")
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
      for (j in 1:length(needAdded)){
        addNow <- needAdded[j]
        lipdTS[[i]][eval(addNow)] <- NA
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
      if (lipdR:::is_blank(lipdTS[[i]][eval(meta_keys[j])])){
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
#' @param lipdTS
#' @param key
#'
#' @return invalidDF
#'
ckeckKey <- function(lipdTS, key, keyGeneral){
  TSvals <- pullTsVariable(lipdTS, key)

  numVals <- length(TSvals)

  validCheck <- unlist(lapply(tolower(TSvals), function(x) x %in%
                                tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["lipdName"]))))) |
    unlist(lapply(tolower(TSvals), function(x) is.na(x))) |
    unlist(lapply(tolower(TSvals), function(x) is.null(x)))


  numInvalid <- sum(!unlist(validCheck))
  message("Found ", numInvalid, " invalid keys from ", length(unlist(validCheck)), " total entries for ", key, "\n")

  invalidDF <- as.data.frame(matrix(nrow = , ncol = 5))
  countA <- 0
  names(invalidDF) <- c("rowNum", "TSid", "dataSetName", "dataSetId", eval(key))
  for (i in which(!unlist(validCheck))){
    numVals
    countA <- countA + 1
    dataFill <- list(i, lipdTS[[i]]$paleoData_TSid, lipdTS[[i]]$dataSetName, lipdTS[[i]]$datasetId, TSvals[i])
    dataFill <- lapply(dataFill, function(x) if(is.null(x)){x=NA}else{x=x})
    for (j in 1:5) {
      invalidDF[countA,j] <- dataFill[[j]]
    }
  }
  return(invalidDF)
}



#' make sure all terms under a controlled key are valid
#'
#' @param lipdTS
#' @param key
#'
#' @return invalid keys df
#' @export

isValidValue <- function(lipdTS, key = NA){

  checkStandardTables()

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
    returns <- lapply(uniqueKeys, function(x) ckeckKey(lipdTS, x, keyGeneral))
  }else{

    returns <- ckeckKey(lipdTS, key, key)
  }

  # if (!verbose){
  #   returns <- NULL
  # }

  invisible(returns)
}




#' standardize terms automatically based on known synonyms
#'
#' @param lipdTS
#' @param key
#'
#' @return updated TS
#' @export

standardizeValue <- function(lipdTS, key = NA){


  checkStandardTables()

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
      for (i in 1:nrow(invalidDF)){

        TSrowNum <- invalidDF$rowNum[[i]]

        currentTS <- lipdTS[[TSrowNum]]

        synonymLoc <- which(tolower(invalidDF[i,5]) == possibleSynonyms)

        if(length(synonymLoc) == 0){
          lipdName <- NA
        }else{
          #find the known synonym
          synonym <- possibleSynonyms[synonymLoc]

          #locate the corresponding lipdName
          synonymTableLoc <- which(synonym == tolower(unname(unlist(standardTables[[eval(keyGeneral)]]["synonym"]))))
          lipdName <- unname(unlist(standardTables[[eval(keyGeneral)]]["lipdName"]))[synonymTableLoc[1]]

        }

        #print(invalidDF[i,5])
        #newRow <- c(TSrowNum, lipdTS[[as.numeric(TSrowNum)]]$dataSetName, lipdTS[[as.numeric(TSrowNum)]]$datasetId, invalidDF[i,5], lipdName)

        # if (length(newRow) != 5){
        #   stop("Problem with TS ", newRow, " for ", key)
        # }

        synonymDF[i,1] <- TSrowNum
        synonymDF[i,2] <- lipdTS[[as.numeric(TSrowNum)]]$dataSetName
        synonymDF[i,3] <- lipdTS[[as.numeric(TSrowNum)]]$datasetId
        synonymDF[i,4] <- invalidDF[i,5]
        synonymDF[i,5] <- lipdName

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
      #   invalidDFnew <- ckeckKey(lipdTS, key)
      #   print(tibble::tibble(invalidDFnew))
      # }

      returns <- list("synonymDF" = synonymDForig)
    }else{
      returns <- NULL
    }


    return(returns)
  }

  returns <- list()
  if (interpretation){
    for (i in 1:length(invalidDF)){
      returns[[i]] <- replaceSynonyms(lipdTS, key=names(invalidDF[[i]])[5], invalidDF[[i]], interpretation)
    }
    #returns <- lapply(invalidDF, function(x) replaceSynonyms(lipdTS, key=names(x)[5], x, interpretation))
  }else{
    returns <- replaceSynonyms(lipdTS, key, invalidDF, interpretation)
  }

  deleteTheseTS <- c()

  if (length(returns)> 1){
    for (i in 1:length(returns)){
      df1 <- returns[[i]]$synonymDF
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
    for (j in 1:nrow(returns[[1]])){
      rowNum <- returns[[1]]$rowNum[j]
      newVal <- returns[[1]]$New[j]
      if(!is.na(rowNum) & length(rowNum) > 0){
        if (grepl("delete", newVal)){
          if(grepl("variableName", key)){
            deleteTheseTS <- c(deleteTheseTS, lipdTS[[as.numeric(rowNum)]]$paleoData_TSid)
          }else{
            lipdTS[[as.numeric(rowNum)]][eval(names(returns[[1]])[4])] <- NULL
          }
        }else{
          lipdTS[[as.numeric(rowNum)]][eval(names(returns[[1]])[4])] <- newVal
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





  returns <- list("TS"=lipdTS, "synonymDF" = returns, "deleteTheseTS" = deleteTheseTS)

  return(returns)
}


#Delete misnamed terms and TS with bad variableName




#Update notes based on results from `updateMetaDataFromStandardTables()` and `standardizeValue()`


#metadataChangesDF <- updateMetaDataFromStandardTables(TS, "paleoData_variableName")$ChangesDF
#standardizeSynonymDF <- standardizeValue(TS, "paleoData_variableName")$synonymDF
#updateNotes(TS, metadataChangesDF, standardizeSynonymDF)

#' Update notes and paleoData_notes
#'
#' @param lipdTS
#' @param metadataChangesDF
#' @param standardizeSynonymDF
#'
#' @return lipdTS


updateNotes <- function(lipdTS, key=NA, metadataChangesDF=NA, standardizeSynonymDF=NA, deleteTheseTS=NA){

  checkStandardTables()

  #choose the appropriate notes
  #for each invalid value from metadataChangesDF, update notes
  if(is.na(key)){
    if(all(is.na(standardizeSynonymDF))){
      stop("Must explicitly enter key if not entering standardizeSynonymDF!")
    }
    key=names(standardizeSynonymDF)[4]
  }
  if(!all(is.na(standardizeSynonymDF))){
    standardizeSynonymDF <-
      standardizeSynonymDF[apply(standardizeSynonymDF,
                                 1,
                                 function(x) sum(is.na(x)))!=ncol(standardizeSynonymDF),]
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
        #cat("deleting: ", lipdTS[[i]]$paleoData_TSid, "\n")
        lipdTS[[i]] <- NULL
      }
    }
  }

  message("Updated metadata for ", nrow(metadataChangesDF), " TS objects,\n",
          "Standardized values for ", nrow(standardizeSynonymDF), " TS Objects,\n",
          "Deleted ", numDelete, " objects\n")


  return(lipdTS)
}

getAllTsNames <- function(TS){
  return(sort(unique(unlist(purrr::map(TS,names)))))
}

standardizeAll <- function(TS){

  an <- getAllTsNames(TS)



  allKeys <- googlesheets4::read_sheet("16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY")

  toStandardize <- setdiff(allKeys$name,"paleoData_proxyGeneral")

  for(tc in toStandardize){

    TS1 <- updateMetaDataFromStandardTables(TS, tc)
    TS2 <- standardizeValue(lipdTS=TS1$TS, key=tc)
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
        an <- map_chr(TS2$synonymDF,\(x) names(x$synonymDF)[[4]])
        ws <- which(an == tcii)

        ssDF <- TS2$synonymDF[[ws]]$synonymDF
      }else{
        ssDF <- TS2$synonymDF[[1]]
      }

      TS <- updateNotes(key = tcii,
                        lipdTS = TS,
                        metadataChangesDF = TS1$ChangesDF,
                        standardizeSynonymDF=ssDF)
    }
    isValidValue(TS, tc)

  }
  return(TS)
}






