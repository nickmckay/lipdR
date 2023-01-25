#
# load("C:/Users/dce25/Downloads/iso2k1_0_1.RData")
# #load("C:/Users/dce72/Downloads/iso2k1_0_1.RData")
# key="paleoData_variableName"
# lipdTS <- TS


#' grab metaData for given key from standard tables
#'
#' @param lipdTS
#' @param key
#'
#' @return list
#' @export
updateMetaDataFromStandardTables <- function(lipdTS, key){

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
  }else(
    return(NULL)
  )

  #Check that the metadata keys exist, if not, add them
  if(!sum(unlist(lapply(lipdTS, function(x) all(meta_keys %in% names(x))))) == length(lipdTS)){
    #add keys where necessary
    for (i in 1:length(lipdTS)){
      needAdded <- meta_keys[!meta_keys %in% names(lipdTS[[i]])]
      for (j in 1:length(needAdded)){
        lipdTS[[i]][eval(needAdded[j])] <- NA
      }
    }
  }

  #update metadata
  #move through each TS, for each metadata field
  for (i in 1:length(lipdTS)){
    #find TS synonym in spreadsheet
    synonymLoc <- which(unlist(unname(lipdTS[[i]][eval(key)])) == standardTables[[eval(key)]]['synonym'])
    for (j in length(meta_keys)){
      #If the field is blank, check the spreadsheet for metadata based on the synonym in the TS
      if (lipdR:::is_blank(lipdTS[[i]][eval(meta_keys[j])])){
        lipdTS[[i]][eval(meta_keys[j])] <- standardTables[[eval(key)]][synonymLoc,eval(meta_keys[j])]
      }
    }
  }

  #compare original TS to new TS
  compDF <- data.frame(matrix(nrow = length(lipdTS), ncol = length(meta_keys)))
  names(compDF) <- meta_keys
  for (i in 1:length(lipdTS)){
    for (j in 1:length(unlist(TSorig[[i]][eval(meta_keys)]))){
      currentKey <- eval(names(unlist(TSorig[[i]][eval(meta_keys)])))[j]
      compDF[i,eval(currentKey)] <-
        unlist(lipdTS[[i]][eval(currentKey)]) == unlist(TSorig[[i]][eval(currentKey)])
    }
    # if (length(comps)==0){
    #   compDF[i,] <- rep(NA, length(meta_keys))
    # }else{
    #   if (length(comps) != ncol(compDF)){
    #     stop("Problem at: ", i, " in compDF due to comps = ", paste(comps, collapse = ", "))
    #   }
    #   compDF[i,] <- comps
    # }
  }

  returns <- list("TS" = lipdTS, "ChangesDF" = compDF)

  return(returns)

}




#' make sure all terms under a controlled key are valid
#'
#' @param lipdTS
#' @param key
#'
#' @return invalid keys df
#' @export

isValidValue <- function(lipdTS, key = NA){
  if (!key %in% names(standardTables)){
    stop(paste("key must be one of: ", paste(names(standardTables),collapse = ", ")))
  }

  #Checking function
  #check the validity in the appropriate spreadsheet
  ckeckKey <- function(lipdTS, key){
    TSvals <- pullTsVariable(lipdTS, key)


    validCheck <- unlist(lapply(tolower(TSvals), function(x) x %in%
                           tolower(unname(unlist(standardTables[[eval(key)]]["lipdName"]))))) |
      unlist(lapply(tolower(TSvals), function(x) is.na(x))) |
      unlist(lapply(tolower(TSvals), function(x) is.null(x)))

    message("Found ", sum(!unlist(validCheck)), " invalid keys from ", length(unlist(validCheck)), " total entries for ", key)

    invalidDF <- as.data.frame(matrix(nrow = sum(!unlist(validCheck)), ncol = 5))
    countA <- 0
    names(invalidDF) <- c("rowNum", "TSid", "dataSetName", "dataSetId", eval(key))
    for (i in which(!unlist(validCheck))){
      countA <- countA + 1
      dataFill <- list(i, lipdTS[[i]]$paleoData_TSid, lipdTS[[i]]$dataSetName, lipdTS[[i]]$datasetId, TSvals[i])
      dataFill <- lapply(dataFill, function(x) if(is.null(x)){x=NA}else{x=x})
      for (j in 1:5) {
        invalidDF[countA,j] <- dataFill[[j]]
      }
    }
    return(invalidDF)
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

    #how many of these keys exist for each record, save the max number
    #numKeysMax <- max(unlist(lapply(keysAll, function(x) length(x))))

    uniqueKeys <- unique(unlist(keysAll))

    #run check for each unique, eg. interpretation1_seasonality
    returns <- lapply(uniqueKeys, function(x) ckeckKey(lipdTS, x))
  }else{
    returns <- ckeckKey(lipdTS, key)
  }

  return(returns)
}




#' standardize terms automatically based on known synonyms
#'
#' @param lipdTS
#' @param key
#'
#' @return updated TS
#' @export

standardizeValue <- function(lipdTS, key = NA){
  lipdTs <- as.lipdTs(lipdTs)

  invalidDF <- isValidValue(lipdTS, key)

  possibleSynonyms <- unique(c(tolower(unname(unlist(standardTables[[eval(key)]]["synonym"]))),
                        tolower(unname(unlist(standardTables[[eval(key)]]["paleoData_pastName"])))))
  possibleSynonyms <- possibleSynonyms[!is.na(possibleSynonyms)]

  validCheck <- lapply(lipdTS, function(x) tolower(unname(unlist(x[eval(key)]))) %in%
                         tolower(unname(unlist(standardTables[[eval(key)]]["lipdName"]))))

  synonymDF <- as.data.frame(matrix(nrow = sum(!unlist(validCheck)), ncol = 5))
  names(synonymDF) <- c("rowNum", "dataSetName", "dataSetId", paste0(eval(key), "Orig"), paste0(eval(key), "New"))
  for (i in 1:nrow(invalidDF)){
    TSrowNum <- invalidDF$rowNum[[i]]

    currentTS <- lipdTS[[TSrowNum]]

    synonymLoc <- which(tolower(invalidDF[i,4]) == possibleSynonyms)

    if(length(synonymLoc) == 0){
      lipdName <- NA
    }else{
      #find the known synonym
      synonym <- possibleSynonyms[synonymLoc]

      #locate the corresponding lipdName
      synonymTableLoc <- which(synonym == tolower(unname(unlist(standardTables[[eval(key)]]["synonym"]))))
      lipdName <- unname(unlist(standardTables[[eval(key)]]["lipdName"]))[synonymTableLoc]



    }


    synonymDF[i,] <- c(TSrowNum, lipdTS[[as.numeric(TSrowNum)]]$dataSetName, lipdTS[[as.numeric(TSrowNum)]]$datasetId, invalidDF[i,4], lipdName)
  }

  synonymDForig <- synonymDF
  print(synonymDF)

  userResp <- askYesNo(msg = "Replace all?" , default = TRUE)

  if(userResp){
    for (i in 1:nrow(synonymDF)){
      lipdTS[[as.numeric(synonymDF$rowNum[i])]][eval(key)] <- synonymDF$archiveTypeNew[i]
    }
  }


  message("Rerunning check for invalid keys in: ", key)
  invalidDFnew <- isValidValue(lipdTS, key)
  print(invalidDFnew)

  returns <- list("TS" = lipdTS, "synonymDF" = synonymDForig)

  return(returns)
}




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


updateNotes <- function(lipdTS, metadataChangesDF, standardizeSynonymDF){
  #choose the appropriate notes
  #for each invalid value from metadataChangesDF, update notes
  if (sub("\\_.*", "", names(standardizeSynonymDF)[4]) == "paleoData") {

    #note values standardized
    for (i in 1:nrow(standardizeSynonymDF)){
      lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes <-
        paste(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$paleoData_notes,
              sub("\\Orig.*", "", names(standardizeSynonymDF)[4]),
              "updated",
              sep = " ")

    }


    #note metadata updated
      for (i in 1:nrow(metadataChangesDF)){

        valsChanged <- metadataChangesDF[i,is.na(metadataChangesDF[i,]) | !metadataChangesDF[i,]]
        valsChanged <- names(valsChanged)
        valsChanged <- paste(valsChanged, collapse=" ")
        lipdTS[[as.numeric(i)]]$paleoData_notes <-
          paste(lipdTS[[as.numeric(i)]]$paleoData_notes,
                valsChanged,
                "have been standardized",
                sep = ", ")
      }

    message("Updated metadata for ", nrow(metadataChangesDF), " TS objects,
            Standardized values for ", nrow(standardizeSynonymDF), " TS Objects")

  }else{

    for (i in 1:nrow(standardizeSynonymDF)){
      lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes <-
        paste(lipdTS[[as.numeric(standardizeSynonymDF$rowNum[i])]]$notes,
              sub("\\Orig.*", "", names(standardizeSynonymDF)[4]),
              "updated",
              sep = " ")
    }
    #note metadata updated
    for (i in 1:nrow(metadataChangesDF)){

      valsChanged <- metadataChangesDF[i,is.na(metadataChangesDF[i,]) | !metadataChangesDF[i,]]
      valsChanged <- names(valsChanged)
      valsChanged <- paste(valsChanged, collapse=" ")
      lipdTS[[as.numeric(i)]]$notes <-
        paste(lipdTS[[as.numeric(i)]]$notes,
              valsChanged,
              "have been standardized",
              sep = ", ")

    }

    message("Updated metadata for ", nrow(metadataChangesDF), " TS objects,
          Standardized values for ", nrow(standardizeSynonymDF), " TS Objects")
  }



  return(lipdTS)
}











