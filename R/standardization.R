
#grab metaData for key
getValueMetaData <- function(lipdTS, key){

  if (!key %in% names(standardTables)){
    stop("key must be one of: ", paste(names(standardTables)[1], names(standardTables)[2],
                                       names(standardTables)[3], names(standardTables)[4],
                                       names(standardTables)[5],  sep = " "))
  }

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



  standardTables





  tolower(unname(unlist(standardTables[[eval(key)]][eval(meta_keys)])))
  standardTables[[eval(key)]][eval(meta_keys)]

  data.frame(standardTables[[eval(key)]][eval(meta_keys)])

  View(standardTables$paleoData_units)
}




#check standardized keys for valid terms
#function includes input for which key to check

#' make sure all terms under a controlled key are valied
#'
#' @param lipdTS
#' @param key
#'
#' @return invalid keys df
#' @export

isValidValue <- function(lipdTS, key){
  if (!key %in% names(standardTables)){
    stop("key must be one of: ", paste(names(standardTables)[1], names(standardTables)[2],
                 names(standardTables)[3], names(standardTables)[4],
                 names(standardTables)[5],  sep = " "))
  }

  validCheck <- lapply(lipdTS, function(x) tolower(unname(unlist(x[eval(key)]))) %in%
                         tolower(unname(unlist(standardTables[[eval(key)]]["lipdName"]))))

  message("Found ", sum(!unlist(validCheck)), " invalid keys from ", length(unlist(validCheck)), " total entries for ", key)

  invalidDF <- as.data.frame(matrix(nrow = sum(!unlist(validCheck)), ncol = 4))
  countA <- 0
  names(invalidDF) <- c("rowNum", "dataSetName", "dataSetId", eval(key))
  for (i in which(!unlist(validCheck))){
    countA <- countA + 1
    invalidDF[countA,] <- c(i, lipdTS[[i]]$dataSetName, lipdTS[[i]]$datasetId, unlist(unname(lipdTS[[i]][eval(key)])))
  }
  return(invalidDF)
}

#

#' standardize terms automatically based on known synonyms
#'
#' @param lipdTS
#' @param key
#'
#' @return updated TS
#' @export

standardizeValue <- function(lipdTS, key){

  #update past info



  #update metadata



  invalidDF <- isValidValue(lipdTS, key)

  possibleSynonyms <- unique(c(tolower(unname(unlist(standardTables[[eval(key)]]["synonym"]))),
                        tolower(unname(unlist(standardTables[[eval(key)]]["pastName"])))))
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

  return(lipdTS)
}










