# example:
# qcStand <- standardizeQCsheetValues(googlesheets4::read_sheet("1k3PZTGZ1n1eljVbXx9qR-PtQ-7LIdj-mi7wtEmzu2iM"))
# writeNewQCsheet <- function(qcID = "1k3PZTGZ1n1eljVbXx9qR-PtQ-7LIdj-mi7wtEmzu2iM",
# qcStand = qcStand,
# newSheetName = "standardizedValues")

#' standardizeQCsheetValues
#'
#' @param qcSheet google qc sheet id
#'
#' @return stnadardized qc sheet
#' @export
standardizeQCsheetValues <- function(qcSheet){
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop(
      "Package 'googlesheets4' must be installed to use this function.",
      call. = FALSE
    )
  }
  allKeys <- names(standardTables)

  standardKeyNames <- setdiff(allKeys,"paleoData_proxyGeneral")

  tsNames <- sub("interpretation", "climateInterpretation1", standardKeyNames)

  keyConversion <- googlesheets4::read_sheet("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w")

  #QCkeys and corresponding TSName and corresponding standardKey in DF
  keyConDF <- keyConversion[keyConversion$tsName %in% tsNames,c(1,2)]
  keyConDF$standKey <- standardKeyNames[unlist(lapply(keyConDF$tsName, function(x) which(x == sub("interpretation", "climateInterpretation1", standardKeyNames))))]
  notes <- rep(NA, times = nrow(qcSheet))
  remainingInvalid <- list()

  standardizedTerms <- data.frame(matrix(nrow = nrow(qcSheet), ncol = length(standardKeyNames)))
  names(standardizedTerms) <- keyConDF$qcSheetName
  #for each key, standardize terms in qc sheet
  for (nm in standardKeyNames){
    #load corresponding standard table
    tableNow <- standardTables[eval(nm)][[1]]
    QCKeyNow <- keyConDF$qcSheetName[keyConDF$standKey == eval(nm)]
    QCTermsNow <- unname(unlist(qcSheet[eval(QCKeyNow)]))
    numTerms <- length(QCTermsNow)
    if (numTerms != nrow(qcSheet)){
      stop("number of terms for ", nm, ": ", numTerms, ", not equal to number of rows in QC sheet: ", nrow(qcSheet))
    }
    #check validity of the terms
    if (nm %in% c("archiveType", "paleoData_variableName")){
      isValid <- unlist(lapply(QCTermsNow, function(x) x %in% tableNow$lipdName))
    }else{
      isValid <- unlist(lapply(QCTermsNow, function(x) is.na(x) | x %in% tableNow$lipdName))
    }
    numInvalid <- sum(!isValid)
    message("\n\nFound ", numInvalid, " invalid terms of ", numTerms, " total for ", nm)
    #Checking for known synonym
    if (numInvalid > 0){
        hasSynonym <- unlist(lapply(QCTermsNow[!isValid], function(x) x %in% tableNow$synonym))
        #synonymLoc <- unlist(lapply(QCTermsNow[!isValid], function(x) which(x == tableNow$synonym)))

      message("Of the ", numInvalid, " invalid terms ", sum(!hasSynonym), " have no known synonym")
      message("Replacing ", sum(hasSynonym), " terms with valid lipd names")
      #for vector of invalid terms, replace each invalid with valid
      invalidQcTermIndex <- which(!isValid)
      for (i in 1:sum(!isValid)){
        if (hasSynonym[i]){
          synonymLoc <- which(QCTermsNow[invalidQcTermIndex[i]] == tableNow$synonym)
          QCTermsNow[invalidQcTermIndex[i]] <- tableNow$lipdName[synonymLoc]
          notes[invalidQcTermIndex[i]] <- paste(notes[invalidQcTermIndex[i]], paste0(nm, " updated"), sep = ", ")
        }
      }
      #check validity of the terms
      if (nm %in% c("archiveType", "paleoData_variableName")){
        isValid <- unlist(lapply(QCTermsNow, function(x) x %in% tableNow$lipdName))
      }else{
        isValid <- unlist(lapply(QCTermsNow, function(x) is.na(x) | x %in% tableNow$lipdName))
      }
      numInvalid <- sum(!isValid)
      total <- length(isValid)
      if (numInvalid >0){
        warning("For ", nm," ", numInvalid, " invalid terms remain\n")


        if("datasetId" %in% names(qcSheet)){
          remainingInvalid[[eval(nm)]] <- data.frame(rowNum = which(!isValid),
                                                     TSid = qcSheet$TSid[!isValid],
                                                     dataSetName = qcSheet$dataSetName[!isValid],
                                                     datasetId = qcSheet$datasetId[!isValid],
                                                     keyTBD = QCTermsNow[!isValid])
        }else{
          remainingInvalid[[eval(nm)]] <- data.frame(rowNum = which(!isValid),
                                                     TSid = qcSheet$TSid[!isValid],
                                                     dataSetName = qcSheet$dataSetName[!isValid],
                                                     datasetId = rep(NA, sum(!isValid)),
                                                     keyTBD = QCTermsNow[!isValid])
        }




      }else{
        message("No remaining invalid terms for ", nm)
      }
      if(!is.null(names(remainingInvalid[[nm]])[5])){
        names(remainingInvalid[[nm]])[5] <- nm
      }
    }

    standardizedTerms[eval(QCKeyNow)] <- QCTermsNow


  }

  newSheet <- qcSheet
  for (jj in keyConDF$qcSheetName){
    newSheet[eval(jj)] <- standardizedTerms[jj]
  }
  newSheet$standardizationNotes <- notes


  returns <- list("standardizedTerms" = standardizedTerms,
                  "notes"=notes,
                  "remainingInvalid" = remainingInvalid,
                  "newSheet" = newSheet)



  for (i in 1:length(returns$notes)){
    returns$notes[[i]] <- sub("^NA, ", "", returns$notes[[i]])
  }

  newSheet$standardizationNotes <- notes

  return(returns)
}

writeNewQCsheet <- function(qcID, qcStand, newSheetName = "standardizedValues"){
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' must be installed to use this function.",
      call. = FALSE
    )
  }
  googlesheets4::sheet_add(ss=qcID, sheet = newSheetName)
  googlesheets4::write_sheet(qcStand$newSheet, ss=qcID, sheet = newSheetName)
}


