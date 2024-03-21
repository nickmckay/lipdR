#' Format/create explicit linkages for various chron/paleoData and measurement tables
#'
#' @param L lipd object
#'
#' @return lipd object
#'
lipdObjectLinkages <- function(L){
  #For each object requiring a linked object:
  ##check for ID, create one if needed
  ##find linked object
  ###check for matching ID, fill in if needed

  #Paleo 1,2,3,etc.
  if (length(grep("paleo", attributes(L)$names)) > 0){
    #iterate over paleoData objects
    for (jj in 1:length(L$paleoData)){
      ##paleoDataId
      if (length(grep("paleoDataId", attributes(L$paleoData[[jj]])$names)) > 0){
        message("has paleoDataId")
      }
      ##linkedChronData
      if (length(grep("linkedChronData", attributes(L$paleoData[[jj]])$names)) > 0){
        message("has linkedChronData")
      }
      if (length(grep("measurementTable", attributes(L$paleoData[[jj]])$names)) > 0){
        #iterate over measurement tables
        for (kk in 1:length(L$paleoData[[jj]]$measurementTable)){
          ###measurementTableId
          if (length(grep("measurementTableId", attributes(L$paleoData[[jj]]$measurementTable[[kk]])$names)) > 0){
            message("has measurementTableId")
          }
          ###linkedMeasurementTable
          if (length(grep("linkedMeasurementTable", attributes(L$paleoData[[jj]]$measurementTable[[kk]])$names)) > 0){
            message("has linkedMeasurementTable")
          }
        }
      }
    }
  }





  #Chron 1,2,3,etc.
  if (length(grep("chron", attributes(L)$names)) > 0){
    #iterate over chronData objects
    for (jj in 1:length(L$chronData)){
      ##linkedChronData
      if (length(grep("linkedChronData", attributes(L)$names)) > 0){
        message("has linkedChronData")
      }
    }
  }
  ##Measurement Table 1,2,3,etc.
  ###measurementTableId
  ###linkedMeasurementTable
  ###linkedModel
  ###linkedSummaryTable
  ###linkedDistributionTable
  ###linkedEnsembleTable
  ##Model 1,2,3,etc.
  ###modelId
  ###Summary Table 1,2,3,etc.
  ####summaryTableId
  ###Ensemble Table 1,2,3,etc.
  ####ensembleTableId
  ###Distribution Table 1,2,3,etc.
  ####distributionTableId
}

takeID <- function(name1){
  x <- readline(paste0("What is the value of ", name1, "?"))
  x <- as.integer(x)
  if (!is.integer(x) || is.na(x)){
    message("invalid, enter an integer")
    takeID(name1)
  }
}

checkLinks <- function(loc1, name1, loc2, name2,ask=TRUE){
  #if name1 doesn't exist, create it
  replace1 = TRUE
  if (length(grep(name1, attributes(loc1)$names)) == 0 || !methods::is(loc1[[name1]], "integer")){
    if (ask){
      replaceAll = askYesNo(paste0(name1,": ", loc1[[name1]], " is missing or of wrong class. Should all unfit IDs be replaced automatically? (Answer 'No' to provide an ID)"))
    }
    if (replaceAll){
      ID1 <- round(runif(1,0,100000),0)
      loc1[[name1]] <- ID1
    }
  } else {
    x <- takeID(name1)
  }
  #if name2 doesn't exist, check elsewhere
  if (length(grep(name2, attributes(loc2)$names)) == 0 || !methods::is(loc2[[name2]], "integer")){

    loc2[[name2]] <- ID1


  }

  if (loc1[[name1]] == loc2[[name2]]){
    message("fields match")
  }

  if (methods::is(loc1[[name1]], "integer") && methods::is(loc2[[name2]], "integer")){
    message("linkage check passed!")
  } else {
    message("objects are not class integer")
  }
}

library(lipdR)

L <- readLipd("C:\\Users\\dce25\\Downloads\\093kliso_simpl.lpd")
L <- readLipd("C:\\Users\\dce25\\Downloads\\Arc-GRIP.Vinther.2010.lpd")

rapply(L, function(x) grepl(x,"link"),how = "unlist")


#link any associated chron data

#if there is a default chronology (linked chron data), go find it
if (!is.na(slot(site1@collunits@collunits[[jj]], "defaultchronology"))){
  chronNow <- slot(site1@collunits@collunits[[jj]], "defaultchronology")

  chronNum <- which(chronNow == unlist(lapply(L$paleoData, function(x) x$linkedChronData)))

  if (length(chronNum) < 1){
    message("No chronology found, please make explicit link to chronData.")
    message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
  }
}#otherwise, report lack of chron data
else{
  message("No chronology found, please make explicit link to chronData.")
  message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
}
