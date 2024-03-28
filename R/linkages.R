#' Build the address to a lipd element from its constituent parts
#'
#' @param pointer list to build address
#'
#' @return full address as string
#'
addressFromList <- function(pointer){
  listIt <- function(x,sep){paste('[[',x,']]',collapse = "",sep=sep)}
  parts <- sapply(pointer, function(x) ifelse(is.numeric(x),listIt(x,""),listIt(x,"'")))
  address <- paste0("L",paste(parts,collapse = "",sep="'"))
  #eval(parse(text=address))
}
incrementPointer <- function(pointer,loc){
    pointer[[loc]] <- pointer[[loc]] + 1
    pointer
}

addressFromListM <- function(pointer,addAddresses = list(),counter=0,branchNow=NULL,success=TRUE){
  zeros <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
  #First round only
  if (counter==0){
    #Find locations of zeros (branch points)
    zeros <- which(unlist(pointer)==0)
    if (is.null(branchNow)){
      branchNow <- max(zeros)
    }
    #Increment all branch points until all are non zero
    while (sum(zeros) > 0){
      branchPoint <- min(zeros)
      pointer <- incrementPointer(pointer,branchPoint)
      zeros <- which(unlist(pointer)==0)
    }
  }
  #print(paste0("zeros: ", zeros))

  #if the last run failed, the branching location should be moved up and the previous branch tip should be restarted at 1
  if(success==FALSE){
    pointer[[branchNow]] <- 1
    branchNow <- zeros[(which(branchNow == zeros)-1)]
    if (length(branchNow)<1){
      return(addAddresses)
    }
    #print(paste0("branchNow: ", branchNow))
    pointer <- incrementPointer(pointer,branchNow)
    #print(paste0("branch Change: ", branchNow))
    addressFromListM(pointer,addAddresses = addAddresses,counter=counter,branchNow=branchNow,success=TRUE)
  }

  #convert the pointer to an address
  address1 <- addressFromList(pointer)
  #If the address points to a valid lipd element...
  if (validAddress(address1)){
    #count the valid address, add it to the list, and print to console
    counter <- counter + 1
    print(paste0("Count: ", counter))
    print(address1)
    addAddresses[[counter]] <- address1
    #note the success to suggest incrementing the branchNow
    pointer <- incrementPointer(pointer,branchNow)
    addressFromListM(pointer,addAddresses=addAddresses,counter=counter,branchNow=branchNow,success=TRUE)
  } else {
    #print("failed, invalid pointer")
    pointer <- incrementPointer(pointer,branchNow)
    #note the failure to suggest decrementing the branchNow value and adjusting a new branch
    addressFromListM(pointer,addAddresses=addAddresses,counter=counter,branchNow=branchNow,success=FALSE)
  }
  return(addAddresses)
}

#' Extract the ID of an element based on its address
#'
#' @param pointer the address of the ID as a list
#'
#' @return the ID
#'
parseID <- function(pointer){
  expr1 <- addressFromList(pointer)
  tryCatch(
    {
      eval(parse(text=expr1))
    },
    error=function(cond) {
      message(conditionMessage(cond))
      NULL
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      NULL
    }
  )
}

#' test a lipd element address for validity
#'
#' @param address1 the address of a lipd element
#'
#' @return the TRUE if valid, else return FALSE
#'
validAddress <- function(address1){
  tryCatch(
    {
      eval(parse(text=address1))
      TRUE
    },
    error=function(cond) {
      message(conditionMessage(cond))
      FALSE
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      FALSE
    }
  )
}

getIdAddress <- function(ID, addressTop){
  address1 <- addressFromList(addressTop)
  IDexists <- sum(unlist(L$paleoData[[1]]$measurementTable[[1]])==ID,na.rm=TRUE)>0
  if (!IDexists){
    message("No such ID in this location")
    NULL
  } else {
    message(paste0("ID: ", ID, " exists"))
  }
}
getIdAddress(ID="093klsimpl-d-f7777d32",addressTop=list("chronData",0,"measurementTable",0,"measurementTableId"))

addressFromList(list("chronData",1,"measurementTable",1,"measurementTableId"))




#' Format/create explicit linkages for various chron/paleoData and measurement tables
#'
#' @param L lipd object
#'
#' @return lipd object
#'
lipdObjectLinkages <- function(L, ask=TRUE){
  #For each object requiring a linked object:
  ##check for ID, create one if needed
  ##find linked object
  ###check for matching ID, fill in if needed

  #Paleo 1,2,3,etc.
  if (length(grep("paleo", attributes(L)$names)) > 0){
    #iterate over paleoData objects
    for (jj in 1:length(L$paleoData)){
      ##paleoDataId
      paleoDataIdAddress <- list("paleoData",jj,"paleoDataId")
      paleoDataId <- parseID(paleoDataIdAddress)
      if (!is.null(paleoDataId)){
        message("has paleoDataId")
        checkLinks(paleoDataIdAddress,L$chronData[[jj]],"linkedPaleoData",ask=ask)
      }
      ##linkedChronData
      if (length(grep("linkedChronData", attributes(L$paleoData[[jj]])$names)) > 0){
        message("has linkedChronData")
        checkLinks(L$paleoData[[jj]],"linkedChronData",L$chronData[[jj]],"chronDataId",ask=ask)
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

validNetotomaID <- function(ID){
  if (is.null(ID)){
    warning("ID is null")
    return(0)
  } else if (length(ID) == 0){
    warning("ID is of length 0")
    return(0)
  } else if (!methods::is(ID, "integer")){
    warning(paste0("class of ID is ", class(ID), " must be class integer"))
    return(0)
  }
  return(1)
}

takeID <- function(name1){
  x <- readline(paste0("What is the value of ", name1, "?"))
  x <- as.integer(x)
  if (!is.integer(x) || is.na(x)){
    message("invalid, enter an integer")
    takeID(name1)
  }
}

checkLinks <- function(pointer1, pointer2 ,auto=TRUE){
  valid1 <- validNetotomaID(loc1[[name1]])
  valid2 <- validNetotomaID(loc2[[name2]])
  #if name1 doesn't exist, create it
  if(!auto){
    auto = askYesNo(paste0(name1,": ", loc1[[name1]], " is missing or of wrong class. Should all unfit IDs be replaced automatically? (Answer 'No' to provide an ID)"))
  }
  ID1 <- round(runif(1,0,100000),0)
  if (!valid1){
    if (ask){
      if (replaceAll){
        loc1[[name1]] <- ID1
      }
    } else {
      loc1[[name1]] <- takeID(name1)
    }

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

checkLinks(L$paleoData[[1]],"linkedChronData",L$chronData[[1]],"chronDataId")


#link any associated chron data

#if there is a default chronology (linked chron data), go find it
if (!is.na(slot(site1@collunits@collunits[[jj]], "defaultchronology"))){
  chronNow <- slot(site1@collunits@collunits[[jj]], "defaultchronology")

  chronNum <- which(chronNow == unlist(lapply(L$paleoData, function(x) x$linkedChronData)))

  if (length(chronNum) < 1){
    message("No chronology found, please make explicit link to chronData.")
    message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
  }
#otherwise, report lack of chron data
} else{
  message("No chronology found, please make explicit link to chronData.")
  message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
}

pointer1 <- list("paleoData",0,"measurementTable",0)

parseID(pointer1)
