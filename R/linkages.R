# The linkages are a set of functions written to handle explicit object relationships, eg. linkedChronData - {points to} - chronDataId
# The high-level functions are:
# checkLinks() - checks for corresponding IDs given their approximate locations
# lipdObjectLinkages() - checks all such linkages with an option to interactively assign missing ones

#' Build the address to a lipd element from its constituent parts
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return full address as string
#'
addressFromList <- function(pointer){

  numbers <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
  zeros <- which(pointer[numbers] == 0)
  if (length(zeros>0)){
    stop("Pointer contains branch points with '0 address', please use addressFromListM()")
  }

  listIt <- function(x,sep){paste('[[',x,']]',collapse = "",sep=sep)}
  parts <- sapply(pointer, function(x) ifelse(is.numeric(x),listIt(x,""),listIt(x,"'")))
  address <- paste0("L",paste(parts,collapse = "",sep="'"))
  address
  #eval(parse(text=address))
}

#' check that an object is a valid pointer
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return TRUE/FALSE
#'
is.pointer <- function(pointer){
  if(is.list(pointer)){
    if (all(sapply(pointer, function(x) is.character(x) || is.numeric(x)))){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


#' check if a pointer has branch points
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return TRUE/FALSE
#'
is.branching.pointer <- function(pointer){
  numbers <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
  zeros <- which(pointer[numbers] == 0)
  if (length(zeros>0)){
    return(TRUE)
  }
  return(FALSE)
}


#' Move to the next object in the list
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#' @param loc location within pointer
#'
#' @return pointer
incrementPointer <- function(pointer,loc){
  tryCatch(
    {
      pointer[[loc]] <- pointer[[loc]] + 1
    },
    error=function(cond) {
      NULL
    },
    warning = function(cond) {
      NULL
    }
  )
    pointer
}

#' Given a generic pointer and its associated lipd object, return all valid addresses
#'
#' @param L lipd object
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return list object with addresses and corresponding values
addressFromListM <- function(L,pointer){
  startTime <- Sys.time()
  if(is.null(L) || !methods::is(L,"lipd")){
    stop("L must be a lipd object!")
  }
  if(!is.pointer(pointer)){
    stop('Pointer is not valid. Must be a list of character and numeric objects. eg. list("paleoData",0,"linkedChronData")')
  }

  if(!is.branching.pointer(pointer)){
    returns <- list(Address=addressFromList(pointer),Ids=parseID(L,pointer))

    return(returns)
  }
  #initiate values
  done <- FALSE
  success=TRUE
  branchNow=NULL
  counter=0
  allAddresses = list()
  allIds = list()
  while (!done){
    if ((Sys.time() - startTime) > 2){
      stop("Problem locating address from pointer. Check that location is valid.")
    }
    numbers <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
    zeros <- which(pointer[numbers] == 0)
    print(numbers)
    print(zeros)
    # if (length(zeros) < 1){
    #   stop("Pointer must contain branch points with '0 Address' to use this function, check pointer")
    # }
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

    #if the last run failed, the branching location should be moved up and the previous branch tip should be restarted at 1
    if(success==FALSE){
      #Reset the previous working branch tip to 1
      pointer[[branchNow]] <- 1
      #Move branchpoint up one
      branchNow <- zeros[(which(branchNow == zeros)-1)]

      #If we find the top-most branch, exit
      if (length(branchNow)<1){
        done <- TRUE
        break
      }
      pointer <- incrementPointer(pointer,branchNow)
      success=TRUE
    }

    #convert the pointer to an address
    address1 <- addressFromList(pointer)
    if (is.null(address1)){
      stop("address1 is null")
    }
    #If the address points to a valid lipd element...
    if (validAddress(L,address1)){
      #count the valid address, add it to the list, and print to console
      counter <- counter + 1
      allAddresses[[counter]] <- address1

      if (is.null(eval(parse(text=address1)))){
        allIds[[counter]] <- NULL
      } else {
        allIds[[counter]] <- eval(parse(text=address1))
      }
      pointer <- incrementPointer(pointer,branchNow)
      #note the success to suggest incrementing the branchNow
      success=TRUE
    } else {
      pointer <- incrementPointer(pointer,branchNow)
      #note the failure to suggest decrementing the branchNow value and adjusting a new branch
      success=FALSE
    }
  }
  returns <- list(Address=allAddresses,Ids=allIds)

  return(returns)
}

#' Extract the ID of an element based on its address
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return the ID
#'
parseID <- function(L,pointer){

  if(is.branching.pointer(pointer)){
    address1 <- addressFromListM(L,pointer)
    expr1 <- address1$Address[[1]]
  } else {
    expr1 <- addressFromList(pointer)
  }
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
#' @param L the lipd object
#'
#' @return the TRUE if valid, else return FALSE
#'
validAddress <- function(L,address1){
  tryCatch(
    {
      try1 <- eval(parse(text=address1))
      if(!is.null(try1)){
        TRUE
      } else {
        FALSE
      }

    },
    error=function(cond) {
      FALSE
    },
    warning = function(cond) {
      FALSE
    }
  )
}

#' get the address given a pointer and the ID
#'
#' @param ID ID at the address
#' @param addressTop where to look within
#'
#' @return address
#'
getIdAddress <- function(ID, addressTop){
  address1 <- addressFromList(addressTop)
  IDexists <- sum(unlist(L$paleoData[[1]]$measurementTable[[1]])==ID,na.rm=TRUE)>0
  if (!IDexists){
    message("No such ID in this location")
    NULL
  } else {
    message(paste0("ID: ", ID, " exists"))
  }
  return(IDexists)
}
# getIdAddress(ID="093klsimpl-d-f7777d32",addressTop=list("chronData",0,"measurementTable",0,"measurementTableId"))
#
# addressFromList(list("chronData",1,"measurementTable",1,"measurementTableId"))




#' Format/create explicit linkages for various chron/paleoData and measurement tables
#' This function addresses the following possible issues that might exist in the linkages:
#' 1. The lipd object may lack linkages altogether
#' 2. The linkage IDs may not be formatted correctly
#' 3. The link IDs may not be paired correctly
#'
#' @param L lipd object
#' @param action one of: c("report","interactive","new","repair"), see checkLinks()
#'
#' @return lipd object
#'
lipdObjectLinkages <- function(L, action="report"){
  if(is.null(L) || !methods::is(L,"lipd")){
    stop("L must be a lipd object!")
  }
  if (!action %in% c("report","interactive","new","repair")){
    stop("'action' must be one of: 'report', 'interactive', 'new', 'repair'")
  }
  #For each object requiring a linked object:
  ##check for ID, create one if needed
  ##find linked object
  ###check for matching ID, fill in if needed

  #Paleo 1,2,3,etc.
  if (length(grep("paleo", attributes(L)$names)) > 0){
    #iterate over paleoData objects
    for (jj in 1:length(L$paleoData)){
      ##paleoDataId may have 1 or more "linkedPaleoData" at L$chronData[[1-n]]
      addressNow <- list("paleoData",jj,"paleoDataId")
      addressLink <- list("chronData",0,"linkedPaleoData")
      links <- checkLinks(L,addressNow,addressLink,action=action)
      # ##linkedChronData
      # if (length(grep("linkedChronData", attributes(L$paleoData[[jj]])$names)) > 0){
      #   message("has linkedChronData")
      #   checkLinks(L$paleoData[[jj]],"linkedChronData",L$chronData[[jj]],"chronDataId",ask=ask)
      # }
      if (length(grep("measurementTable", attributes(L$paleoData[[jj]])$names)) > 0){
        #iterate over measurement tables
        for (kk in 1:length(L$paleoData[[jj]]$measurementTable)){
          ###measurementTableId may have 1 or more "linkedMeasurementTable" at L$chronData[[1-n]]$measurementTable[[1-n]]
          addressNow <- list("paleoData",jj,"measurementTable",kk,"measurementTableId")
          addressLink <- list("chronData",0,"measurementTable",0,"linkedMeasurementTable")
          links <- checkLinks(L,addressNow,addressLink,action=action)
        }
      }
    }
  }


  # #Chron 1,2,3,etc.
  # if (length(grep("chron", attributes(L)$names)) > 0){
  #   #iterate over chronData objects
  #   for (jj in 1:length(L$chronData)){
  #     ##linkedChronData
  #     if (length(grep("linkedChronData", attributes(L)$names)) > 0){
  #       message("has linkedChronData")
  #     }
  #   }
  # }
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

#' verify that a given ID is valid, Neotoma wants an integer
#'
#' @param ID
#'
#' @return 0 or 1
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

#' Get an ID from the user
#'
#' @param name1
#'
#' @returns ID of class integer
takeID <- function(name1){
  x <- readline(paste0("What is the value of ", name1, "?"))
  x <- as.integer(x)
  if (!is.integer(x) || is.na(x)){
    message("invalid, enter an integer")
    takeID(name1)
  } else {
    x
  }
}

#' Find duplicate links
#'
#' @param Ids1 set of Ids
#' @param Ids2 set of ids
#'
#' @return NULL
#'
findDuplicates <- function(Ids1, Ids2){
  if (!validNetotomaID(Ids1) || !validNetotomaID(Ids1)){
    stop("Ids1 and Ids2 must be valid Neotoma Ids")
  }
  for (ii in Ids1){
    iiLinkIndex <- which(ii == Ids2)
    if (length(iiLinkIndex) > 1){
      stop(paste0("Duplicate links for: ", ii))
    }
  }
}


#' Given a location requiring a linkage, check for its counterpart eg. paleoDataId/linkedPaleoData
#' Pointers may be branching (containing 0's) or specific (pointing to 1 particular location)
#'
#' @param L a lipd object
#' @param pointer1 a pointer is a list of character and numeric objects which serve as an address within a lipd
#' @param pointer2 location within the lipd corresponding to pointer1
#' @param action one of: c("report","interactive","new","repair")
#'
#' @details
#' action options:
#' report: print to console
#' interactive: ask user to fill any missing IDs
#' new: generate new,random IDs based on positions in the lipd structure, eg. L$paleoData$[[1]]$paleoDataId and L$chronData$[[1]]$linkedPaleoData would be assigned the same IDs
#' repair: asssumes the pairing of IDs is correct, but the IDs are in the wrong format, and assigns new, random integers
#'
#' @return an updated lipd object
checkLinks <- function(L, pointer1, pointer2, action="report"){
  if(is.null(L) || !methods::is(L,"lipd")){
    stop("L must be a lipd object!")
  }
  if(!is.pointer(pointer1) || !is.pointer(pointer2)){
    stop('Pointer(s) not valid. Must be a list of character and numeric objects. eg. list("paleoData",0,"linkedChronData")')
  }
  if (!action %in% c("report","interactive","new","repair")){
    stop("'action' must be one of: 'report', 'interactive', 'new', 'repair")
  }
  #get the address from the pointers, or return FALSE if it doesn't exist
  #pointer1
  if(is.branching.pointer(pointer1)){
    address1 <- addressFromListM(L,pointer1)
    if (length(address1$address<1)){
      message(paste0(pointer1[length(pointer1)]," does not exist at ", paste0(pointer1,collapse = " ")))
      return(FALSE)
    } else {
      address1 <- address1$Address[[1]]
      ID1 <- address1$Ids[[1]]
    }
  } else {
    address1 <- addressFromList(pointer1)
    ID1 <- parseID(L,pointer1)
  }
  #pointer2
  if(is.branching.pointer(pointer2)){
    address2 <- addressFromListM(L,pointer2)
    if (length(address2$address<1)){
      message(paste0(pointer2[length(pointer2)]," does not exist at ", paste0(pointer2,collapse = " ")))
      return(FALSE)
    } else {
      address2 <- address2$Address[[1]]
      ID2 <- address2$Ids[[1]]
    }
  } else {
    address2 <- addressFromList(pointer2)
    ID2 <- parseID(L,pointer2)
  }


  if(is.null(parseID(L, pointer1))){
    message(paste0(pointer1[length(pointer1)]," does not exist at ", address1))
    return(FALSE)
  }
  if(is.null(parseID(L, pointer2))){
    message(paste0(pointer2[length(pointer2)]," does not exist ", address2))
    return(FALSE)
  }
  addresses1 <- addressFromListM(L,pointer1)
  addresses2 <- addressFromListM(L,pointer2)
  for (ii in 1:length(addresses1$Ids)){
    iiLinkIndex <- which(unlist(addresses1$Ids[ii]) == unlist(addresses2$Ids))
    if (length(iiLinkIndex) < 1){
      message(paste0("No corresponding links for '", addresses1$Ids[ii], "' from ", addresses1$Address[ii], " located in ", paste0(unlist(addresses2$Address),collapse=", ")))
      return(FALSE)
    }
  }
  return(TRUE)
}
