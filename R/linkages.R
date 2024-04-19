#' convert pointer list object into lipd address form
#'
#' @param pointer
#'
#' @return address
#'
addressForm <- function(pointer){
  listIt <- function(x,sep){paste('[[',x,']]',collapse = "",sep=sep)}
  parts <- sapply(pointer, function(x) ifelse(is.numeric(x),listIt(x,""),listIt(x,"'")))
  assembled <- paste(parts,collapse = "",sep="'")
  return(assembled)
}

# The linkages are a set of functions written to handle explicit object relationships, eg. linkedChronData - {points to} - chronDataId
# The high-level functions are:
# checkLinks() - checks for corresponding IDs given their exact or approximate locations
# lipdObjectLinkages() - checks all such linkages from expected relationships

#' Build the address to a lipd element from its constituent parts
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return full address as string
#'
addressFromList <- function(L,pointer){

  numbers <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
  zeros <- which(pointer[numbers] == 0)
  if (length(zeros>0)){
    stop("Pointer contains branch points with '0 address', please use addressFromListM()")
  }

  parts <- addressForm(pointer)
  address <- paste0("L",parts)
  if (validAddress(L,address)){
    return(address)
  } else {
    return(NULL)
  }
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
    #message("Not a branching pointer, using addressFromList()")
    add1 <- addressFromList(L,pointer)
    if(length(add1)>0){
      returns <- list(Address=add1,Ids=eval(parse(text = add1)))
    } else {
      returns <- NULL
    }
    return(returns)
  }
  #initiate values
  done <- FALSE
  success=TRUE
  branchNow=NULL
  counter=0
  allAddresses = list()
  allIds = list()
  numbers <- which(!is.na(unlist(lapply(pointer,function(x) as.numeric(x)))))
  pointer <- incrementAll(pointer)
  if (is.null(branchNow)){
    branchNow <- max(numbers)
  }
  while (length(branchNow)>0){
    if ((Sys.time() - startTime) > 2){
      break
    }
    #First round only
    if (counter==0){
       if (is.null(branchNow)){
        branchNow <- max(numbers)
      }
    }

    #if the last run failed, the branching location should be moved up and the previous branch tip should be restarted at 1
    if(success==FALSE){
      #Reset the previous working branch tip to 1
      pointer[[branchNow]] <- 1
      #Move branchpoint up one
      branchNow <- numbers[(which(branchNow == numbers)-1)]

      #If we find the top-most branch, exit
      if (length(branchNow)<1){
        done <- TRUE
        break
      }
      pointer <- incrementPointer(pointer,branchNow)
      success=TRUE
    }
    #convert the pointer to an address
    address1 <- addressFromList(L,pointer)

    #If the address points to a valid lipd element...
    if (!is.null(address1)){
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
  if (done==TRUE && length(returns$Address)==0){
    returns<-NULL
  }
  return(returns)
}

#' Increment all branch points in pointer from 0 to 1
#'
#' @param pointer
#'
#' @return pointer
incrementAll <- function(pointer){
  #Find locations of zeros (branch points)
  zeros <- which(pointer == 0)
  #Increment all branch points until all are non zero
  while (length(zeros) > 0){
    branchPoint <- min(zeros)
    pointer <- incrementPointer(pointer,branchPoint)
    zeros <- which(unlist(pointer)==0)
  }
  return(pointer)
}

#' Extract the ID of an element based on its address
#'
#' @param pointer a pointer is a list of character and numeric objects which serve as an address within a lipd
#'
#' @return the ID
#'
parseID <- function(L,pointer){
  if (is.null(addressFromList(L,pointer))){
    return(NULL)
  }

  if(is.branching.pointer(pointer)){
    address1 <- addressFromListM(L,pointer)
    # print(address1)
    if(is.null(address1)){
      return(NULL)
    }
    expr1 <- address1$Address[[1]]
  } else {
    expr1 <- addressFromList(L,pointer)
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

#' Format/create explicit linkages for various chron/paleoData and measurement tables
#' This function addresses the following possible issues that might exist in the linkages:
#' 1. The lipd object may lack linkages altogether
#' 2. The linkage IDs may not be formatted correctly
#' 3. The link IDs may not be paired correctly
#'
#' @param L lipd object
#' @param action one of: c("report","repair", "new"), see checkLinks() for details
#' @param force overwrite existing IDs, only applies for action="new"
#'
#' @return lipd object
#'
lipdObjectLinkages <- function(L, action="report",force=FALSE){
  if(is.null(L) || !methods::is(L,"lipd")){
    stop("L must be a lipd object!")
  }
  if (!action %in% c("report","repair","new")){
    stop("'action' must be one of: 'report', 'repair', or 'new'. Use checkLinks() to create new links where more than one possible link exists")
  }
  #For each object requiring a linked object:
  ##check for ID, create one if needed
  ##find linked object
  ###check for matching ID, fill in if needed

  #Check for chronData
  hasChronData <- !is.null(addressFromListM(L,list("chronData",0)))
  #Check for chronData measurementTable(s)
  hasChronDataMT <- !is.null(addressFromListM(L,list("chronData",0,"measurementTable")))
  #Check for paleoData
  hasPaleoData <- !is.null(addressFromListM(L,list("paleoData",0)))
  #Check for paleoData measurementTable(s)
  hasPaleoDataMT <- !is.null(addressFromListM(L,list("paleoData",0,"measurementTable")))
  #Check for chronData model(s)
  hasChronDataModel <- !is.null(addressFromListM(L,list("chronData",0,"model")))

  #Paleo 1,2,3,etc.
  if (hasPaleoData && hasChronData){
    #iterate over paleoData objects
    for (jj in 1:length(L$paleoData)){
      ##paleoDataId may have 1 or more "linkedPaleoData" at L$chronData[[1-n]]
      addressNow <- list("paleoData",jj,"paleoDataId")
      addressLink <- list("chronData",0,"linkedPaleoData")
      L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
      ##linkedChronData
      addressNow <- list("paleoData",jj,"linkedChronData")
      addressLink <- list("chronData",0,"chronDataId")
      L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
      if (hasPaleoDataMT){
        #iterate over measurement tables
        for (kk in 1:length(L$paleoData[[jj]]$measurementTable)){
          if (hasChronDataMT){
            ###measurementTableId may have 1 or more "linkedMeasurementTable" at L$chronData[[1-n]]$measurementTable[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"measurementTableId")
            addressLink <- list("chronData",0,"measurementTable",0,"linkedMeasurementTable")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
            ###linkedMeasurementTable may have 1 or more "measurementTableId" at L$chronData[[1-n]]$measurementTable[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"linkedMeasurementTable")
            addressLink <- list("chronData",0,"measurementTable",0,"measurementTableId")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)

          } else {
            message("Missing chronData measurementTable")
          }

          if (hasChronDataModel){
            ###linkedModel may have 1 or more "modelId" at L$chronData[[1-n]]$model[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"linkedModel")
            addressLink <- list("chronData",0,"model",0,"modelId")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
            ###linkedSummaryTable may have 1 or more "summaryTableId" at L$chronData[[1-n]]$model[[1-n]]$summaryTable[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"linkedSummaryTable")
            addressLink <- list("chronData",0,"model",0,"summaryTable",0,"summaryTableId")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
            ###linkedDistributionTable may have 1 or more "distributionTableId" at L$chronData[[1-n]]$model[[1-n]]$distributionTable[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"linkedDistributionTable")
            addressLink <- list("chronData",0,"model",0,"distributionTable",0,"distributionTableId")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)
            ###linkedEnsembleTable may have 1 or more "ensembleTableId" at L$chronData[[1-n]]$model[[1-n]]$ensembleTable[[1-n]]
            addressNow <- list("paleoData",jj,"measurementTable",kk,"linkedEnsembleTable")
            addressLink <- list("chronData",0,"model",0,"ensembleTable",0,"ensembleTableId")
            L <- checkLinks(L,addressNow,addressLink,action=action,force=force)

          } else {
            message("Missing chronData model")
          }

        }
      } else {
        message("Missing paleoData measurementTable")
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
  return(L)
}

#' verify that a given ID is valid, Neotoma wants an integer
#'
#' @param ID
#'
#' @return 0 or 1
validNetotomaID <- function(ID){
  if (is.null(ID)){
    warning("ID is null")
    return(FALSE)
  } else if (length(ID) == 0){
    warning("ID is of length 0")
    return(FALSE)
  } else if (!methods::is(ID, "integer")){
    message(glue::glue("class of {ID} is {class(ID)}, must be class integer"))
    return(FALSE)
  }
  return(TRUE)
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

#' Given a location requiring a linkage in pointer1, check for its counterpart in pointer2 eg. paleoDataId/linkedPaleoData
#' Pointers may be branching (containing 0's) or specific (pointing to 1 particular location)
#'
#' @param L a lipd object
#' @param pointer1 a pointer is a list of character and numeric objects which serve as an address within a lipd
#' @param pointer2 location within the lipd corresponding to pointer1
#' @param action one of: c("report","new","repair")
#' @param ID new ID to set for pointer1 and and pointer2, only applies for action="new"
#' @param force overwrite existing IDs, only applies for action="new"
#'
#' @details
#' action options:
#' report: print to console
#' new: generate new IDs (random number or designated by ID param). Pointers must be singular (not branching)
#' repair: asssumes the pairing of IDs is correct, but the IDs are in the wrong format, and assigns new, random integers
#'
#' @return TRUE/FALSE (must all have links to return TRUE)
checkLinks <- function(L, pointer1, pointer2, action="report", ID=NULL, force=FALSE){
  if(is.null(L) || !methods::is(L,"lipd")){
    stop("L must be a lipd object!")
  }
  if(!is.pointer(pointer1) || !is.pointer(pointer2)){
    stop('Pointer(s) not valid. Must be a list of character and numeric objects. eg. list("paleoData",0,"linkedChronData")')
  }
  if (!action %in% c("report","new","repair")){
    stop("'action' must be one of: 'report', 'new', 'repair")
  }
  # convert pointers to addresses (these may be single or multiple)
  addresses1 <- addressFromListM(L,pointer1)
  addresses2 <- addressFromListM(L,pointer2)
  ## if action is "report" OR "repair"
  if (action == "report" || action == "repair"){
    ### if the addresses exist
    if (length(addresses1$Address)>0 && length(addresses2$Address)>0){
      address1 <- unlist(addresses1$Address)
      address2 <- unlist(addresses2$Address)
      ### gather IDs from addresses
      ID1 <- unlist(addresses1$Ids)
      ID2 <- unlist(addresses2$Ids)
      #### if IDs exist
      if (length(ID1)>0 && length(ID1)>0){
        #####match the Ids
        for (ii in 1:length(ID1)){
          iiLinkIndex <- which(ID1[ii] == ID2)
          if (length(iiLinkIndex) < 1){
            message(paste0("No corresponding links for '", ID1[ii], "' from ", address1[ii], " located in ", paste0(unlist(address2),collapse=", ")))
            return(L)
          } else {
            print(glue::glue("Found link between {glue::glue_collapse(address1,sep='_')} and {glue::glue_collapse(address2,sep='_')}"))
            linkedAddresses <- address2[iiLinkIndex]
            isValid <- validNetotomaID(ID1[ii])
            ### for invalid IDs, replace them, if action is repair
            if (!isValid && action=="repair"){
              newID <- as.integer(runif(1,min=10000,max=99999))
              eval(parse(text=sprintf('%s <- as.integer(%s)', address1[ii], as.integer(newID))))
              for (zz in linkedAddresses){
                eval(parse(text=sprintf('%s <- as.integer(%s)', zz, as.integer(newID))))
              }
              print(glue::glue("Repaired {unlist(address1[ii])} and linked counterpart(s) with new ID: {newID}"))
            }
          }
        }
        #### else report IDs do not exist
      } else {
        message(glue::glue("No ID at {glue::glue_collapse(address1,sep='_')} and/or {glue::glue_collapse(address2,sep='_')}"))
      }
      ### else report bad address(es)
    } else {
      message(glue::glue("No valid address at {glue::glue_collapse(pointer1,sep='_')} and/or {glue::glue_collapse(pointer2,sep='_')}"))
    }
  }
  ## if action is "new"
  if (action == "new"){
    if (length(addressFromListM(L,pointer1)$Ids)>0 || length(addressFromListM(L,pointer2)$Ids)>0){
      if (force == FALSE){
        message(glue::glue("Found existing IDs at {glue::glue_collapse(pointer1,sep='_')} and/or {glue::glue_collapse(pointer2,sep='_')}. Use force=TRUE to overwrite."))
        return(L)
      }
    }
    pointerHead1 <- pointer1[-length(pointer1)]
    addressHead1 <- addressFromListM(L,pointerHead1)
    pointerHead2 <- pointer2[-length(pointer2)]
    addressHead2 <- addressFromListM(L,pointerHead2)
    ### if each pointer contains only 1 valid head
    if (length(addressHead1$Address)==1 && length(addressHead2$Address)==1){
      #### create new addresses for valid address heads
      newAddress <- paste0("L",addressForm(incrementAll(pointer1)))
      newAddress2 <- paste0("L",addressForm(incrementAll(pointer2)))
      #### create new IDs for the new addresses, unless specified
      if (is.null(ID)){
        newID <- as.integer(runif(1,min=10000,max=99999))
      } else {
        newID <- ID
      }
      #### assign the IDs to the new addresses
      eval(parse(text=sprintf('%s <- as.integer(%s)', newAddress, newID)))
      eval(parse(text=sprintf('%s <- as.integer(%s)', newAddress2, newID)))
      #### report success
      print(glue::glue("Linked {unlist(newAddress)} and {newAddress2} with new ID: {newID}"))
      #### report failure, multiple options for suggested links
    } else {
      message(glue::glue("Could not set new ID for {glue::glue_collapse(pointer1,sep='_')} and {glue::glue_collapse(pointer2,sep='_')}. Unable to choose from multiple possible addresses. Set mannually using checkLinks() with specific pointers."))
    }
  }
  return(L)
}
