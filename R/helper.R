quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 


#' Create a random TSid
#'
#' @param prefix Optionally pre-pend a string
#'
#' @return TSid
#' @export
createTSid <- function(prefix = ""){
  
  return(paste0(prefix,paste(c("R",sample(c(letters,LETTERS,seq(0,9)),size = 16,replace=TRUE)),collapse = "")))
}

#' Add TSid where missing in a LiPD file
#' @param L LiPD file
#' @param prefix a prefix to prepend to the randomized TSid
#' @return a Lipd file
#' @export
addTSidToLipd <- function(L,prefix = "add"){
  mts <- lipdR::extractTs(L)
  addPaleo <- FALSE
  addChron <- FALSE
  
  for(i in 1:length(mts)){
    if(length(mts[[i]]$paleoData_TSid)==0){
      mts[[i]]$paleoData_TSid <- createTSid(prefix = prefix)
      addPaleo <- TRUE
      print(glue::glue("Added TSid {mts[[i]]$paleoData_TSid} to chronData variable {mts[[i]]$paleoData_variableName}"))
      
      
    }
  }
  if(addPaleo){
    L <- collapseTs(mts)
  }
  #try do it with chronData too
  if(!is.null(L$chronData)){
    cts <- try(lipdR::extractTs(L,mode = "chron"),silent = TRUE)
    for(i in 1:length(cts)){
      if(length(cts[[i]]$chronData_TSid)==0){
        cts[[i]]$chronData_TSid <- createTSid(prefix = prefix)
        addChron <- TRUE
        print(glue::glue("Added TSid {cts[[i]]$chronData_TSid} to chronData variable {cts[[i]]$chronData_variableName}"))
      }
      
    }
    
    if(addChron){
      L <- collapseTs(cts)
    }
    
  }
  
  return(L)
}
