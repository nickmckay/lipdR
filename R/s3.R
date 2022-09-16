new_lipd <- function(x = list()){
  structure(x,class = c("lipd",class(list())))
}

new_multiLipd <- function(x = list()){
  structure(x,class = c("multi_lipd",class(list())))
}

new_lipdTs <- function(x = list()){
  structure(x,class = c("lipd_ts",class(list())))
}

new_lipdTsTibble <- function(x = list()){
  structure(x,class = c("lipd_ts_tibble",class(tibble::tibble())))
}

new_lipdTsTibbleLong <- function(x = list()){
  structure(x,class = c("lipd_ts_tibble_long",class(tibble::tibble())))
}

#' Is this a LiPD object?
#'
#' @param x any object
#'
#' @return boolean
#' @export
is.lipd <- function(x){
  if(any(class(x) == "lipd")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Is this a multiLiPD object?
#'
#' @param x any object
#'
#' @return boolean
#' @export
is.multiLipd <- function(x){
  if(any(class(x) == "multi_lipd")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Is this a LiPD TS object?
#'
#' @param x any object
#'
#' @return boolean
#' @export
is.lipdTs <- function(x){
  if(any(class(x) == "lipd_ts")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Is this a LiPD TS Tibble object?
#'
#' @param x any object
#'
#' @return boolean
#' @export
is.lipdTsTibble <- function(x){
  if(any(class(x) == "lipd_ts_tibble")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Is this a LiPD TS Tibble long object?
#'
#' @param x any object
#'
#' @return boolean
#' @export
is.lipdTsTibbleLong <- function(x){
  if(any(class(x) == "lipd_ts_tibble_long")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Convert a compatible object into a LiPD Structure
#'
#' @param x  a lipd-compatible object
#'
#' @return a LiPD object
#' @export
as.lipd <- function(x){
  if(is.lipd(x)){
    return(x)
  }else if(is.lipdTs(x)){
    x <- quiet(collapseTs(x,force = "if necessary"))
    
    if(any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else{
      stop("Tried to convert from a lipd_ts to a multi_lipd, but it looks like more than one dataset was represented, so cannot convert this to lipd class. Perhaps you want as.multiLipd?")
    }
  }else if(is.lipdTsTibble(x)){
    x <- purrr::transpose(x) %>% 
      collapseTs(force = "if necessary") %>% 
      suppressMessages()
    if(any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else{
      stop("Tried to convert from a lipd_ts_tibble to a multi_lipd, but it looks like more than one dataset was represented, so cannot convert this to lipd class. Perhaps you want as.multiLipd?")
    }
  }else if(is.lipdTsTibbleLong(x)){
    x <- untidyTs(x) 
    
    if(any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else{
      stop("Tried to convert from a lipd_ts_tibble_long to a multi_lipd, but it looks like more than one dataset was represented, so cannot convert this to lipd class. Perhaps you want as.multiLipd?")
    }
  }else if(is.list(x)){
    if(validLipd(x)){
      return(new_lipd(x))
    }else{
      stop("Tried to convert a list to a LiPD object, but it failed validLipd(). Check ?validLipd for details")
    }
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD."))
  }
}

#' Convert a compatible object into a multiLiPD Structure
#'
#' @param x  a multiLiPD-compatible object
#'
#' @return a multi LiPD object
#' @export
as.multiLipd <- function(x){
  if(is.multiLipd(x)){
    return(x)
  }else if(is.lipd(x)){
    stop("Cannot convert a single lipd to a multi_lipd. use multiLipd() to create a multi_lipd from a collection of lipd objects.")
    
  }else if(is.lipdTs(x)){
    x <- quiet(collapseTs(x,force = "if necessary"))
    
    if(!any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else{
      stop("Tried to convert from a lipd_ts to a multi_lipd, but it looks like only one dataset was represented, so cannot convert this to a multi_lipd")
    }
  }else if(is.lipdTsTibble(x)){
    x <- purrr::transpose(x) %>% 
      collapseTs(force = "if necessary") %>% 
      quiet()
    if(!any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else{
      stop("Tried to convert from a lipd_ts_tibble to a multi_lipd, but it looks like only one dataset was represented, so cannot convert this to a multi_lipd")
    }
  }else if(is.lipdTsTibbleLong(x)){
    x <- untidyTs(x) 
    
    if(!any("dataSetName" %in% names(x))){
      structure(x,class = c("multi_lipd",class(list()))) %>% 
        return()
    }else if(is.list(x)){
      if(validMultiLipd(x)){
        return(new_multiLipd(x))
      }else{
        stop("Tried to convert a list to a LiPD object, but it failed validMultiLipd(). Check ?validMultiLipd for details")
      }
    }else{
      
      stop("Tried to convert from a lipd_ts_tibble_long to a multi_lipd, but it looks like only one dataset was represented, so cannot convert this to a multi_lipd")
    }
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD."))
  }
}

#' Convert a compatible object into a lipd TS Structure
#'
#' @param x  a lipd ts compatible object
#'
#' @return a LiPD TS object
#' @export
as.lipdTs <- function(x){
  if(is.lipd(x) | is.multiLipd(x)){
    extractTs(x) %>% 
      structure(class = c("lipd_ts",class(list()))) %>% 
      return()
  }else if(is.lipdTs(x)){
    return(x)
  }else if(is.lipdTsTibble(x)){
    purrr::transpose(x) %>% 
      structure(class = c("lipd_ts",class(list()))) %>% 
      return()
  }else if(is.lipdTsTibbleLong(x)){
    untidyTs(x) %>% 
      structure(class = c("lipd_ts",class(list()))) %>% 
      return()
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to lipd_ts object."))
  }
}

#' Convert a compatible object into a lipd TS Tibble object
#'
#' @param x  a lipd ts tibble-compatible object
#'
#' @return a  a lipd TS Tibble object
#' @export
as.lipdTsTibble <- function(x){
  if(is.lipd(x) | is.multiLipd(x)){
    extractTs(x) %>% 
      ts2tibble() %>% 
      structure(class = c("lipd_ts_tibble",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTs(x)){
    ts2tibble(x) %>% 
      structure(class = c("lipd_ts_tibble",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTsTibble(x)){
    return(x)
  }else if(is.lipdTsTibbleLong(x)){
    untidyTs(x) %>% 
      ts2tibble() %>% 
      structure(class = c("lipd_ts_tibble",class(tibble::tibble()))) %>% 
      return()
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to lipd_ts_tibble object."))
  }
}

#' Convert a compatible object into a lipd TS Tibble long object
#'
#' @param x  a lipd ts tibble long-compatible object
#'
#' @return a  a lipd TS Tibble long object
#' @export
as.lipdTsTibbleLong <- function(x){
  if(is.lipd(x) | is.multiLipd(x)){
    extractTs(x) %>% 
      tidyTs() %>% 
      structure(class = c("lipd_ts_tibble_long",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTs(x)){
    tidyTs(x) %>% 
      structure(class = c("lipd_ts_tibble_long",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTsTibble(x)){
    purrr::transpose(x) %>% 
      tidyTs() %>% 
      structure(class = c("lipd_ts_tibble_long",class(tibble::tibble()))) %>% 
      return() 
  }else if(is.lipdTsTibbleLong(x)){
    return(x)
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to lipd_ts_tibble object."))
  }
}


#' short print out of single lipd
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
print.lipd <- function(x){
  cat(x$dataSetName,"\n")
  cat(x$datasetId,"\n")
  cat(x$changelog[[length(x$changelog)]]$version,"\n")
  cat("\n")
  cat("use summary() to see details\n")
}

#' Full summary detail of single lipd
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
summary.lipd <- function (x){
  lipdSummary(x)
}


#' short print out of multi lipd
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
print.multi_lipd <- function (x){
  cat(paste0("multi lipd object containing ", length(x), " individual lipds\n"))
  cat("use summary() to see details")
}


#' full summary of multi lipd
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
summary.multi_lipd <- function (x, ...){
  multiLipdSummary(x, ...)
}


#' full summary of lipd ts
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
summary.lipd_ts <- function(x, ...) {
  lipdTSSummary(x, ...)
}


#' full simary of lipd ts tibble
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
summary.lipd_ts_tibble <- function(x, ...) {
  lipdTSSummary(x, ...)
}




