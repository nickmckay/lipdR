new_lipd <- function(x = list()){
  structure(x,class = c("lipd",class(list())))
}

new_lipdts <- function(x = list()){
  structure(x,class = c("lipd-ts",class(list())))
}

new_lipdTsTibble <- function(x = list()){
  structure(x,class = c("lipd-ts-tibble",class(tibble::tibble())))
}

new_lipdTsTibbleLong <- function(x = list()){
  structure(x,class = c("lipd-ts-tibble-long",class(tibble::tibble())))
}

is.lipd <- function(x){
  if(any(class(x) == "lipd")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

is.lipdTs <- function(x){
  if(any(class(x) == "lipd-ts")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

is.lipdTsTibble <- function(x){
  if(any(class(x) == "lipd-ts-tibble")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

is.lipdTsTibbleLong <- function(x){
  if(any(class(x) == "lipd-ts-tibble-long")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


as.lipd <- function(x){
  if(is.lipd(x)){
    structure(x,class = c("lipd",class(list()))) %>% 
    return()
  }else if(is.list(x)){
    if(validLipd(x)){
      return(new_lipd(x))
    }else{
      stop("Tried to convert a list to a LiPD object, but it failed validLipd(). Check ?validLipd for details")
    }
  }else if(is.lipdTs(x)){
    collapseTs(x,force = "if necessary") %>% 
    structure(class = c("lipd",class(list()))) %>% 
    return()
  }else if(is.lipdTsTibble(x)){
    purrr::transpose(x) %>% 
      collapseTs(force = "if necessary") %>% 
      structure(class = c("lipd",class(list()))) %>% 
      return()
  }else if(is.lipdTsTibbleLong(x)){
    untidyTs(x) %>% 
      structure(class = c("lipd",class(list()))) %>% 
      return()
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD."))
  }
}

as.lipdTs <- function(x){
  if(is.lipd(x)){
    extractTs(x) %>% 
    structure(class = c("lipd-ts",class(list()))) %>% 
    return()
  }else if(is.lipdTs(x)){
    return(x)
  }else if(is.lipdTsTibble(x)){
    purrr::transpose(x) %>% 
      structure(class = c("lipd-ts",class(list()))) %>% 
      return()
  }else if(is.lipdTsTibbleLong(x)){
    untidyTs(x) %>% 
      structure(class = c("lipd-ts",class(list()))) %>% 
      return()
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD-TS object."))
  }
}

as.lipdTsTibble <- function(x){
  if(is.lipd(x)){
    extractTs(x) %>% 
      ts2tibble() %>% 
      structure(class = c("lipd-ts-tibble",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTs(x)){
    ts2tibble(x) %>% 
    structure(class = c("lipd-ts-tibble",class(tibble::tibble()))) %>% 
    return()
  }else if(is.lipdTsTibble(x)){
      return(x)
  }else if(is.lipdTsTibbleLong(x)){
    untidyTs(x) %>% 
      ts2tibble() %>% 
      structure(class = c("lipd-ts-tibble",class(tibble::tibble()))) %>% 
      return()
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD-TS-tibble object."))
  }
}

as.lipdTsTibbleLong <- function(x){
  if(is.lipd(x)){
    extractTs(x) %>% 
      tidyTs() %>% 
      structure(class = c("lipd-ts-tibble-long",class(tibble::tibble()))) %>% 
      return()
  }else if(is.lipdTs(x)){
    tidyTs(x) %>% 
    structure(class = c("lipd-ts-tibble-long",class(tibble::tibble()))) %>% 
    return()
  }else if(is.lipdTsTibble(x)){
    purrr::transpose(x) %>% 
      tidyTs() %>% 
      structure(class = c("lipd-ts-tibble-long",class(tibble::tibble()))) %>% 
      return() 
  }else if(is.lipdTsTibbleLong(x)){
      return(x)
  }else{
    stop(glue::glue("I don't know how to convert class: {class(x)} to LiPD-TS-tibble object."))
  }
}