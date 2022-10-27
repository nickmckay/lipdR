#' Is this a valid LiPD
#'
#' @param L a lipd object
#' @param allow.ensemble Check for ensemble variables as well
#'
#' @return T/F
#' @export
validLipd <- function(L,allow.ensemble = TRUE){
  good <- TRUE
  
  #is list
  if(!is.list(L)){
    print(glue::glue("{L$dataSetName}: LiPD object must be a list"))
    return(FALSE)
  }
  
  
  
  #check root
  rootRequired <- c("archiveType",
                    "dataSetName",
                    "datasetId",
                    "changelog",
                    "geo",
                    "lipdVersion",
                    "createdBy")   
  
  rootRecommended <- c("paleoData",
                       "chronData",
                       "pub")
  
  if(!("paleoData" %in% names(L) | "chronData" %in% names(L))){
    print(glue::glue("{L$dataSetName}: LiPD object must contain paleoData and/or chronData"))
    good <- FALSE
  }
  
  for(rr in rootRequired){
    if(!(rr %in% names(L))){
      print(glue::glue("{L$dataSetName}: LiPD object must contain {rr}"))
      good <- FALSE
    }
  }
  for(rr in rootRecommended){
    if(!(rr %in% names(L))){
      warning(glue::glue("LiPD object should ideally contain {rr}"))
    }
  }
  
  
  
  
  # Check pub ---------------------------------------------------------------
  
  if(!validPub(L)){
    print(glue::glue("{L$dataSetName}: invalid pub section"))
    good <- FALSE
  }
  
  # check funding -----------------------------------------------------------
  # not used enough to worry about yet
  
  # Check Geo ---------------------------------------------------------------
  
  if(!validGeo(L)){
    print(glue::glue("{L$dataSetName}: invalid geo section"))
    good <- FALSE
  }
  
  # Check paleo measurement contents ---------------------------------------------------
  if(!validPaleo(L,allow.ensemble = allow.ensemble)){
    print(glue::glue("{L$dataSetName}: invalid paleoData section"))
    good <- FALSE
  }
  
  # Check chron measurement contents ---------------------------------------------------
  if(!validChron(L,allow.ensemble = allow.ensemble)){
    print(glue::glue("{L$dataSetName}: invalid chronData section"))
    good <- FALSE
  }
  
  
  
  #if it passes everything
  if(good){
    return(TRUE)
  }else{
    print(L$dataSetName)
    return(FALSE)
  }
  
  
}


validPub <- function(L){
  if(!"pub" %in% names(L)){
    warning(glue::glue("{L$dataSetName}: No publication metadata present"))
    return(TRUE) #pubs not required
  }
  pub <- L$pub
  
  if(!is.list(pub)){
    print(glue::glue("{L$dataSetName}: Publication must be a list"))
    return(FALSE)
  }
  
  if(!is.null(names(pub))){
    print(glue::glue("{L$dataSetName}: Publication must be an unnamed list"))
    return(FALSE)
    
  }
  
  if(length(pub) < 1){
    print(glue::glue("{L$dataSetName}: Publication must have length >1"))
    return(FALSE)
  }
  
  for(i in 1:length(pub)){
    p <- pub[[i]]
    if(!all(purrr::map_lgl(p,is.character) | purrr::map_lgl(p,is.numeric) | names(p) == "author")){
      print(glue::glue("{L$dataSetName} pub{i}: All fields except author must be character or numeric fields"))
      
    }
    if(any(names(p) == "author")){
      if(!is.list(p$author)){
        print(glue::glue("{L$dataSetName} pub{i}: author field should be a list"))
        return(FALSE)
      }
    }
    
    forbidden <- c("authors")
    for(f in 1:length(forbidden)){
      if(any(names(p) == forbidden[f])){
        print(glue::glue("{L$dataSetName} pub{i}: {forbidden[f]} is a forbidden term"))
        return(FALSE)
      }
    }
    
  }
  
  return(TRUE)
  
  
}

validGeo <- function(L){
  if(!"geo" %in% names(L)){
    warning(glue::glue("{L$dataSetName}: No geo metadata present"))
    return(FALSE) 
  }
  geo <- L$geo
  
  if(!is.list(geo)){
    print(glue::glue("{L$dataSetName}: Geo metadata must be a list"))
    return(FALSE)
  }
  
  
  reqNames <- c("latitude","longitude")
  
  for(rn in reqNames){
    if(!rn %in% names(geo)){
      print(glue::glue("{rn} must be in geo object"))
      return(FALSE)
    }
  }
  
  
  mustNumeric <- c("latitude","longitude","elevation","sisalSiteId")
  
  for(mn in mustNumeric){
    if(mn %in% names(geo)){
      if(!is.numeric(geo[[mn]])){
        print(glue::glue("{mn} must be numeric"))
        return(FALSE)
      }
    }
  }
  
  
  otherNames <- setdiff(names(geo),mustNumeric)
  for(on in otherNames){
    if(!is.character(geo[[on]])){
      print(glue::glue("{on} must be character"))
      return(FALSE)
    }
  }
  
  #check lat and long
  if(geo$latitude < -90 | geo$latitude > 90){
    print("geo$latitude is outside plausible boundaries")
  }
  if(geo$longitude < -180 | geo$longitude > 360){
    print("geo$longitude is outside plausible boundaries")
  }
  
  
  
  return(TRUE)
  
}

validPaleo <- function(L,allow.ensemble = TRUE){
  if(!"paleoData" %in% names(L)){
    warning(glue::glue("{L$dataSetName}: No paleoData metadata present"))
    return(TRUE) #pubs not required
  }
  paleo <- L$paleoData
  
  if(!is.list(paleo)){
    print(glue::glue("{L$dataSetName}: Publication must be a list"))
    return(FALSE)
  }
  
  if(!is.null(names(paleo))){
    print(glue::glue("{L$dataSetName}: paleoData must be an unnamed list"))
    return(FALSE)
    
  }
  
  if(length(paleo) < 1){
    print(glue::glue("{L$dataSetName}: paleoData must have length >1"))
    return(FALSE)
  }
  
  
  tn <- purrr::map(paleo,names) %>% unlist()
  if(any(!tn %in% c("measurementTable","ensembleTable","distributionTable","model"))){
    print(glue::glue("{L$dataSetName}:Invalid table names in paleoData"))
    return(FALSE)
  }
  
  
  for(p in 1:length(paleo)){
    P <- paleo[[p]]
    #check measurementTable structure
    if(any(names(P) == "measurementTable")){
      for(m in 1:length(P$measurementTable)){
        MT <- P$measurementTable[[m]]
        #get the lists
        il <- which(purrr::map_lgl(MT,is.list))
        LS <- MT[il]
        if(length(LS) < 2){
          print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} has fewer than two variables"))
          return(FALSE)
        }
        
        #check for valid variable
        if(!all(purrr::map_lgl(LS,validVariable,allow.ensemble = allow.ensemble))){
          print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} has invalid variables"))
          return(FALSE)
        }
        
        #check that all values are the same length
        lengths <- purrr::map_dbl(LS, variableLength)
        
        if(any(lengths < 1)){
          print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} variable values are missing"))
          return(FALSE)
        }
        
        if(!all(lengths == lengths[1])){
          print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} variable values have different lengths"))
          return(FALSE)
        }
        
        #check that age, depth and year are numeric
        isNum <- purrr::map_lgl(LS, ~ all(is.numeric(.x$values)))
        
        depthCol <- which(names(isNum) == "depth")
        if(length(depthCol) >= 1){
          if(!all(isNum[depthCol])){
            print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} depth values are not numeric"))
            return(FALSE)
          }
        }
        
        
        ageCol <- which(names(isNum) == "age")
        if(length(ageCol) >= 1){
          if(!all(isNum[ageCol])){
            print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} age values are not numeric"))
            return(FALSE)
          }
        }
        
        
        yearCol <- which(names(isNum) == "year")
        if(length(yearCol) >= 1){
          if(!all(isNum[yearCol])){
            print(glue::glue("{L$dataSetName}: paleoData {p} measurementTable {m} year values are not numeric"))
            return(FALSE)
          }
        }
        
      }
    }
  }
  
  return(TRUE)
  
}

validChron <- function(L,allow.ensemble = TRUE){
  if(!"chronData" %in% names(L)){
    warning(glue::glue("{L$dataSetName}: No chronData metadata present"))
    return(TRUE) #pubs not required
  }
  chron <- L$chronData
  
  if(!is.list(chron)){
    print(glue::glue("{L$dataSetName}: Publication must be a list"))
    return(FALSE)
  }
  
  if(!is.null(names(chron))){
    print(glue::glue("{L$dataSetName}: chronData must be an unnamed list"))
    return(FALSE)
    
  }
  
  if(length(chron) < 1){
    print(glue::glue("{L$dataSetName}: chronData must have length >1"))
    return(FALSE)
  }
  
  
  tn <- purrr::map(chron,names) %>% unlist()
  if(any(is.null(tn))){
    print(glue::glue("{L$dataSetName}:Invalid table names in chronData"))
    return(FALSE)  
  }
  
  if(any(! tn %in% c("measurementTable","ensembleTable","distributionTable","model"))){
    print(glue::glue("{L$dataSetName}:Invalid table names in chronData"))
    return(FALSE)
  }
  
  
  for(p in 1:length(chron)){
    P <- chron[[p]]
    #check measurementTable structure
    if(any(names(P) == "measurementTable")){
      for(m in 1:length(P$measurementTable)){
        MT <- P$measurementTable[[m]]
        #get the lists
        il <- which(purrr::map_lgl(MT,is.list))
        LS <- MT[il]
        if(length(LS) < 2){
          print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} has fewer than two variables"))
          return(FALSE)
        }
        
        #check for valid variable
        if(!all(purrr::map_lgl(LS,validVariable,allow.ensemble = allow.ensemble))){
          print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} has invalid variables"))
          return(FALSE)
        }
        
        #check that all values are the same length
        lengths <- purrr::map_dbl(LS, ~ length(.x$values))
        
        if(any(lengths < 1)){
          print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} variable values are missing"))
          return(FALSE)
        }
        
        if(!all(lengths == lengths[1])){
          print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} variable values have different lengths"))
          return(FALSE)
        }
        
        #check that age, depth and year are numeric
        isNum <- purrr::map_lgl(LS, ~ all(is.numeric(.x$values)))
        
        depthCol <- which(names(isNum) == "depth")
        if(length(depthCol) >= 1){
          if(!all(isNum[depthCol])){
            print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} depth values are not numeric"))
            return(FALSE)
          }
        }
        
        
        ageCol <- which(names(isNum) == "age")
        if(length(ageCol) >= 1){
          if(!all(isNum[ageCol])){
            print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} age values are not numeric"))
            return(FALSE)
          }
        }
        
        
        yearCol <- which(names(isNum) == "year")
        if(length(yearCol) >= 1){
          if(!all(isNum[yearCol])){
            print(glue::glue("{L$dataSetName}: chronData {p} measurementTable {m} year values are not numeric"))
            return(FALSE)
          }
        }
        
      }
    }
  }
  return(TRUE)
  
}






validVariable <- function(V,allow.ensemble = TRUE){
  reqNames <- c("TSid","variableName","values","number")
  for(r in reqNames){
    if(! r %in% names(V)){
      print(glue::glue("Required variable {r} is missing"))
      print(glue::glue("TSid: {V$TSid}"))
      print(glue::glue("variableName: {V$variableName}"))
      return(FALSE)
    }
  }
  
  #check class
  reqChar <- c("TSid","variableName")
  for(on in reqChar){
    if(!is.character(V[[on]])){
      print(glue::glue("{on} must be character ({V$TSid} - {V$variableName})"))
      return(FALSE)
    }
  }
  
  #check vector
  if(!is.vector(V$values)){
    #check for 1 column matrix
    if(is.matrix(V$values)){
      if(ncol(V$values) == 1){
        return(TRUE)
      }else if(ncol(V$values) > 1){
        if(allow.ensemble){
          return(TRUE)
        }else{
          print(glue::glue("variable values must be a one column vector (allow.ensemble = FALSE) and ({V$TSid} - {V$variableName}) is a multicolumn"))
          return(FALSE)
        }
      }
    }
    print(glue::glue("variable values must be a vector ({V$TSid} - {V$variableName})"))
    return(FALSE)
  }
  
  return(TRUE)
}


validMultiLipd <- function(D){
  all(purrr::map_lgl(D,validLipd))
}

variableLength <- function(VL){
  if(is.matrix(VL$values)){
    return(NROW(VL$values))
  }else{
    return(length(VL$values))
  }
  
}
