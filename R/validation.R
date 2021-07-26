validLipd <- function(L){
  
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
    return(FALSE)
  }
  
  for(rr in rootRequired){
    if(!(rr %in% names(L))){
      print(glue::glue("{L$dataSetName}: LiPD object must contain {rr}"))
      return(FALSE)
    }
  }
  for(rr in rootRecommended){
    if(!(rr %in% names(L))){
      warning(glue::glue("LiPD object should ideally contain {rr}"))
    }
  }
  


# big picture - paleo/chron data ---------------------------------------------------------

  if("paleoData" %in% names(L)){
    d <- L$paleoData
    
    #check table names
    tn <- purrr::map_chr(d,names)
    if(any(!tn %in% c("measurementTable","ensembleTable","distributionTable"))){
      print("Invalid table names in paleoData")
      return(FALSE)
    }
    
    
  }
  

# Check pub ---------------------------------------------------------------

  if(!validPub(L)){
    print(glue::glue("{L$dataSetName}: invalid pub section"))
    return(FALSE)
  }

# check funding -----------------------------------------------------------
# not used enough to worry about yet

# Check Geo ---------------------------------------------------------------

  if(!validGeo(L)){
    print(glue::glue("{L$dataSetName}: invalid geo section"))
    return(FALSE)
  }
  
# Check paleo measurement contents ---------------------------------------------------


  # Check chron measurement contents ---------------------------------------------------
  
  
  
  
  #if it passes everything
  return(TRUE)
  
  
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