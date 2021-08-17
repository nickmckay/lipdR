addMissingTSid <- function(L){
  
  if(validLipd(L)){
    return(L)
  }
  #paleo first
  if(any(names(L) == "paleoData")){
    for(p in 1:length(L$paleoData)){
      M <- L$paleoData[[p]]$measurementTable
      for(m in 1:length(L$paleoData[[p]]$measurementTable)){
        v <- which(purrr::map_lgl(L$paleoData[[p]]$measurementTable[[m]],is.list))
        for(vi in v){
          if(!"TSid" %in% names(L$paleoData[[p]]$measurementTable[[m]][[vi]])){
            print(glue::glue("{L$dataSetName} missing paleoData{p}meas{m}-{L$paleoData[[p]]$measurementTable[[m]][[vi]]$variableName} TSid, adding one in"))
           L$paleoData[[p]]$measurementTable[[m]][[vi]]$TSid <- createTSid("p")
          }
        }
        
      }
    }
    
  }
  if(any(names(L) == "chronData")){
    for(p in 1:length(L$chronData)){
      M <- L$chronData[[p]]$measurementTable
      for(m in 1:length(L$chronData[[p]]$measurementTable)){
        v <- which(purrr::map_lgl(L$chronData[[p]]$measurementTable[[m]],is.list))
        for(vi in v){
          if(!"TSid" %in% names(L$chronData[[p]]$measurementTable[[m]][[vi]])){
            print(glue::glue("{L$dataSetName} missing chronData{p}meas{m}-{L$chronData[[p]]$measurementTable[[m]][[vi]]$variableName} TSid, adding one in"))
            L$chronData[[p]]$measurementTable[[m]][[vi]]$TSid <- createTSid("c")
          }
        }
        
      }
    }
    
  }
  return(L)
  
}