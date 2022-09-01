lipdSummary <- function(L){
  #Name
  cat(crayon::bold(glue::glue("{L$dataSetName} - {L$datasetId} - v.{L$changelog[[length(L$changelog)]]$version}\n\n")))
  cat("\n\n")
  
  #archiveType
  
  #lipdverseUrl
  cat(glue::glue("{L$lipdverseUrl}\n\n"))
  
  #geo
  
  cat(crayon::bold("Geographic Metadata\n\n"))
  
  gcmd <- L$geo$location
  
  if(!is.null(gcmd)){
  cat(glue::glue("{L$geo$siteName} ({gcmd})\n"))
  }else{
    cat(glue::glue("{L$geo$siteName} "))
  }
  
  if(!is.finite(L$geo$elevation)){
    cat(glue::glue("({L$geo$latitude}N, {L$geo$longitude}E)"))
  }else{
    cat(glue::glue("({L$geo$latitude}N, {L$geo$longitude}E) - {L$geo$elevation} masl"))
  }
  
  
  #pub
  #ideally this would print out citations for nPub citatations (where nPub is an input, with a default of 3? publications)
  
  
  #paleodata
  
  #numbers of paleoData objects, and number and types of tables within each object. 
  #a list of variable names and units (e.g. temperature (deg C))
  
  ts <- extractTs(L) %>% ts2tibble()
  
  #ts$paleoData_variableName
  #table(ts$paleoData_variableName)
  
  #number of observations rows in each table
  #age range in each table
  
  #chronData
  
  cts <- extractTs(L,mode = "chron") %>% ts2tibble()
  
  
  
  
  #examples from actR summary code: https://github.com/LinkedEarth/actR/blob/main/R/summary.R
  
  # cat(crayon::silver(glue::glue("Searched for {crayon::bold(exc.prefix)} excursions in a {crayon::bold(object$event.window)} year window around {crayon::bold(object$event.yr)} {object$timeUnits}, with reference windows of {crayon::bold(object$ref.window)} years on either side.\n\n")))
  # cat("\n")
  # cat(crayon::bold(glue::glue("Overall result: Empirical p-value = {resFun(object$empirical_pvalue)}\n\n")))
  # cat(glue::glue("Time uncertainty considered? {hasTimeEnsemble}\n\n"))
  # cat(glue::glue("Paleo uncertainty considered? {hasPaleoEnsemble}\n\n")) 
  # cat(glue::glue("Error propagation ensemble members = {object$unc.prop.n}\n\n"))
  # cat(glue::glue("Null hypothesis testing ensemble members = {object$null.hypothesis.n}\n\n"))
  # cat("\n")
  # cat(crayon::bold("Parameter choices:\n"))
  # for(p in params.to.print){
  #   cat(
  #     crayon::silver(
  #       glue::glue(
  #         "{p} = {object[p]}\n\n"
  #       )
  #     )
  #   )
  # }
  
  
}

createCitation <- function(pub){
  
}



