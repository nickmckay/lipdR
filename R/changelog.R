updateLipdverseLink <- function(L){
  vers <- getVersion(L)
  L$lipdverseLink <- paste0("https://lipdverse.org/data/",L$datasetId,"/",stringr::str_replace_all(as.character(vers),"[.]","_"))
  return(L)
}



#' Initialize a changelog for a LiPD object
#'
#' @description This function will create a changelog for a file that doesn't already have one.
#' @inheritParams updateChangelog
#' @return A LiPD object with a changelog
#' @export
initializeChangelog <- function(L,
                                notes = "Starting the changelog",
                                curator = Sys.info()[["user"]],
                                timestamp = lubridate::now(tzone = "UTC"),
                                version = "1.0.0"){
  if(!is.null(L$changelog)){#it already exists
    warning("It looks like this file already has a change log, you probably want `updateChangelog()`. returning the input LiPD object without modification.")
    return(L)
  }




  thisChange <- list(version = as.character(version),
                     curator = curator,
                     timestamp = paste(timestamp,tz(timestamp)),
                     notes  =  notes)

  L$changelog <- list(thisChange)
  return(L)
}


tibDiff <- function(tcol){
  #check for a change
  if(length(tcol) > 0){
    old <- tcol[1]
    new <- tcol[2]
    if(is.na(old) | tolower(old) == "na" | is.null(old)){
      old <- ""
    }
    if(is.na(new) | tolower(new) == "na" | is.null(new)){
      new <- ""
    }

    if(old != new){
      return(glue::glue("'{old}' has been replaced by '{new}'"))
    }
  }
}

#' Create a changelog by comparing two LiPD files
#'
#' @param Lold
#' @param Lnew
#' @param good.base
#' @param exclude.paleo
#' @param exclude.chron
#'
#' @return
#' @export
createChangelog <- function(Lold,
                            Lnew,
                            good.base = c("archiveType",
                                          "createdBy",
                                          "dataSetName",
                                          "dataSource",
                                          "notes",
                                          "originalDataUrl"),
                            exclude.paleo = c("paleoData_meanValue12k",
                                              "paleoData_mostRecentCompilations",
                                              "paleoData_medianRes12k",
                                              "paleoData_tableName",
                                              "paleoData_values"),
                            exclude.chron = c("chronData_values")){

  cl <- c() #initialize changelog
  ct <- c() #initialize change type
  cv <- c() #initialize change variable
  checked.base <- FALSE #initialize

  lastVers <- getVersion(Lold)

  # Check for paleo and chronData -------------------------------------------
  #paleoData
  npdo <- length(Lold$paleoData)
  npdn <- length(Lnew$paleoData)

  if(npdn>0){
    hasPaleo <- TRUE
  }else{
    hasPaleo <- FALSE
  }
  #if paleoData was added or removed, let's report that
  if(npdo == 0 & npdn > 0){
    cl <- c(cl,"PaleoData has been added to this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)
  }
  if(npdo > 0 & npdn == 0){
    cl <- c(cl,"All PaleoData have been removed from this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)
  }

  #chronData
  ncdo <- length(Lold$chronData)
  ncdn <- length(Lnew$chronData)

  if(ncdn>0){
    hasChron <- TRUE
  }else{
    hasChron <- FALSE
  }
  #if chronData was added or removed, let's report that
  if(ncdo == 0 & ncdn > 0){
    cl <- c(cl,"chronData has been added to this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)

  }
  if(ncdo > 0 & ncdn == 0){
    cl <- c(cl,"All chronData have been removed from this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)

  }


  # Go through paleoData ----------------------------------------------------
  if(hasPaleo){
    #get tibbles
    to <- Lold %>%
      extractTs() %>%
      ts2tibble() %>%
      dplyr::arrange(paleoData_TSid)

    tn <- Lnew %>%
      extractTs() %>%
      ts2tibble() %>%
      dplyr::arrange(paleoData_TSid)

    #check TSids are unique
    if(any(duplicated(to$paleoData_TSid))){
      stop(glue::glue("{to$dataSetName[1]}: the original dataset has duplicated TSids"))
    }
    if(any(duplicated(tn$paleoData_TSid))){
      stop(glue::glue("{tn$dataSetName[1]}: the new dataset has duplicated TSids"))
    }


    # Check for added/removed columns -----------------------------------------

    #check for added columns
    if(any(!tn$paleoData_TSid %in% to$paleoData_TSid)){#then one was added
      wa <- which(!tn$paleoData_TSid %in% to$paleoData_TSid)
      for(i in wa){
        cl <- c(cl,
                glue::glue("Column '{tn$paleoData_TSid[i]}', with variable name '{tn$paleoData_variableName[i]}', was added to the dataset")
        )
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,NA)
      }
      #then remove them - we won't describe the details of added columns
      tn <- tn[-wa,]
    }

    #check for removed columns
    if(any(!to$paleoData_TSid %in% tn$paleoData_TSid)){#then one was added
      wr <- which(!to$paleoData_TSid %in% tn$paleoData_TSid)
      for(i in wr){
        cl <- c(cl,
                glue::glue("Column '{to$paleoData_TSid[i]}', with variable name '{to$paleoData_variableName[i]}', was removed from the dataset")
        )
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,NA)

      }
      #then remove them - this should force the datasets to always have the same number of columns
      to <- to[-wr,]
    }



    #make sure there are some rows remaining
    if(nrow(tn) == 1 | nrow(to) == 1){
      warning(glue::glue("{Lnew$dataSetName}: there is only 1 matching TSids in the paleoData. This is not typical."))
    }


    #make sure there are some rows remaining
    if(nrow(tn) < 1 | nrow(to) < 1){
      print(glue::glue("{Lnew$dataSetName}: there is no matching TSids in the paleoData. You probably entered an incorrect file or fixed some TSid issues"))
      print("trying by variableName")
      #get tibbles
      to <- Lold %>%
        extractTs(mode = "paleo") %>%
        ts2tibble() %>%
        dplyr::arrange(paleoData_variableName)

      tn <- Lnew %>%
        extractTs(mode = "paleo") %>%
        ts2tibble() %>%
        dplyr::arrange(paleoData_variableName)

      #check TSids are unique
      if(any(duplicated(to$paleoData_variableName))){
        stop("the original dataset has duplicated paleo variableName")
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,paste(to$to$paleoData_variableName[duplicated(to$paleoData_variableName)],collapse = ", "))

        changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)

        return(changelog)
      }
      if(any(duplicated(tn$paleoData_variableName))){
        stop("the original dataset has duplicated paleo variableName")
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,paste(tn$paleoData_variableName[duplicated(tn$paleoData_variableName)],collapse = ", "))

        changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)

        return(changelog)
      }


      # Check for added/removed columns -----------------------------------------

      #check for added columns
      if(any(!tn$paleoData_variableName %in% to$paleoData_variableName)){#then one was added
        wa <- which(!tn$paleoData_variableName %in% to$paleoData_variableName)
        for(i in wa){
          cl <- c(cl,
                  glue::glue("Column '{tn$paleoData_TSid[i]}', with variable name '{tn$paleoData_variableName[i]}', was added to the dataset")
          )
          ct <- c(ct,"paleoData table")
          cv <- c(cv,NA)

        }
        #then remove them - we won't describe the details of added columns
        tn <- tn[-wa,]
      }

      #check for removed columns
      if(any(!to$paleoData_variableName %in% tn$paleoData_variableName)){#then one was added
        wr <- which(!to$paleoData_variableName %in% tn$paleoData_variableName)
        for(i in wr){
          cl <- c(cl,
                  glue::glue("Column '{to$paleoData_TSid[i]}', with variable name '{to$paleoData_variableName[i]}', was removed from the dataset")
          )
          ct <- c(ct,"paleoData table")
          cv <- c(cv,NA)

        }
        #then remove them - this should force the datasets to always have the same number of columns
        to <- to[-wr,]
      }
      if(nrow(tn) < 1 | nrow(to) < 1){
        stop("there are 0 or 1 matching TSids AND variableNames in the paleoData. You probably entered an incorrect file, or fixed a TSid problem")
      }
    }

    #check to make sure that the TSids and number of rows are identical
    tn <- tn %>% dplyr::arrange(paleoData_TSid)
    to <- to %>% dplyr::arrange(paleoData_TSid)


    # Check base metadata -----------------------------------------------------

    #make sure the names are present
    ne <- good.base[!good.base %in% names(to)]

    if(length(ne) > 0){
      for(n in ne){
        to <- dplyr::mutate(to,!!n := NA)
      }
    }

    ne <- good.base[!good.base %in% names(tn)]

    if(length(ne) > 0){
      for(n in ne){
        tn <- dplyr::mutate(tn,!!n := NA)
      }
    }

    #filter and collapse to just non-paleo metadata




    bto <- dplyr::select(to,!!good.base,
                         starts_with("pub"),
                         starts_with("geo_"),
                         starts_with("funding")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


    #should only be one row now
    if(nrow(bto) != 1){
      stop("The old dataset has discrepencies in metadata between columns that shouldn't exist")
    }


    btn <- dplyr::select(tn,!!good.base,
                         starts_with("pub"),
                         starts_with("geo_"),
                         starts_with("funding")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


    if(nrow(btn) != 1){
      stop("The new dataset has discrepencies in metadata between columns that shouldn't exist")
    }

    tcdf <- dplyr::bind_rows(bto,btn)
    tcdf[is.null(tcdf)] <- NA

    #check for changes and report back
    cldf <- purrr::map_dfc(tcdf,tibDiff)
    if(nrow(cldf)>0){
      #fold into changelog
      cl <- c(cl,paste(names(cldf),cldf,sep = ": "))
      #what type of base change?
      for(cli in 1:ncol(cldf)){
        if(grepl("pub[0-9]_",names(cldf)[cli])){
          ct <- c(ct,"Publication metadata")
        }else if(grepl("funding[0-9]_",names(cldf)[cli])){
          ct <- c(ct,"Funding metadata")
        }else if(grepl("geo_",names(cldf)[cli])){
          ct <- c(ct,"Geographic metadata")
        }else{
          ct <- c(ct,"Base metadata")
        }
      }
      cv <- c(cv,names(cldf))
    }

    checked.base <- TRUE
    # Check paleoData column metadata -----------------------------------------

    paleoSelect <- function(x,exclude.paleo){

      exclude.paleo <- exclude.paleo[exclude.paleo %in% names(x)]

      o <- dplyr::select(x,starts_with("paleoData_"),
                         starts_with("interpretation"),
                         starts_with("calibration")) %>%
        dplyr::select(-starts_with("paleoData_has"),
                      -!!exclude.paleo)
      return(o)}


    #make all columns character for this comparison
    ptn <- paleoSelect(tn,exclude.paleo) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))
    pto <- paleoSelect(to,exclude.paleo) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))

    #loop through TSids
    for(i in 1:nrow(ptn)){
      tsi <- ptn$paleoData_TSid[i]
      tsname <- ptn$paleoData_variableName[i]

      #prep the comparison
      tcdf <- dplyr::bind_rows(pto[i,],ptn[i,])
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      #fold into changelog

      if(nrow(cldf) > 0){
        cl <- c(cl,
                paste(glue::glue("{tsname} ({tsi})"),names(cldf),cldf,sep = ": "))
        for(cli in 1:ncol(cldf)){
          if(grepl(pattern = "interpretation[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Paleo Interpretation metadata")
          }else if(startsWith(prefix = "calibration",names(cldf)[cli])){
            ct <- c(ct,"Paleo Calibration metadata")
          }else{
            ct <- c(ct,"Paleo Column metadata")
          }
        }
        cv <- c(cv,names(cldf))

      }
    }

    # compare the paleoData_values --------------------------------------------
    for(i in 1:nrow(tn)){
      tov <- to$paleoData_values[[i]]
      tov[is.na(tov)] <- -999
      tnv <- tn$paleoData_values[[i]]
      tnv[is.na(tnv)] <- -999

      if(is.character(tov) & is.character(tnv)){
        valChange <- !all(tov == tnv)
      }else if(is.numeric(tov) & is.numeric(tnv)){
        valChange <- !all(dplyr::near(tov,tnv))
      }else{
        warning("it seems like the old and new values are of different classes, which is bad. Converting to character for comparison")
        valChange <- !all(as.character(tov) == as.character(tnv))
      }

      if(valChange){
        tsi <- tn$paleoData_TSid[i]
        tsname <- tn$paleoData_variableName[i]
        cl <- c(cl,
                glue::glue("{tsname} ({tsi}): The size of the paleoData_values have changed, from {length(tov)} to {length(tnv)} entries."))
        ct <- c(ct,"PaleoData values")
        cv <- c(cv,"paleoData_values")
      }else{
        if(!all(tov == tnv)){
          tsi <- tn$paleoData_TSid[i]
          tsname <- tn$paleoData_variableName[i]
          cl <- c(cl,
                  glue::glue("{tsname} ({tsi}): The paleoData_values have changed"))
          ct <- c(ct,"PaleoData values")
          cv <- c(cv,"paleoData_values")
        }
      }

    }

  }


  # Go through chronData ----------------------------------------------------
  totest <- Lold %>%
    extractTs(mode = "chron")

  if(hasChron & length(totest) > 0){# only check for new columns if there was an old chron.
    #get tibbles
    to <- Lold %>%
      extractTs(mode = "chron") %>%
      ts2tibble() %>%
      dplyr::arrange(chronData_TSid)

    tn <- Lnew %>%
      extractTs(mode = "chron") %>%
      ts2tibble() %>%
      dplyr::arrange(chronData_TSid)

    #check TSids are unique
    if(any(duplicated(to$chronData_TSid))){
      print("the original dataset has duplicated chron TSids, can't proceed")
      cl <- c(cl,
              glue::glue("ChronData: Duplicated TSids names in original dataset, couldn't log column-level changes.")
      )

      ct <- c(ct,"ChronData table")
      cv <- c(cv,paste(to$chronData_TSid[duplicated(to$chronData_TSid)],collapse = ", "))

      changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)

      return(changelog)
    }
    if(any(duplicated(tn$chronData_TSid))){
      stop("the new dataset has duplicated chron TSids")
    }


    # Check for added/removed columns -----------------------------------------

    #check for added columns
    if(any(!tn$chronData_TSid %in% to$chronData_TSid)){#then one was added
      wa <- which(!tn$chronData_TSid %in% to$chronData_TSid)
      for(i in wa){
        cl <- c(cl,
                glue::glue("Column '{tn$chronData_TSid[i]}', with variable name '{tn$chronData_variableName[i]}', was added to the dataset")
        )
        ct <- c(ct,"ChronData table")
        cv <- c(cv,NA)

      }
      #then remove them - we won't describe the details of added columns
      tn <- tn[-wa,]
    }

    #check for removed columns
    if(any(!to$chronData_TSid %in% tn$chronData_TSid)){#then one was added
      wr <- which(!to$chronData_TSid %in% tn$chronData_TSid)
      for(i in wr){
        cl <- c(cl,
                glue::glue("Column '{to$chronData_TSid[i]}', with variable name '{to$chronData_variableName[i]}', was removed from the dataset")
        )
        ct <- c(ct,"ChronData table")
        cv <- c(cv,NA)

      }
      #then remove them - this should force the datasets to always have the same number of columns
      to <- to[-wr,]
    }

    #make sure there are some rows remaining
    if(nrow(tn) < 1 | nrow(to) < 1){
      print("there are 0 or 1 matching TSids in the chronData. You probably entered an incorrect file, or fixed a TSid problem")
      print("trying by variableName")


      to <- Lold %>%
        extractTs(mode = "chron") %>%
        ts2tibble()

      tn <- Lnew %>%
        extractTs(mode = "chron") %>%
        ts2tibble()

      if(nrow(to) == 0){
        stop("there seems to be no data in the old chronData table")
      }
      if(nrow(tn) == 0){
        stop("there seems to be no data in the new chronData table")
      }
      print(to$dataSetName)

      to <- dplyr::arrange(to,chronData_variableName)

      tn <- dplyr::arrange(tn,chronData_variableName)

      #check TSids are unique
      if(any(duplicated(to$chronData_variableName))){
        print("the original dataset has duplicated chron variableName, can't proceed")
        cl <- c(cl,
                glue::glue("ChronData: Duplicated variable names in original dataset, couldn't log column-level changes.")
        )

        ct <- c(ct,"ChronData table")
        cv <- c(cv,paste(to$chronData_variableName[duplicated(to$chronData_variableName)],collapse = ", "))

        changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)

        return(changelog)

      }

      if(any(duplicated(tn$chronData_variableName))){
        stop("the new dataset has duplicated variableName")
      }

      # Check for added/removed columns -----------------------------------------

      #check for added columns
      if(any(!tn$chronData_variableName %in% to$chronData_variableName)){#then one was added
        wa <- which(!tn$chronData_variableName %in% to$chronData_variableName)
        for(i in wa){
          cl <- c(cl,
                  glue::glue("Column '{tn$chronData_TSid[i]}', with variable name '{tn$chronData_variableName[i]}', was added to the dataset")
          )
          ct <- c(ct,"ChronData table")
          cv <- c(cv,NA)

        }
        #then remove them - we won't describe the details of added columns
        tn <- tn[-wa,]
      }

      #check for removed columns
      if(any(!to$chronData_variableName %in% tn$chronData_variableName)){#then one was added
        wr <- which(!to$chronData_variableName %in% tn$chronData_variableName)
        for(i in wr){
          cl <- c(cl,
                  glue::glue("Column '{to$chronData_TSid[i]}', with variable name '{to$chronData_variableName[i]}', was removed from the dataset")
          )
          ct <- c(ct,"ChronData table")
          cv <- c(cv,NA)

        }
        #then remove them - this should force the datasets to always have the same number of columns
        to <- to[-wr,]
      }
      if(nrow(tn) < 1 | nrow(to) < 1){
        print("there are 0 or 1 matching TSids AND variableNames in the chronData. You probably entered an incorrect file, or fixed a TSid problem")
        cl <- c(cl,
                glue::glue("ChronData: There are 0 or 1 matching TSids AND variableNames in the chronData. You probably entered an incorrect file, or fixed a TSid problem")
        )

        ct <- c(ct,"ChronData table")
        cv <- c(cv,"unknown")

        changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)

        return(changelog)
      }
    }

    #check to make sure that the TSids and number of rows are identical
    tn <- tn %>% dplyr::arrange(chronData_TSid)
    to <- to %>% dplyr::arrange(chronData_TSid)


    # Check base metadata -----------------------------------------------------
    if(!checked.base){
      #make sure the names are present
      ne <- good.base[!good.base %in% names(to)]

      if(length(ne) > 0){
        for(n in ne){
          to <- dplyr::mutate(to,!!n := NA)
        }
      }

      ne <- good.base[!good.base %in% names(tn)]

      if(length(ne) > 0){
        for(n in ne){
          tn <- dplyr::mutate(tn,!!n := NA)
        }
      }

      #filter and collapse to just non-chron metadata




      bto <- dplyr::select(to,!!good.base,
                           starts_with("pub"),
                           starts_with("geo_"),
                           starts_with("funding")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


      #should only be one row now
      if(nrow(bto) != 1){
        stop("The old dataset has discrepencies in metadata between columns that shouldn't exist")
      }


      btn <- dplyr::select(tn,!!good.base,
                           starts_with("pub"),
                           starts_with("geo_"),
                           starts_with("funding")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


      if(nrow(btn) != 1){
        stop("The new dataset has discrepencies in metadata between columns that shouldn't exist")
      }

      tcdf <- dplyr::bind_rows(bto,btn)
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      if(nrow(cldf)>0){
        #fold into changelog
        cl <- c(cl,paste(names(cldf),cldf,sep = ": "))
        #what type of base change?
        for(cli in 1:ncol(cldf)){
          if(grepl("pub[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Publication metadata")
          }else if(grepl("funding[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Funding metadata")
          }else if(grepl("geo_",names(cldf)[cli])){
            ct <- c(ct,"Geographic metadata")
          }else{
            ct <- c(ct,"Base metadata")
          }
        }
        cv <- c(cv,names(cldf))

      }

      checked.base <- TRUE
    }
    # Check chronData column metadata -----------------------------------------

    chronSelect <- function(x,exclude.chron){
      exclude.chron <- exclude.chron[exclude.chron %in% names(x)]

      o <- dplyr::select(x,starts_with("chronData_"),
                         starts_with("interpretation"),
                         starts_with("calibration")) %>%
        dplyr::select(-starts_with("chronData_has"),
                      -!!exclude.chron)
      return(o)}


    #make all columns character for this comparison
    ptn <- chronSelect(tn,exclude.chron) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))
    pto <- chronSelect(to,exclude.chron) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))

    #loop through TSids
    for(i in 1:nrow(ptn)){
      tsi <- ptn$chronData_TSid[i]
      tsname <- ptn$chronData_variableName[i]

      #prep the comparison
      tcdf <- dplyr::bind_rows(pto[i,],ptn[i,])
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      #fold into changelog

      if(nrow(cldf) > 0){
        cl <- c(cl,
                paste(glue::glue("{tsname} ({tsi})"),names(cldf),cldf,sep = ": "))
        ct <- c(ct,
                rep("Chron Column metadata",times = ncol(cldf)))
        cv <- c(cv,names(cldf))

      }
    }



    # compare the chronData_values --------------------------------------------
    for(i in 1:nrow(tn)){
      #change NAs to allow testing
      tov <- to$chronData_values[[i]]
      tov[is.na(tov)] <- -999
      tnv <- tn$chronData_values[[i]]
      tnv[is.na(tnv)] <- -999

      if(is.character(tov) & is.character(tnv)){
        valChange <- !all(tov == tnv)
      }else if(is.numeric(tov) & is.numeric(tnv)){
        valChange <- !all(dplyr::near(tov,tnv))
      }else{

        warning("it seems like the old and new values are of different classes, which is bad. Converting to character for comparison")
        valChange <- !all(as.character(tov) == as.character(tnv))

      }

      if(valChange){
        tsi <- tn$chronData_TSid[i]
        tsname <- tn$chronData_variableName[i]
        cl <- c(cl,
                glue::glue("{tsname} ({tsi}): The size of the chronData_values have changed, from {length(tov)} to {length(tnv)} entries."))
        ct <- c(ct,"ChronData values")
        cv <- c(cv,"chronData_values")
      }else{
        if(!all(tov == tnv)){
          tsi <- tn$chronData_TSid[i]
          tsname <- tn$chronData_variableName[i]
          cl <- c(cl,
                  glue::glue("{tsname} ({tsi}): The chronData_values have changed"))
          ct <- c(ct,"ChronData values")
          cv <- c(cv,"chronData_values")
        }
      }

    }

  }else if(hasChron & length(totest) == 0){
    tn <- Lnew %>%
      extractTs(mode = "chron") %>%
      ts2tibble() %>%
      dplyr::arrange(chronData_TSid)

    #check TSids are unique


    #check for added columns
    wa <- seq_along(tn$chronData_TSid)
    for(i in wa){
      cl <- c(cl,
              glue::glue("Column '{tn$chronData_TSid[i]}', with variable name '{tn$chronData_variableName[i]}', was added to the dataset")
      )
      ct <- c(ct,"ChronData table")
      cv <- c(cv,NA)
    }


  }

  if(length(cl)>0){
    changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = Lnew$dataSetName,lastVersion = lastVers)
  }else{
    changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL,lastVersion = NULL)
  }

  return(changelog)
}


#' Update the changelog entry in a LiPD file with new changes
#'
#' @param L
#' @param changelog
#' @param version
#' @param notes
#' @param curator
#' @param timestamp
#'
#' @return
#' @export
updateChangelog <- function(L,
                            changelog,
                            version = NA,
                            notes = NA,
                            curator = Sys.info()[["user"]],
                            timestamp = lubridate::now(tzone = "UTC")){

  if(nrow(changelog) == 0){#no changes, don't write
    return(L)
  }
  #get the comparison version
  lastVers <- changelog$lastVersion[1]

  #restrict to just type and change for now
  changelog <- dplyr::select(changelog,type,change)

  #get prior changes
  allChanges <- L$changelog

  #prepare changelog
  changes <- changelog %>% dplyr::group_by(type) %>% tidyr::nest()
  #change to list structure
  changelist <- setNames(changes[[2]],changes[[1]]) %>%
    map(as.matrix) %>%
    map(setNames,NULL)

  getVers <- function(x){as.character(x$version)}

  #version check and increment
  if(is.na(version)){#automatically increment version
    lastVers <- max(as.numeric_version(purrr::map_chr(allChanges,getVers)))
    #check to make sure there is a previous version
    if(length(lastVers)==0){
      lastVers <- as.numeric_version("1.0.0") #we'll start here.
    }
    #initialize
    vers <- lastVers
    #increment intelligently
    if(any(grepl("values",names(changelist)))){#minor
      vers[[1,2]] <- lastVers[[1,2]]+1
      vers[[1,3]] <- 0
    }else{#patch
      vers[[1,3]] <- lastVers[[1,3]]+1
    }

  }else{
    #increment intelligently
    if(version == "major"){
      if(is.na(notes)){
        stop("the `notes` field must be present for a major version change")
      }
      lastVers <- max(as.numeric_version(purrr::map_chr(allChanges,getVers)))
      #check to make sure there is a previous version
      if(length(lastVers)==0){
        stop("there don't seem to be any previous versions - version must be specified")
      }
      vers[[1,1]] <- lastVers[[1,1]]+1
      vers[[1,2:3]] <- 0
    }else{
      vers <- as.numeric_version(version)
    }
  }


  if(!is.na(notes)){#add in notes if present
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       notes = notes,
                       changes =  changelist)
  }else{
    #create this instance of the changelog
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       changes =  changelist)
  }

  #update the changes
  L$changelog <- append(list(thisChange),L$changelog)
  L$lipdverseLink <- paste0("https://lipdverse.org/data/",L$datasetId,"/",stringr::str_replace_all(as.character(vers),"[.]","_"))

  return(L)

}


#' Update the changelog entry in a LiPD file with new changes
#'
#' @param L
#' @param changelog
#' @param version
#' @param notes
#' @param curator
#' @param timestamp
#'
#' @return
#' @export
createNewChangelog <- function(L,
                               version = NA,
                               notes = NA,
                               curator = Sys.info()[["user"]],
                               timestamp = lubridate::now(tzone = "UTC")){

  if(!is.null(L$changelog)){#no changes, don't write
    stop("Changelog already exists, use updateChangelog()")
  }

  #get the comparison version
  lastVers <- "none"
  vers <- "0.0.0"

  changelist <- list("Starting new changelog")


  if(!is.na(notes)){#add in notes if present
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       notes = notes,
                       changes =  changelist)
  }else{
    #create this instance of the changelog
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       changes =  changelist)
  }

  #update the changes
  L$changelog <- list(thisChange)

  return(L)

}

#' Get one instance from a LiPD changelog
#'
#' @param L
#' @param version
#'
#' @return
#' @export
getChangelog <- function(L,version = "newest"){
  allvers <- as.numeric_version(purrr::map_chr(L$changelog,"version"))
  if(length(allvers) == 0){
    stop("This dataset does not appear to have a changelog yet. Use createChangelog() and updateChangelog() to add one.")
  }

  if(grepl(pattern = "new",version,ignore.case = T)){
    wv <- which(allvers == max(allvers))
    if(length(wv)>1){wv <- 1}
  }else if(grepl(pattern = "previous",version,ignore.case = T)){
    wv <- which(allvers == sort(allvers,decreasing = T)[2])
  }else if(grepl(pattern = "old",version,ignore.case = T)){
    wv <- which(allvers == min(allvers))
    if(length(wv)>1){wv <- length(allvers)}
  }else{#try to match the version
    wv <- which(allvers == as.numeric_version(version))
    if(length(wv)>1){stop("multiple version matches")}
  }

  if(length(wv) == 0){
    stop("failed to find a match for this version")
  }

  if(length(wv) > 1){
    stop("Found multiple matches for this version. This is bad.")
  }

  #spit it out
  return(L$changelog[[wv]])

}

#' Get the current version of a LiPD file from it's changelog
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
getVersion <- function(L){
  version <- as.numeric_version(purrr::map_chr(L$changelog,"version")) %>%
    max() %>%
    as.character()

  if(length(version)==0){
    version <- "0.0.0"
  }

  return(version)
}

#' Get the timestamp frmo current version of a LiPD file from it's changelog
#'
#' @param L
#'
#' @return
#' @export
getTimestamp <- function(L){
  timestamp <- lubridate::as_datetime(purrr::map_chr(L$changelog,"timestamp")) %>%
    max() %>%
    as.character()

  if(length(timestamp)==0){
    timestamp <- "none"
  }

  return(timestamp)
}

#' write dataset version to a variable in the LiPD file
#'
#' @param L A LiPD file
#'
#' @return A LiPD file
#' @export
writeVersionToRoot <- function(L){
  L$datasetVersion <- getVersion(L)
  L$datasetTimestamp <- getTimestamp(L)

  return(L)
}


#' Create a Markdown representation of a LiPD changelog
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
createMarkdownChangelog <- function(L){

  #Initialize Rmd
  mdcl <- "---" %>%
    str_c("pagetitle: changelog",sep = "\n") %>%
    str_c("output: html_document",sep = "\n") %>%
    str_c("---",sep = "\n") %>%
    str_c("\n") %>%
    str_c("\n\n") %>%
    str_c(glue::glue("# Version history for {L$datasetId} - {L$dataSetName}"))



  if(is.null(L$changelog)){
    mdcl <- mdcl %>%
      str_c("\n\n") %>%
      str_c(glue::glue("No changelog for {L$dataSetName}"))
  }else{
    cl <- L$changelog
    mdcl <- mdcl %>%
      str_c("\n\n")


    for(i in 1:length(cl)){
      mdcl <- mdcl %>%
        str_c(createSingleMarkdownChangelog(cl[[i]])) %>%
        str_c("\n\n")
    }

    mdcl <- stringr::str_replace_all(mdcl,pattern = "''",replacement = "NULL")
  }
  return(mdcl)

}

#' Create markdown for a single entry
#'
#' @param scl
#'
#' @return
#' @export
createSingleMarkdownChangelog<- function(scl){
  #don;t show fourth digit version
  printvers <- as.numeric_version(scl$version)[1,1:3]
  scl$version <- NULL

  clmd <-  glue::glue("### Version: {printvers} \n") %>%
    str_c("\n")

  lev1 <- names(scl)
  for(l1 in 1:length(lev1)){
    tb <- lev1[l1]
    #do level one changes
    if(is.character(scl[[l1]])){#then write the bullets
      clmd <- str_c(clmd, glue::glue("* *{lev1[l1]}*: {scl[[l1]]}\n")) %>%
        str_c("\n")
    }else{
      clmd <- str_c(clmd, glue::glue("* *{lev1[l1]}*:\n")) %>%
        str_c("\n")

      lev2 <- scl[[l1]]

      for(l2 in 1:length(lev2)){
        thisType <- names(lev2)[l2]
        clmd <- str_c(clmd, glue::glue("\t + *{thisType}*:\n",.trim = FALSE))
        theseChanges <- lev2[[l2]]
        for(t in 1:length(theseChanges)){
          clmd <- str_c(clmd, glue::glue("\t \t - {theseChanges[t]}\n",.trim = FALSE))
        }
        clmd <- str_c(clmd,"\n")
      }
    }
  }

  clmd <- str_replace_all(clmd,"\\\\","\\") %>%
    str_remove_all("textemdash")

  return(clmd)

}

#' Create a random Dataset ID for a LiPD dataset
#'
#' @return
#' @export
createDatasetId <- function(){
  return(paste(sample(c(letters,LETTERS,0:9),size = 20,replace = TRUE),collapse = ""))
}


#' Create html changelog details and summaries for the differences between two projects
#'
#' @param Dold
#' @param Dnew
#' @param proj
#' @param projVersOld
#' @param projVersNew
#' @param webDirectory
#'
#' @return
#' @export
createProjectChangelog <- function(Dold,
                                   Dnew,
                                   proj,
                                   projVersOld,
                                   projVersNew,
                                   webDirectory = "~/GitHub/lipdverse/html",
                                   notesTib = NA){

  #figure which are in each compilation
  #old
  TSo <- extractTs(Dold)
  wicO <- inThisCompilation(TSo,proj,projVersOld)
  dsnO <- pullTsVariable(TSo,"dataSetName",strict.search = TRUE)
  allNames <- unique(unlist(sapply(TSo,names)))
  if(!"datasetId"  %in% allNames){
    TSn <- extractTs(Dnew)
    wicN <- inThisCompilation(TSn,proj,projVersNew)
    dsnN <- pullTsVariable(TSn,"dataSetName",strict.search = TRUE)
    dsIdN <- pullTsVariable(TSn,"datasetId",strict.search = TRUE)
    mdfO <- tibble::tibble(dsn = dsnO)
    mdfN <- tibble::tibble(dsn = dsnN,dsid = dsIdN)
    mdf <- dplyr::left_join(mdfO,dplyr::distinct(mdfN),by = "dsn")
    dsIdO <- mdf$dsid
  }else{
    dsIdO <- pullTsVariable(TSo,"datasetId",strict.search = TRUE)
  }
  icOi <- which(purrr::map_lgl(wicO,isTRUE))
  dsIdIcO <- unique(dsIdO[icOi])
  dsnIcO <- unique(dsnO[icOi])
  dataVersOld <- map_chr(dsnIcO,~ getVersion(Dold[[.x]]))

  oT <- tibble::tibble(datasetId = dsIdIcO,
                       dataSetNameOld = dsnIcO,
                       versionOld = dataVersOld)

  #new
  TSn <- extractTs(Dnew)
  wicN <- inThisCompilation(TSn,proj,projVersNew)
  dsnN <- pullTsVariable(TSn,"dataSetName",strict.search = TRUE)
  dsIdN <- pullTsVariable(TSn,"datasetId",strict.search = TRUE)
  icNi <- which(purrr::map_lgl(wicN,isTRUE))
  dsIdIcN <- unique(dsIdN[icNi])
  dsnIcN <- unique(dsnN[icNi])
  dataVersNew <- map_chr(dsnIcN,~ getVersion(Dnew[[.x]]))

  nT <- tibble::tibble(datasetId = dsIdIcN,
                       dataSetNameNew = dsnIcN,
                       versionNew = dataVersNew)


  #combined tibble
  cT <- dplyr::full_join(oT,nT,by = "datasetId")

  #look for removed datasets
  rT <- cT %>%
    dplyr::filter(is.na(dataSetNameNew))

  nRemoved <- nrow(rT)

  #added
  aT <- cT %>%
    dplyr::filter(is.na(dataSetNameOld))

  nAdded <- nrow(aT)

  #go through and check out the differences
  cTg <- cT %>%
    dplyr::filter(!is.na(dataSetNameNew) & !is.na(dataSetNameOld)) %>% #only when both are in there
    dplyr::filter(versionOld != versionNew) #and teh versions are different

  if(!all(is.na(notesTib))){
    notesTib <- dplyr::select(notesTib,datasetId,notes = changes)
    cTg <- dplyr::left_join(cTg,notesTib,by = "datasetId")
  }else{
    cTg$notes <- NA
  }

  #setup big changelog
  bigCl <- tibble::tibble()

  Dchanged <- list()
  bigCl <- tibble::tibble() #for later
  if(nrow(cTg)>0){#then there are changes to record

    for(i in 1:nrow(cTg)){
      tdsid <- cTg$datasetId[i]
      # print(i)
      # print(cTg$dataSetNameOld[i])
      # print(cTg$dataSetNameNew[i])

      cl <- try(createChangelog(Dold[[cTg$dataSetNameOld[i]]],Dnew[[cTg$dataSetNameNew[i]]]))
      if(is(cl,"try-error")){
        changelog <- tibble::tibble(type = "Error", change = cl[1], variable = "Error",dataSetName = NULL,lastVersion = NULL)
        Dchanged[[cTg$dataSetNameNew[i]]] <- updateChangelog(Dnew[[cTg$dataSetNameNew[i]]],
                                                             changelog = changelog,
                                                             notes = cTg$notes[i],
                                                             version = paste0(cTg$versionNew[i],".1000"))#force it to have the same version (with a .1000) for this purpose
      }else{
        if(nrow(cl) > 0){
          bigCl <- dplyr::bind_rows(bigCl,cl)
          Dchanged[[cTg$dataSetNameNew[i]]] <- updateChangelog(Dnew[[cTg$dataSetNameNew[i]]],
                                                               changelog = cl,
                                                               notes = cTg$notes[i],
                                                               version = paste0(cTg$versionNew[i],".1000"))#force it to have the same version (with a .1000) for this purpose
        }
      }
    }
  }

  #create a list that has what's needed for the markdown summary
  mdList <- list(bigCl = bigCl,aT = aT,rT = rT,projVersOld = projVersOld, projVersNew = projVersNew, proj = proj)
  #save a file of data needed for the summary
  save(mdList,file = file.path(webDirectory,"markdownChangelogData.RData"))

  #create the page
  rmarkdown::render(input = file.path(webDirectory,"changelogSummarizer.Rmd"),
                    output_file = file.path(webDirectory,proj,projVersNew,"changelogSummary.html"))


  #create a detailed changelog
  if(nrow(cTg)>0){#then there are changes to record

    mdcl <- glue::glue("# **{proj}**: Detailed changes from version *{projVersOld}* to *{projVersNew}*") %>%
      str_c("\n\n") %>%
      str_c(glue::glue("#### **{date()}**")) %>%
      str_c("\n\n") %>%
      str_c("A summary of changes made to the project is listed [here](changelogSummary.html)") %>%
      str_c("\n\n")
    for(d in 1:length(Dchanged)){
      tcl <- getChangelog(Dchanged[[d]],version = "newest")
      mdcl <- mdcl %>%
        str_c(glue::glue("## {Dchanged[[d]]$dataSetName}")) %>%
        str_c("\n\n") %>%
        str_c(createSingleMarkdownChangelog(tcl)) %>%
        str_c("\n\n")

      # if(is(tmdcl,"try-error")){
      #   mdcl <- mdcl %>%
      #                 glue::glue("## {Dchanged[[d]]$dataSetName}") %>%
      #                 str_c("\n\n") %>%
      #                 str_c("changelog includes special characters that are breaking createProjectChangelog. Skipping.")
      # }else{
      #   mdcl <- tmdcl
      #}
    }

    mdcl <- stringr::str_replace_all(mdcl,pattern = "''",replacement = "NULL")

  }else{

    mdcl <- glue::glue("## **{proj}** version *{projVersNew}* There do not appear to have been any changes to the datasets.")
  }

  write_file(mdcl,file.path(webDirectory,proj,projVersNew,"changelogDetail.Rmd"))

  rmarkdown::render(file.path(webDirectory,proj,projVersNew,"changelogDetail.Rmd"),output_file = file.path(webDirectory,proj,projVersNew,"changelogDetail.html"))
}


