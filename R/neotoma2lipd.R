


#' convert lipd format to neotoma
#'
#' @param L
#'
#' @return neotoma site
#' @import neotoma2
#' @export
#'
lipd2neotoma <- function(L){
  #ensure neotoma2 is loaded
  if (!requireNamespace("neotoma2", quietly = TRUE)) {
    stop(
      "Package 'neotoma2' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
      call. = FALSE
    )
  }
  if (L$createdBy == "neotoma2lipd"){
    site1 <- fromOrigNeotoma(L)
  }else{
    site1 <- fromOrigLipd(L)
  }
  return(site1)
}


#' convert an original lipd dataset into a new neotoma site
#'
#' @param L
#'
#' @return neotoma site
#'
fromOrigLipd <- function(L){
  #initiate site and set site-level metadata
  site1 <- neotoma2::set_site()

  #note any unplaced metadata at the site level
  siteSlots <- getSlots("site")
  whichChronMetaNotPlaced <- names(L)[!names(L) %in% c(names(getSlots("site")))]
  whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% c("paleoData", "chronData")]

  if (!is.null(L$dataSetName)){
    slot(site1, "sitename") <- L$dataSetName
    whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% "dataSetName"]
  }
  if (!is.null(L$geo$elevation)){
    slot(site1, "altitude") <- L$geo$elevation
    whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% "geo"]
  }
  if (!is.null(L$geo$longitude) & !is.null(L$geo$longitude)){
    slot(site1, "geography") <- st_as_sf(st_sfc(st_point(c(L$geo$longitude,L$geo$latitude))))
    whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% "geo"]
  }
  if (!is.null(L$geo$description)){
    slot(site1, "description") <- L$geo$description
    whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% "geo"]
  }


  warning(paste0("The following site (lipd dataset) metadata could not be placed: ", paste(whichChronMetaNotPlaced, collapse = " "), "\n",
                 "Consider renaming to one of the controlled neotoma terms: ", paste(siteSlots, collapse = " "),"\n"))

  #######################################################################
  #paleoData
  #######################################################################

  #loop over paleoData objects
  if (length(grep("paleo", attributes(L)$names)) > 0){
    #grab measurement tables
    paleo1 <- getMeasurementTables(L, "paleo")
    #space for paleoData-object-level metadata

    #iterate over paleoData objects - equivalent to collunit in Neotoma (parts of the collunit are also in chronData)
    for (jj in 1:length(L$paleoData)){
      datasetAll <- new("datasets")
      site1@collunits@collunits[[jj]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = new("chronologies"), colldate = as.Date(character(0)))
      #loop over measurement tables
      if (length(L$paleoData[[jj]]) > 0){
        #space for measurementTable-object-level metadata

        #iterate over paleoData measurement tables - equivalent to dataset in Neotoma
        for (j in 1:length(L$paleoData[[jj]])){

          PD1 <- L$paleoData[[jj]]$measurementTable[[j]]
          sampleTab <- paleo1[[eval(paste0("paleo", jj, "meas", j))]]
          sampleTabNames <- names(sampleTab)

          allSampleKeys <- list()
          for (i in 1:length(sampleTabNames)){
            allSampleKeys[[i]] <- names(PD1[sampleTabNames[i]][[1]])
          }
          sampleCols <- unique(unlist(allSampleKeys))

          #initiate list for neotoma "samples"
          allSamps <- list()

          pullAge <- which(tolower(sampleTabNames) == "age")
          pullDepth <- which(tolower(sampleTabNames) == "depth")
          pullYear <- which(tolower(sampleTabNames) == "year")

          #iterate over all ages (sample layers) to build a data frame at each layer
          for (k in 1:nrow(sampleTab)){

            pullAgeDepthYear <- c(sampleTabNames[pullAge], sampleTabNames[pullDepth], sampleTabNames[pullYear])
            notAgeDepthYear <- which(!sampleTabNames %in% pullAgeDepthYear)

            neoSamples <- data.frame(matrix(ncol = length(sampleCols), nrow = length(notAgeDepthYear), data=NA))
            colnames(neoSamples) <- sampleCols

            #iterate over each variable (i) of the sample df (k)
            for (i in notAgeDepthYear){
              #iterate over each property (ii) for a given variable (i)
              for (ii in 1:length(sampleCols)){
                #check that the given property exists for this variable
                if (is.null(PD1[sampleTabNames[i]][[1]][eval(sampleCols[ii])][[1]])){
                  neoSamples[i,ii] <- NA
                }else{
                  #
                  if (grepl("value", sampleCols[ii])){
                    neoSamples[i,ii] <- PD1[sampleTabNames[i]][[1]][eval(sampleCols[ii])][[1]][k]
                  }else{
                    neoSamples[i,ii] <- PD1[sampleTabNames[i]][[1]][eval(sampleCols[ii])][[1]][1]
                  }
                }
              }
            }

            #build sample and assign sample slots
            #note - there is not currently a place to store sample-level metadata in lipd!
            sample1 <- new("sample")
            sample1@datum <- neoSamples
            if (length(pullAge)>0){
              sample1@ages <- as.numeric(PD1[sampleTabNames[pullAge]][[1]][eval(sampleCols[ii])][[1]][k])
            }
            if (length(pullDepth)>0){
              sample1@depth <- as.numeric(PD1[sampleTabNames[pullDepth]][[1]][eval(sampleCols[ii])][[1]][k])
            }
            allSamps[[k]] <- sample1
          }

          #Organize samples and datasets
          dataset1 <- neotoma2::set_dataset()
          for (i in 1:length(allSamps)){
            dataset1@samples@samples[[i]] <- allSamps[[i]]
          }
          #Find and store metadata for  dataset slots
          datasetSlots <- names(getSlots("dataset"))
          whichMetadata <- !names(PD1) %in% names(sampleTab)
          sampleMetaNames <- names(PD1[whichMetadata])
          datasetMetaNotPlaced <- sampleMetaNames[!sampleMetaNames %in% datasetSlots]
          warning(paste0("The following dataset (paleoData measurement table) metadata could not be placed: ", paste(datasetMetaNotPlaced, collapse = " "), "\n",
                         "Consider renaming to one of the controlled neotoma terms: ", paste(siteSlots, collapse = " "),"\n"))

          sampleMetaNames <- sampleMetaNames[sampleMetaNames %in% datasetSlots]

          datasetMetaNotPlaced <- sampleMetaNames[sampleMetaNames %in% datasetSlots]

          if (length(sampleMetaNames)>0){
            for (kk in sampleMetaNames){
              if(.hasSlot(dataset1, kk)){
                slot(dataset1, kk) <- PD1[eval(kk)][[1]]
              }
            }
          }
          datasetAll@datasets[[j]] <- dataset1
        }
      }
      slot(site1@collunits@collunits[[jj]], "datasets") <- datasetAll
    }
  }

  #######################################################################
  #chronData
  #######################################################################

  if (length(grep("chron", attributes(L)$names)) > 0){
    #grab measurement tables
    chron1 <- getMeasurementTables(L, "chron")
    #space for chronData-object-level metadata

    #iterate over chronData objects - equivalent to collunit in Neotoma (parts of the collunit are also in paleoData)
    for (jj in 1:length(L$chronData)){
      chronos1 <- new("chronologies")

      #loop over measurement tables
      if (length(L$chronData[[jj]]) > 0){
        #space for measurementTable-object-level metadata

        #iterate over chronData measurement tables - equivalent to dataset in Neotoma
        for (j in 1:length(L$chronData[[jj]])){
          #copy lipd "chron data measurement table" as neotoma "chron controls" table
          chronos1@chronologies[[j]] <- new("chronology")
          chronos1@chronologies[[j]]@chroncontrols <- chron1[eval(paste0("chron",jj,"meas",j))]
          #fill other matching neotoma "chronology" slots with chron data measurement-table-level metadata
          chrnSlots <- unlist(names(getSlots("chronology")))
          haveChronoSlots <- names(getSlots("chronology"))[names(getSlots("chronology")) %in% names(L$chronData[[jj]]$measurementTable[[j]])]
          for (zz in haveChronoSlots){
            slot(chronos1@chronologies[[j]], zz) <- L$chronData[[jj]]$measurementTable[[j]][eval(zz)][[1]]
          }

          #which metadata could not be placed?
          whichChronMetaNotPlaced <- names(L$chronData[[jj]]$measurementTable[[j]]) %in% c(names(getSlots("chronology")), names(chron1[eval(paste0("chron",jj,"meas",j))][[1]]))
          if(any(whichChronMetaNotPlaced)){
            warning(paste0("The following chronology (chronData measurement table) metadata could not be placed: ", names(L$chronData[[jj]]$measurementTable[[j]])[!whichChronMetaNotPlaced], "\n",
                           "Consider renaming to one of the controlled neotoma terms: ", paste(chrnSlots, collapse = " "),"\n"))
          }
        }
      }
      slot(site1@collunits@collunits[[jj]], "chronologies") <- chronos1
    }
  }



  return(site1)
}


#' Convert lipd back to its original neotoma format
#'
#' @param L lipd object
#' @importFrom methods new
#' @importFrom sf st_as_sf st_sfc st_point
#'
#' @return neotoma site
#'
fromOrigNeotoma <- function(L){

  #save measurement tables
  mtabs1 <- getMeasurementTables(L)


  datasetAll <- new("datasets")
  #######################################################################
  #paleoData
  #######################################################################

  #grab paleoData names and dataframe
  for (j in 1:length(grep("paleo", attributes(mtabs1)$names))){

    paleoTabIndex <- grep("paleo", attributes(mtabs1)$names)[j]
    paleoTabName <- attributes(mtabs1)$names[paleoTabIndex]
    PD1 <- mtabs1[[paleoTabIndex]]

    #initiate list for neotoma "samples"
    allSamps <- list()

    sampleTab <- PD1[,!is.na(PD1[j,])]

    sampleTabNames <- names(sampleTab)

    if (j==1){
      pullAge <- which(tolower(sampleTabNames) %in% "age")
      if(length(pullAge)<1){
        ans1 <- 0
        message("No clear age column")
        pullAge <- grepl("age",tolower(sampleTabNames))
        for(h in which(pullAge)){
          ans1 <- 0
          while (!ans1 %in% c("y","n")){
            ans1 <- readline(prompt = paste0("Is this your 'age' column header? ", sampleTabNames[h], " (y/n): "))
            if (ans1 == "y"){
              var1 <- h
              message(paste0("Okay, setting 'age' column to ", sampleTabNames[var1]))
              break
            }
          }
        }
        if (ans1 != "y"){
          var1Check <- FALSE
          while (!var1Check){
            message("column headers: ")
            lapply(1:length(sampleTabNames), function(x) cat(paste0(x, " ", sampleTabNames[x], "\n")))

            var1 <- readline(prompt = paste0("Enter the index corresponding to your 'age' column from 1 to ", length(pullAge),
                                             ". Enter 0 if no 'age' column: "))
            var1 <- as.numeric(var1)
            if (!is.na(var1)){
              if (var1 >= 0 & var1 <= length(pullAge)){
                var1Check <- TRUE
                if (var1==0){
                  message("Okay, no 'age' column")
                }else{
                  message(paste0("Okay, setting 'age' column to ", sampleTabNames[var1]))
                }
              }
            }
          }
        }
        pullAge <- var1
      }

      pullDepth <- which(tolower(sampleTabNames) %in% "depth")
      if(length(pullDepth)<1){
        ans1 <- 0
        message("No clear depth column")
        pullDepth <- grepl("depth",tolower(sampleTabNames))
        for(h in which(pullDepth)){
          ans1 <- 0
          while (!ans1 %in% c("y","n")){
            ans1 <- readline(prompt = paste0("Is this your 'depth' column header? ", sampleTabNames[h], " (y/n): "))
            if (ans1 == "y"){
              var2 <- h
              message(paste0("Okay, setting 'depth' column to ", sampleTabNames[var2]))
              break
            }
          }
        }
        if (ans1 != "y"){
          var1Check <- FALSE
          while (!var1Check){
            message("column headers: ")
            lapply(1:length(sampleTabNames), function(x) cat(paste0(x, " ", sampleTabNames[x], "\n")))

            var2 <- readline(prompt = paste0("Enter the index corresponding to your 'depth' column from 1 to ", length(pullAge),
                                             ". Enter 0 if no 'depth' column: "))
            var2 <- as.numeric(var2)
            if (!is.na(var2)){
              if (var1 >= 0 & var2 <= length(pullAge)){
                var1Check <- TRUE
                if (var2==0){
                  message("Okay, no 'depth' column")
                }else{
                  message(paste0("Okay, setting 'depth' column to ", sampleTabNames[var2]))
                }
              }
            }
          }
        }
        pullDepth <- var2
      }
    }



    #iterate over all ages (sample layers)

    for (k in 1:nrow(sampleTab)){
      rowCt <- 0

      pullAgeDepth <- c(sampleTabNames[var1], sampleTabNames[var2])
      notAgeDepth <- which(!sampleTabNames %in% pullAgeDepth)

      neoSamples <- data.frame(matrix(ncol = 10, nrow = length(notAgeDepth), data=NA))
      colnames(neoSamples) <- c("units", "value", "context", "element", "taxonid", "symmetry", "taxongroup", "elementtype", "variablename", "ecologicalgroup")



      for (i in notAgeDepth){
        rowCt <- rowCt + 1
        units1 <- L$paleoData[[1]]$measurementTable[[1]][sampleTabNames[i]][[1]]$units
        if(length(units1)>0){
          neoSamples[rowCt,1] <- units1
        }
        element1 <- L$paleoData[[1]]$measurementTable[[1]][sampleTabNames[i]][[1]]$element
        if(length(element1)>0){
          neoSamples[rowCt,4] <- element1
        }
        taxonid1 <- L$paleoData[[1]]$measurementTable[[1]][sampleTabNames[i]][[1]]$taxonid
        if(length(taxonid1)>0){
          neoSamples[rowCt,5] <- taxonid1
        }
        taxongroup1 <- L$paleoData[[1]]$measurementTable[[1]][sampleTabNames[i]][[1]]$taxongroup
        if(length(taxongroup1)>0){
          neoSamples[rowCt,7] <- taxongroup1
        }
        ecologicalgroup1 <- L$paleoData[[1]]$measurementTable[[1]][sampleTabNames[i]][[1]]$ecologicalgroup
        if(length(ecologicalgroup1)>0){
          neoSamples[rowCt,10] <- ecologicalgroup1
        }

        neoVars <- strsplit(sampleTabNames[i], "_")[[1]]
        numVars <- length(neoVars)
        neoSamples[rowCt,2] <- as.integer(sampleTab[k,i])
        if(length(grep("undiff", neoVars[1])) > 0){
          neoVars[1] <- paste0(strsplit(neoVars[1], "undiff")[[1]][1], " undiff.")
        }
        if(length(grep("type", neoVars[1])) > 0){
          neoVars[1] <- paste0(strsplit(neoVars[1], "type")[[1]][1], "-type")
        }
        if(numVars == 1){
          neoSamples[rowCt,9] <- neoVars[1]
        }else if (numVars == 2){
          neoSamples[rowCt,9] <- neoVars[1]
          neoSamples[rowCt,8] <- neoVars[2]
        }else if (numVars == 3){
          neoSamples[rowCt,9] <- neoVars[1]
          neoSamples[rowCt,8] <- neoVars[2]
          neoSamples[rowCt,3] <- neoVars[3]
        }
      }


      neoSamples$element <- neoSamples$elementtype
      neoSamples <- neoSamples[order(neoSamples$units, neoSamples$value, neoSamples$context, neoSamples$element, as.numeric(neoSamples$taxonid)),]

      if (length(L$paleoData[[1]]$measurementTable[[1]]$age$units)>0){
        ageType <- L$paleoData[[1]]$measurementTable[[1]]$age$units
      }else{
        ageType <- NA
      }

      if (pullAge==0){
        age1 <- NA
      }else{
        age1 <- sampleTab[pullAge]
      }


      if (length(L$chronData[[1]]$measurementTable[[1]]$ageYoung$TSid)>0){
        split1 <- strsplit(L$chronData[[1]]$measurementTable[[1]]$ageYoung$TSid, "_")
        ChronID <- strsplit(split1[[1]][2], "ageYoung")
        ChronID <- as.integer(ChronID[[1]][1])
      }else{
        ChronID <- 9999
      }


      ages1 <- data.frame("age" = age1,
                          "agetype" = ageType,
                          "ageolder" = NA,
                          "ageyounger" = NA,
                          "chronologyid" = ChronID,
                          "chronologyname" = NA,
                          "row.names" = 1)


      sample1 <- new("sample")

      sample1@datum <- neoSamples
      sample1@depth <- as.integer(sampleTab[k,pullDepth])
      sample1@ages <- ages1

      allSamps[[k]] <- sample1


    }


    if (!is.null(L$originalDataUrl)){
      dataset1 <- neotoma2::set_dataset(datasetid = strsplit(L$originalDataUrl, "/")[[1]][length(strsplit(L$originalDataUrl, "/")[[1]])])
    }else{
      message("Setting dataset ID as 9999")
      dataset1 <- neotoma2::set_dataset(datasetid = 9999)
    }


    for (i in 1:length(allSamps)){
      dataset1@samples@samples[[i]] <- allSamps[[i]]
    }


    datasetAll@datasets[[j]] <- dataset1

  }


  #######################################################################
  #chronData
  #######################################################################

  chronos1 <- new("chronologies")

  for (j in 1:sum(grepl("chron", attributes(mtabs1)$names))){

    chronTabIndex <- grep("chron", attributes(mtabs1)$names)[j]
    CD1 <- mtabs1[[chronTabIndex]]

    chronos1@chronologies[[j]] <- new("chronology")

    chronos1@chronologies[[j]]@chroncontrols <- CD1

    chronos1@chronologies[[j]]@chronologyid <- as.integer(ChronID[[1]][1])
  }




  site1 <- neotoma2::set_site()



  site1@collunits@collunits[[1]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = chronos1, colldate = as.Date(character(0)))

  if (!is.null(L$geo$neotomaSiteId)){
    site1$siteid <- L$geo$neotomaSiteId
  }else{
    message("Neotoma site ID set to 9999")
    site1$siteid <- 9999
  }


  ans2 <- askYesNo(paste0("Is the LiPD dataset name going to be the Neotoma site name (", L$dataSetName, ")?\n"))
  if (ans2){
    site1$sitename <- L$dataSetName
  }else{
    site1$sitename <- readline(prompt = "Please enter a site name: ")
  }


  #site1$sitename <- L$geo$siteName
  # site1$lat <- lipdR:::getGeoNeotoma2(D@sites[[1]])$latitude
  # site1$long <- lipdR:::getGeoNeotoma2(D@sites[[1]])$longitude
  site1$altitude <- L$geo$elevation

  site1@geography = st_as_sf(st_sfc(st_point(c(L$geo$longitude,L$geo$latitude))))

  if (!is.null(L$geo$description)){
    site1@description <- L$geo$description
  }


  return(site1)

}


