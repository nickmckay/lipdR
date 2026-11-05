#' Convert a LiPD object into a Neotoma site
#'
#' Converts a LiPD object into a `neotoma2` `site` object, enabling round-tripping
#' between the LiPD and Neotoma ecosystems. Requires the suggested packages `neotoma2`
#' and `sf`.
#'
#' @param L a LiPD object
#' @importFrom methods new
#' @export
#' @return a `neotoma2` site object
#' @examples
#' \dontrun{
#' L <- readLipd()
#' site <- lipd2neotoma(L)
#' }
lipd2neotoma <- function(L){

  if (!requireNamespace("neotoma2", quietly = TRUE)) {
    stop(
      "Package 'neotoma2' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
      call. = FALSE
    )
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' must be installed to use this function. Install it from github using `install.packages('sf')`",
      call. = FALSE
    )
  }

  #save measurement tables
  mtabs1 <- getMeasurementTables(L)


  #######################################################################
  #paleoData
  #######################################################################

  #grab paleoData names and dataframe
  for (j in seq_along(grep("paleo", attributes(mtabs1)$names))){
    paleoTabIndex <- grep("paleo", attributes(mtabs1)$names)[j]
    paleoTabName <- attributes(mtabs1)$names[paleoTabIndex]
    PD1 <- mtabs1[[paleoTabIndex]]

    #initiate list for neotoma "samples"
    allSamps <- list()

    #iterate over all ages (sample layers)
    for (k in 1:nrow(PD1)){
      #cat(k, "\n")

      sampleTab <- PD1[k,!is.na(PD1[k,])]

      sampleTabNames <- names(sampleTab)

      # Identify the 'age' column (required). Prefer an exact match, then any
      # column whose name contains "age".
      pullAge <- which(tolower(sampleTabNames) %in% "age")
      if(length(pullAge) < 1){
        pullAge <- which(grepl("age", tolower(sampleTabNames)))
      }
      if(length(pullAge) < 1){
        stop("No 'age' column found in the measurement table; cannot convert to Neotoma")
      }
      var1 <- pullAge[1]

      # Identify the 'depth' column (optional).
      pullDepth <- which(tolower(sampleTabNames) %in% "depth")
      if(length(pullDepth) < 1){
        pullDepth <- which(grepl("depth", tolower(sampleTabNames)))
      }
      var2 <- if(length(pullDepth) >= 1) pullDepth[1] else NA_integer_

      pullAgeDepth <- c(sampleTabNames[var1], sampleTabNames[var2])
      pullAgeDepth <- pullAgeDepth[!is.na(pullAgeDepth)]
      notAgeDepth <- which(!sampleTabNames %in% pullAgeDepth)

      neoSamples <- data.frame(matrix(ncol = 10, nrow = length(notAgeDepth), data=NA))
      colnames(neoSamples) <- c("units", "value", "context", "element", "taxonid", "symmetry", "taxongroup", "elementtype", "variablename", "ecologicalgroup")

      rowCt <- 0
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
        neoSamples[rowCt,2] <- as.integer(sampleTab[i])
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
        ChronID <- NA
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
      if(length(pullDepth) >= 1){
        sample1@depth <- as.numeric(sampleTab[[pullDepth[1]]])
      }
      sample1@ages <- ages1

      allSamps[[k]] <- sample1


    }



    # Derive a Neotoma datasetid from the original data URL when available,
    # otherwise leave it unset.
    datasetIdFromUrl <- NA_integer_
    if(!is.null(L$originalDataUrl) && is.character(L$originalDataUrl) && nzchar(L$originalDataUrl)){
      urlParts <- strsplit(L$originalDataUrl, "/")[[1]]
      datasetIdFromUrl <- suppressWarnings(as.integer(urlParts[length(urlParts)]))
    }
    dataset1 <- neotoma2::set_dataset(datasetid = datasetIdFromUrl)

    # Initialise the samples slot (NULL by default) before populating it
    dataset1@samples <- methods::new("samples")
    for (i in 1:length(allSamps)){
      dataset1@samples@samples[[i]] <- allSamps[[i]]
    }

    datasetAll <- new("datasets")
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

  # Initialise the collunits slot (NULL by default) before populating it
  site1@collunits <- methods::new("collunits")
  site1@collunits@collunits[[1]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = chronos1, colldate = as.Date(character(0)))

  if(!is.null(L$geo$neotomaSiteId)){
    site1@siteid <- as.integer(L$geo$neotomaSiteId)
  }
  if(!is.null(L$geo$siteName)){
    site1@sitename <- L$geo$siteName
  }
  if(!is.null(L$geo$elevation)){
    site1@altitude <- as.numeric(L$geo$elevation)
  }

  site1@geography = sf::st_as_sf(sf::st_sfc(sf::st_point(c(L$geo$longitude,L$geo$latitude))))

  if(!is.null(L$geo$description)){
    site1@description <- L$geo$description
  }

  return(site1)

}

