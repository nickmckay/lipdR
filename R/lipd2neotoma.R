#' Convert lipd to neotoma
#'
#' @param L lipf object
#' @importFrom methods new
#'
#' @return neotoma site
#' @export
#'

#please note that this is a beta version for testing, there are many bugs here still

lipd2neotoma <- function(L){

  if (!requireNamespace("neotoma2", quietly = TRUE)) {
    stop(
      "Package 'neotoma2' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
      call. = FALSE
    )
  }

  #save measurement tables
  mtabs1 <- getMeasurementTables(L)

  #######################################################################
  #paleoData
  #######################################################################

  #grab paleoData names and dataframe
  lipdPDnames <- colnames(mtabs1$paleo1meas1)
  PD1 <- mtabs1$paleo1meas1

  #initiate list for neotoma "samples"
  allSamps <- list()

  #iterate over all ages (sample layers)
  for (k in 1:length(PD1$age)){

    sampleTab <- PD1[k,!is.na(PD1[k,])]

    sampleTabNames <- names(sampleTab)

    pullAgeDepth <- which(sampleTabNames %in% c("age", "depth"))
    notAgeDepth <- which(!sampleTabNames %in% c("age", "depth"))

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

      neoVars <- str_split(sampleTabNames[i], "_")[[1]]
      numVars <- length(neoVars)
      neoSamples[rowCt,2] <- as.integer(sampleTab[i])
      if(length(grep("undiff", neoVars[1])) > 0){
        neoVars[1] <- paste0(str_split(neoVars[1], "undiff")[[1]][1], " undiff.")
      }
      if(length(grep("type", neoVars[1])) > 0){
        neoVars[1] <- paste0(str_split(neoVars[1], "type")[[1]][1], "-type")
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

    ageType <- L$paleoData[[1]]$measurementTable[[1]]$age$units
    split1 <- strsplit(L$chronData[[1]]$measurementTable[[1]]$ageYoung$TSid, "_")
    ChronID <- strsplit(split1[[1]][2], "ageYoung")

    ages1 <- data.frame("age" = sampleTab$age,
                        "agetype" = ageType,
                        "ageolder" = NA,
                        "ageyounger" = NA,
                        "chronologyid" = as.integer(ChronID[[1]][1]),
                        "chronologyname" = NA,
                        row.names = 1)


    sample1 <- new("sample")

    sample1@datum <- neoSamples
    sample1@depth <- sampleTab$depth
    sample1@ages <- ages1

    allSamps[[k]] <- sample1


  }



  dataset1 <- neotoma2::set_dataset(datasetid = strsplit(L$originalDataUrl, "/")[[1]][length(strsplit(L$originalDataUrl, "/")[[1]])])

  for (i in 1:length(allSamps)){
    dataset1@samples@samples[[i]] <- allSamps[[i]]
  }


  #######################################################################
  #chronData
  #######################################################################


  datasetAll <- new("datasets")
  datasetAll@datasets[[1]] <- dataset1

  chronos1 <- new("chronologies")
  chronos1@chronologies[[1]] <- new("chronology")

  chronos1@chronologies[[1]]@chroncontrols <- mtabs1$chron1meas1

  chronos1@chronologies[[1]]@chronologyid <- as.integer(ChronID[[1]][1])


  site1 <- neotoma2::set_site()



  site1@collunits@collunits[[1]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = chronos1, colldate = as.Date(character(0)))


  return(site1)

}
