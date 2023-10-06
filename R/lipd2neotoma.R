


#' convert lipd format to neotoma
#'
#' @param L
#'
#' @return neotoma site
#' @import neotoma2
#' @export lipd2neotoma
#'
lipd2neotoma <- function(L){
  #ensure neotoma2 is loaded
  if (!requireNamespace("neotoma2", quietly = TRUE)) {
    stop(
      "Package 'neotoma2' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
      call. = FALSE
    )
  }
  #don't use fromOrigNeotoma() yet, not finished
  # if (L$createdBy == "neotoma2lipd"){
  #   site1 <- fromOrigNeotoma(L)
  # }else{
  #   site1 <- fromOrigLipd(L)
  # }
  site1 <- fromOrigLipd(L)
  return(site1)
}


#' convert an original lipd dataset into a new neotoma site
#'
#' @param L
#'
#' @return neotoma site
#'
fromOrigLipd <- function(L){

  #define the lithology class if it does not exist
  # if(!isClass("lithology")){
  #   setClass("lithology",representation(lithologyid="integer",
  #                                       depthtop="numeric",
  #                                       depthbottom="numeric",
  #                                       lowerboundary="character",
  #                                       description="character",
  #                                       recdatecreated="Date",
  #                                       recdatemodified="Date"))
  # }


  #initiate site and set site-level metadata
  site1 <- neotoma2::set_site()

  #note any unplaced metadata at the site level
  siteSlots <- names(getSlots("site"))
  whichChronMetaNotPlaced <- names(L)[!names(L) %in% siteSlots]
  whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% c("paleoData", "chronData")]

  if (!is.null(L$dataSetName)){
    slot(site1, "sitename") <- L$geo$siteName
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

  #ignore these lipd-specific metadata
  whichChronMetaNotPlaced <- whichChronMetaNotPlaced[!whichChronMetaNotPlaced %in% c("@context", "createdBy", "archiveType", "lipdVersion" )]

  if(length(whichChronMetaNotPlaced)>0){
    message(paste0("The following site (lipd dataset) metadata could not be placed: ", paste(whichChronMetaNotPlaced, collapse = " "), "\n",
                   "Consider renaming to one of the controlled neotoma terms: ", paste(siteSlots, collapse = " "),"\n"))
  }



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
      site1@collunits@collunits[[jj]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = new("chronologies"), colldate = as.Date(character(1)))


      #set collunit metadata

      possibleLipdCollunitKeys <- c("neotomaCollectionUnitId",
                            "notes",
                            "neotomaHandle",
                            "collectionDate",
                            "location",
                            "waterDepth",
                            "gpsLocation",
                            "neotomaCollectionUnitType",
                            "collectionDevice",
                            "paleoDataName",
                            "depositionalEnvironment",
                            "linkedChronData"
      )
      lipdCollunitKeys <- names(L$paleoData[[jj]])[names(L$paleoData[[jj]]) %in% possibleLipdCollunitKeys]

      for (rr in lipdCollunitKeys){
        if (rr == "neotomaHandle"){
          aa = "handle"
        }else if (rr == "collectionDate"){
          aa = "colldate"
        }else if (rr == "paleoDataName"){
          aa = "collectionunitname"
        }else if (rr == "depositionalEnvironment"){
          aa = "depositionalenvironment"
        }else if (rr == "collectionDevice"){
          aa = "collectiondevice"
        }else if (rr == "neotomaCollectionUnitType"){
          aa = "collunittype"
        }else if (rr == "gpsLocation"){
          aa = "gpslocation"
        }else if (rr == "waterDepth"){
          aa = "waterdepth"
        }else if (rr == "neotomaCollectionUnitId"){
          aa = "collectionunitid"
        }else if (rr == "linkedChronData"){
          aa = "defaultchronology"
        }else{
          aa=rr
        }
        slot(site1@collunits@collunits[[jj]], eval(aa)) <- unname(L$paleoData[[jj]][eval(rr)])[[1]]
      }

      ###############################################################################################
      #link any associated chron data

      #if there is a default chronology (linked chron data), go find it
      if (!is.na(slot(site1@collunits@collunits[[jj]], "defaultchronology"))){
        chronNow <- slot(site1@collunits@collunits[[jj]], "defaultchronology")

        chronNum <- which(chronNow == unlist(lapply(L2$paleoData, function(x) x$linkedChronData)))

        if (length(chronNum) < 1){
          message("No chronology found, please make explicit link to chronData.")
          message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
        }
      }#otherwise, report lack of chron data
      else{
        message("No chronology found, please make explicit link to chronData.")
        message("See documentation here: https://docs.google.com/document/d/1BvVcnj1VfDscIAwV5vEM4CIVwRLtRz74hqW9-8j6icM/edit?usp=sharing")
      }



      ###############################################################################################

      #loop over measurement tables
      if (length(L$paleoData[[jj]]$measurementTable) > 0){



        #iterate over paleoData measurement tables - equivalent to dataset or lithology in Neotoma
        for (j in 1:length(L$paleoData[[jj]]$measurementTable)){

          if (length(L$paleoData[[jj]]$measurementTable[[j]]$tableName) < 1){
            stop(paste0("at paeloData ", jj, " measurementTable ", j, " 'tableName' must be defined"))
          }


          if(L$paleoData[[jj]]$measurementTable[[j]]$tableName == "dataset"){
            ###########################################################################
            #Add new dataset
            ###########################################################################

            if (!is.null(L$paleoData[[jj]]$paleoDataId)){
              paleoNow <- L$paleoData[[jj]]$paleoDataId
            }

            ###########################################################################
            #Link any chron data
            if (!is.null(L$paleoData[[jj]]$measurementTable[[j]]$measurementTableId) & !is.null(chronNum)& !is.null(paleoNow)){
              #use the known paleo-chron linkage to find measurement table
              chronTabNow <- L$paleoData[[jj]]$measurementTable[[j]]$linkedMeasurementTable
              chronTabNum <- which(chronTabNow == unlist(lapply(L2$chronData[[chronNum]]$measurementTable, function(x) x$measurementTableId)))
              #and model
              modelNow <- L$paleoData[[jj]]$measurementTable[[j]]$linkedModel
              modelNum <- which(modelNow == unlist(lapply(L2$chronData[[chronNum]]$model, function(x) x$modelId)))
              #and summary table
              if (is.numeric(modelNum)){
                summaryTabNow <- L$paleoData[[jj]]$measurementTable[[j]]$linkedSummaryTable
                summaryTabNum <- which(summaryTabNow == unlist(lapply(L2$chronData[[chronNum]]$model[[modelNum]]$summaryTable, function(x) x$summaryTableId)))
              }


              if (length(chronTabNum) < 0){
                message(paste0("The paleodata (", paleoNow, ") and chronData (", chronNow, ") link was established, but the measurement table linkage was not found."))
              }
            }

            ###########################################################################
            #Build new chronology
            chronos1 <- new("chronologies")
            #copy lipd "chron data measurement table" as neotoma "chron controls" table
            chronos1@chronologies[[j]] <- new("chronology")
            chron1 <- getMeasurementTables(L, "chron")
            chronos1@chronologies[[j]]@chroncontrols <- chron1[eval(paste0("chron",chronNum,"meas",chronTabNum))][[1]]
            #fill other matching neotoma "chronology" slots with chron data measurement-table-level metadata
            chrnSlots <- unlist(names(getSlots("chronology")))[!unlist(names(getSlots("chronology"))) %in% c("agemodel", "modelagetype")]
            for (zz in chrnSlots){
              if (zz == "chronologyid"){
                aa = "measurementTableId"
              }else if (zz == "dateprepared"){
                aa = "neotomaDatePrepared"
              }else if (zz == "chronologyname"){
                aa = "neotomaChronologyname"
              }else {
                aa = zz
              }
              if (!is.null(L$chronData[[chronNum]]$measurementTable[[chronTabNum]][eval(aa)][[1]][[1]])){
                slot(chronos1@chronologies[[j]], zz) <- L$chronData[[chronNum]]$measurementTable[[chronTabNum]][eval(aa)][[1]][[1]]
              }
            }
            if (!is.null(chronNum) & !is.null(modelNum)){
              slot(chronos1@chronologies[[j]], "agemodel") <- L$chronData[[chronNum]]$model[[modelNum]]$methods$method
              slot(chronos1@chronologies[[j]], "modelagetype") <- L$chronData[[chronNum]]$model[[modelNum]]$methods$units
            }else{
              message("Could not find model methods. Chronology metadata fields 'agemodel' and 'modelagetype' not filled.")
            }
            if (slot(chronos1@chronologies[[j]], "chronologyid") == L$chronData[[chronNum]]$measurementTable[[chronTabNum]]$measurementTableId){
              slot(chronos1@chronologies[[j]], "isdefault") <- 1
            }else{
              slot(chronos1@chronologies[[j]], "isdefault") <- 0
            }

            #which metadata could not be placed?
            whichChronMetaNotPlaced <- names(L$chronData[[chronNum]]$measurementTable[[chronTabNum]]) %in% c(names(getSlots("chronology")), names(chron1[eval(paste0("chron",chronNum,"meas",chronTabNum))][[1]]), "measurementTableId", "linkedMeasurementTable")
            whichChronMetaNotPlacedNames <- names(L$chronData[[chronNum]]$measurementTable[[chronTabNum]])[!whichChronMetaNotPlaced]
            if(length(whichChronMetaNotPlacedNames)>0){
              message(paste0("The following chronology (chronData measurement table) metadata could not be placed: ", whichChronMetaNotPlacedNames, "\n",
                             "Consider renaming to one of the controlled neotoma terms: ", paste(chrnSlots, collapse = " "),"\n"))
            }
            slot(site1@collunits@collunits[[jj]], "chronologies") <- chronos1


            ###########################################################################
            #dataset metadata
            dataset1 <- neotoma2::set_dataset()

            possibleLipdDatasetKeys <-
              c(
                "measurementTableId",
                "neotomaDatabase",
                "neotomaDatasetType",
                "measurementTableName",
                "ageRangeYoung",
                "notes",
                "measurementTableDOI",
                "ageRangeOld",
                "specimens",
                "measurementTablePIList"
              )

            lipdDatasetKeys <- names(L$paleoData[[jj]]$measurementTable[[j]])[names(L$paleoData[[jj]]$measurementTable[[j]]) %in% possibleLipdDatasetKeys]

            for (rr in lipdDatasetKeys){
              if (rr == "measurementTableId"){
                aa = "datasetid"
              }else if (rr == "measurementTableName"){
                aa = "datasetname"
              }else if (rr == "measurementTableDOI"){
                aa = "doi"
              }else if (rr == "measurementTablePIList"){
                aa = "pi_list"
              }else if (rr == "neotomaDatabase"){
                aa = "database"
              }else if (rr == "neotomaDatasetType"){
                aa = "datasettype"
              }else if (rr == "ageRangeYoung"){
                aa = "age_range_young"
              }else if (rr == "ageRangeOld"){
                aa = "age_range_old"
              }else{
                aa=rr
              }
              slot(dataset1, eval(aa)) <- unname(L$paleoData[[jj]]$measurementTable[[j]][eval(rr)])[[1]]
            }

            #note unplaced dataset metadata
            areTheseDatasetMetadata <- names(L$paleoData[[jj]]$measurementTable[[j]])[unlist(lapply(L$paleoData[[jj]]$measurementTable[[j]], function(x) class(x) != "list"))]

            datasetMetadataNotPlaced <- areTheseDatasetMetadata[!areTheseDatasetMetadata %in% c(possibleLipdDatasetKeys,"tableName", "linkedMeasurementTable", "linkedModel", "linkedSummaryTable")]

            if (length(datasetMetadataNotPlaced)>0){
              message(paste0("The following dataset (paleoData measurement table) metadata could not be placed: ", paste(datasetMetadataNotPlaced, collapse = " "), "\n",
                             "Consider renaming to one of the controlled lipd standard for neotoma terms: ", paste(possibleLipdDatasetKeys, collapse = " "),"\n"))
            }



            #Gather dataset data
            PD1 <- L$paleoData[[jj]]$measurementTable[[j]]
            sampleTab <- paleo1[[eval(paste0("paleo", jj, "meas", j))]]
            sampleTabNames <- names(sampleTab)

            ###########################################################################
            #build a sample

            #extract sample metadata
            possibleLipdSampleKeys <-
              c(
                "age",
                "igsn",
                "neotomaSampleId",
                "thickness",
                "neotomaSampleName",
                "neotomaSampleAnalyst",
                "neotomaAnalysisUnitId",
                "neotomaAnalysisUnitName",
                "depth"
              )

            lipdSampleKeys <- names(L$paleoData[[jj]]$measurementTable[[j]])[names(L$paleoData[[jj]]$measurementTable[[j]]) %in% possibleLipdSampleKeys]

            #Note unplaced sample metadata
            doTheyHaveMetadata <- names(L$paleoData[[jj]]$measurementTable[[j]])[unlist(lapply(L$paleoData[[jj]]$measurementTable[[j]], function(x) length(names(x))==2))]
            areTheyKnownKeys <- doTheyHaveMetadata[!doTheyHaveMetadata %in% possibleLipdSampleKeys]
            if (length(areTheyKnownKeys)>0){
              message(paste0("The following sample (paleoData measurement table columns) metadata could not be placed: ", paste(areTheyKnownKeys, collapse = " "), "\n",
                             "Consider renaming to one of the controlled lipd standard for neotoma terms: ", paste(possibleLipdSampleKeys, collapse = " "),"\n"))
            }

            #Get all the sample datum column headers
            allSampleKeys <- list()
            for (i in 1:length(sampleTabNames)){
              #Handling of controlled synonyms for sample datum keys (paleoData measurement table keys in lipd)
              allColHeaders <- names(PD1[sampleTabNames[i]][[1]])
              for (dd in allColHeaders){
                if (dd == "values"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "values")] = "value"
                }else if (dd == "neotomaElement"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaElement")] = "element"
                }else if (dd == "neotomaTaxonId"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaTaxonId")] = "taxonid"
                }else if (dd == "neotomaTaxonGroup"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaTaxonGroup")] = "taxongroup"
                }else if (dd == "variableName"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "variableName")] = "variablename"
                }else if (dd == "neotomaEcologicalGroup"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaEcologicalGroup")] = "ecologicalgroup"
                }else if (dd == "neotomaSymmetry"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaSymmetry")] = "symmetry"
                }else if (dd == "neotomaContext"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "neotomaContext")] = "context"
                }else if (dd == "units"){
                  names(PD1[sampleTabNames[i]][[1]])[which(names(PD1[sampleTabNames[i]][[1]]) == "units")] = "units"
                }else {
                  message(paste0("For ", sampleTabNames[i], " This sample datum key is not valid for Neotoma: ", dd, "\nplease refer to lipd2neotoma standardization documentation for help"))
                }
              }

              allSampleKeys[[i]] <- names(PD1[sampleTabNames[i]][[1]])
            }
            sampleCols <- unique(unlist(allSampleKeys))

            #initiate list for neotoma "samples"
            allSamps <- list()

            pullAge <- which(tolower(sampleTabNames) == "age")
            pullDepth <- which(tolower(sampleTabNames) == "depth")
            pullYear <- which(tolower(sampleTabNames) == "year")

            #iterate over all ages (sample layers) to build a data frame (datum) at each layer
            for (k in 1:nrow(sampleTab)){

              pullAgeDepthYear <- c(sampleTabNames[pullAge], sampleTabNames[pullDepth], sampleTabNames[pullYear], lipdSampleKeys)
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
                    if (grepl("value", sampleCols[ii])){
                      neoSamples[i,ii] <- PD1[sampleTabNames[i]][[1]][eval(sampleCols[ii])][[1]][k]
                    }else{
                      neoSamples[i,ii] <- PD1[sampleTabNames[i]][[1]][eval(sampleCols[ii])][[1]][1]
                    }
                  }
                }
              }

              neoSamples$value[as.numeric(neoSamples$value) == 0] <- NA
              neoSamples <- neoSamples[!is.na(neoSamples$value),]


              #assign sample slots
              sample1 <- new("sample")

              #add sample metadata
              for (rr in lipdSampleKeys){
                if (rr == "age"){
                  aa = "ages"
                }else if (rr == "neotomaSampleId"){
                  aa = "sampleid"
                }else if (rr == "neotomaSampleName"){
                  aa = "samplename"
                }else if (rr == "neotomaSampleAnalyst"){
                  aa = "sampleanalyst"
                }else if (rr == "neotomaAnalysisUnitId"){
                  aa = "analysisunitid"
                }else if (rr == "neotomaAnalysisUnitName"){
                  aa = "analysisunitname"
                }else{
                  aa=rr
                }
                slot(sample1, eval(aa)) <- as.numeric(unname(L$paleoData[[jj]]$measurementTable[[j]][eval(rr)])[[1]]$values[k])
              }

              if (length(slot(sample1, eval("ages")))<1 | is.na(slot(sample1, eval("depth")))){
                summaryTab <- L2$chronData[[chronNum]]$model[[modelNum]]$summaryTable[[summaryTabNum]]
                #toss out any metadata fields
                summaryTab <- summaryTab[unlist(lapply(summaryTab, function(x) class(x)=="list"))]
                #add additonal sample metadata
                for (rr in names(summaryTab)){
                  if (rr == "age"){
                    aa = "ages"
                  }else{
                    aa=rr
                  }
                  slot(sample1, eval(aa)) <- as.numeric(summaryTab[eval(rr)][[1]]$values[k])
                }
              }



              #add datum to appropriate sample slot
              sample1@datum <- neoSamples

              #add current sample to list of all samples
              allSamps[[k]] <- sample1
            }

            #Place samples and dataset
            for (i in 1:length(allSamps)){
              dataset1@samples@samples[[i]] <- allSamps[[i]]
            }

            #place dataset in list of datasets
            site1@collunits@collunits[[jj]]@datasets@datasets[[j]] <- dataset1
            #datasetAll@datasets[[j]] <- dataset1

          }
          # else if (L$paleoData[[jj]]$measurementTable[[j]]$tableName == "lithology"){
          #
          #   ###########################################################################
          #   #Add new lithology
          #   ###########################################################################
          #
          #   lith1 <- new("lithology")
          #
          #
          #   possibleLipdLithologyKeys <-
          #     c(
          #       "neotomaLithologyid",
          #       "neotomaLowerboundary",
          #       "neotomaRecdatecreated",
          #       "neotomaRecdatemodified"
          #     )
          #
          #   lipdLithologKeys <- names(L$paleoData[[jj]]$measurementTable[[j]])[names(L$paleoData[[jj]]$measurementTable[[j]]) %in% possibleLipdLithologyKeys]
          #
          #   for (rr in lipdLithologKeys){
          #     if (rr == "neotomaLithologyid"){
          #       aa = "lithologyid"
          #     }else if (rr == "neotomaLowerboundary"){
          #       aa = "lowerboundary"
          #     }else if (rr == "neotomaRecdatecreated"){
          #       aa = "recdatecreated"
          #     }else if (rr == "neotomaRecdatemodified"){
          #       aa = "recdatemodified"
          #     }else{
          #       aa=rr
          #     }
          #     slot(lith1, eval(aa)) <- unname(L$paleoData[[jj]]$measurementTable[[j]][eval(rr)])[[1]]
          #   }
          #
          #   #note unplaced dataset metadata
          #   areTheseLithologyMetadata <- names(L$paleoData[[jj]]$measurementTable[[j]])[unlist(lapply(L$paleoData[[jj]]$measurementTable[[j]], function(x) class(x) != "list"))]
          #
          #   lithologyMetadataNotPlaced <- areTheseLithologyMetadata[!areTheseLithologyMetadata %in% c(possibleLipdLithologyKeys,"tableName")]
          #
          #   if (length(datasetMetadataNotPlaced)>0){
          #     message(paste0("The following lithology (paleoData measurement table) metadata could not be placed: ", paste(datasetMetadataNotPlaced, collapse = " "), "\n",
          #                    "Consider renaming to one of the controlled lipd standard for neotoma terms: ", paste(possibleLipdLithologyKeys, collapse = " "),"\n"))
          #   }
          #
          #
          #   lithologyColNames <- names(L$paleoData[[jj]]$measurementTable[[j]])[unlist(lapply(L$paleoData[[jj]]$measurementTable[[j]], function(x) class(x) == "list"))]
          #
          #   lithologies <- list()
          #
          #   if (length(lithologyColNames) > 0) {
          #     for (yy in 1:length(L$paleoData[[jj]]$measurementTable[[j]][lithologyColNames[1]][[1]]$values)){
          #       lithNew <- lith1
          #       for (tt in lithologyColNames){
          #         if (tt == "neotomaLithologyid"){
          #           aa = "lithologyid"
          #           slot(lithNew, eval(aa)) <- as.numeric(L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy])
          #         }else if (tt == "neotomaLowerboundary"){
          #           aa = "lowerboundary"
          #           slot(lithNew, eval(aa)) <- L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy]
          #         }else if (tt == "neotomaRecdatecreated"){
          #           aa = "recdatecreated"
          #           slot(lithNew, eval(aa)) <- L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy]
          #         }else if (tt == "neotomaRecdatemodified"){
          #           aa = "recdatemodified"
          #           slot(lithNew, eval(aa)) <- L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy]
          #         }else if (tt == "depthTop"){
          #           aa = "depthtop"
          #           slot(lithNew, eval(aa)) <- as.numeric(L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy])
          #         }else if (tt == "depthBottom"){
          #           aa = "depthbottom"
          #           slot(lithNew, eval(aa)) <- as.numeric(L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy])
          #         }else if (tt == "facies"){
          #           aa = "description"
          #           slot(lithNew, eval(aa)) <- L$paleoData[[jj]]$measurementTable[[j]][eval(tt)][[1]]$values[yy]
          #         }else{
          #           aa=tt
          #         }
          #       }
          #       lithologies[[yy]] <- lithNew
          #     }
          #   }
          # } else {
          #   stop(paste0("at paeloData ", jj, " measurementTable ", j, " 'tableName' must be one of: 'dataset', 'lithology'"))
          # }
        }
      }
      #place datasets in collunit
      #slot(site1@collunits@collunits[[jj]], "datasets") <- datasetAll
    }
  }

  #neotomaStuff <- list(site1, lithologies)

  #return(neotomaStuff)
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


  #datasetAll <- new("datasets")
  datasetAll <- list()
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
      dataset1@specimens <- new("specimens")
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



  site1@collunits@collunits[[1]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = chronos1, colldate = as.Date(character(1)))

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


