#' Convert lipd to neotoma
#'
#' @param L lipd object
#' @importFrom methods new
#' @importFrom sf st_as_sf st_sfc st_point
#'
#' @return neotoma site
#'
L=L1


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
  for (j in 1:sum(grep("paleo", attributes(mtabs1)$names))){
    paleoTabName <- attributes(mtabs1)$names[paleoTabIndex]
    paleoTabIndex <- grep("paleo", attributes(mtabs1)$names)[j]
    PD1 <- mtabs1[[paleoTabIndex]]

    #initiate list for neotoma "samples"
    allSamps <- list()

    #iterate over all ages (sample layers)
    for (k in 1:nrow(PD1)){
      #cat(k, "\n")

      sampleTab <- PD1[k,!is.na(PD1[k,])]

      sampleTabNames <- names(sampleTab)

      pullAge <- which(tolower(sampleTabNames) %in% "age")
      if(length(pullAge)<1){
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

      pullAgeDepth <- c(sampleTabNames[var1], sampleTabNames[var2])
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
      sample1@depth <- sampleTab[pullDepth]
      sample1@ages <- ages1

      allSamps[[k]] <- sample1


    }



    dataset1 <- neotoma2::set_dataset(datasetid = strsplit(L$originalDataUrl, "/")[[1]][length(strsplit(L$originalDataUrl, "/")[[1]])])

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



  site1@collunits@collunits[[1]] <- neotoma2::set_collunit(datasets = datasetAll, chronologies = chronos1, colldate = as.Date(character(0)))

  site1$siteid <- L$geo$neotomaSiteId
  site1$sitename <- L$geo$siteName
  # site1$lat <- lipdR:::getGeoNeotoma2(D@sites[[1]])$latitude
  # site1$long <- lipdR:::getGeoNeotoma2(D@sites[[1]])$longitude
  site1$altitude <- L$geo$elevation

  site1@geography = st_as_sf(st_sfc(st_point(c(L$geo$longitude,L$geo$latitude))))

  site1@description <- L$geo$description

  return(site1)

}

