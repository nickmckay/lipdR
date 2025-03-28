#' Collapse time series into LiPD dataset form
#' @export
#' @author Chris Heiser
#'
#' @param ts Time series : list
#' @param force Attempt to collapse time series when lipd ts_storage is not provided: bool
#' @param verbose Print details of collapse process
#'
#' @return D: LiPD data, sorted by dataset name : list
#' @examples
#' \dontrun{
#' D <- readLipd()
#' ts <- extractTs(D)
#' D <- collapseTs(ts)
#' }
#'
collapseTs <- function(ts, force=FALSE, verbose = NA){
  #before doing anything else, reorder the data by dataset name.
  dsn <- pullTsVariable(ts,"dataSetName",strict.search = TRUE)
  if(all(is.na(verbose))){
    if(length(unique(dsn)) > 20){
      verbose <- FALSE
    }else{
      verbose <- TRUE
    }
  }

  ts <- ts[order(dsn)]
  ts_storage <- list()
  timeID <- NA
  whichtables <- ts[[1]]$whichtables

  # Get the original data from the lipd environment whenever possible
  if(force == "if necessary"){
    if(exists("TMP_ts_storage", envir = lipdEnv)){
      force <- FALSE
    }else{
      force <- TRUE
    }
  }


  # Use the time_id to get the corresponding raw data from lipd envir ts storage
  if(!force){
    timeID <- ts[[1]]$timeID
    ts_storage <- get_ts_lipd()
    raw_datasets <- ts_storage[[timeID]]
    mode <- ts[[1]][["mode"]]
  }
if(!verbose){
  pb <- txtProgressBar(min = 0, max = length(ts),title = "Collapsing to LiPD objects",style = 3)
}
  D <- list()
  tryCatch({
    # Do some collapse stuff
    for(i in 1:length(ts)){
      if(!verbose){
        setTxtProgressBar(pb = pb,value = i)
      }
      ts[[i]] = add_missing_ts_data(ts[[i]])
      pc <- paste0(ts[[i]][["mode"]], "Data")
      # ONLY PROCESS BASE DATA ON FIRST DATASET OCCURENCE. All subsequent timeseries entries from the same dataset will only add its unique column data to the running dataset.
      if(!ts[[i]][["dataSetName"]] %in% names(D)){
        dsn <- ts[[i]][["dataSetName"]]
        if(verbose){
          print(paste0("collapsing: ", dsn))
        }
        # Recover paleoData OR chronData from raw data. Recovers only the section opposite of the current mode.
        D[[dsn]] <- put_base_data(ts[[i]], raw_datasets, dsn, force, mode, verbose)
        if(!force){
          # Remove the old target tables, as we'll be writing these fresh. Other tables as-is.
          D[[dsn]] <- rm_existing_tables(D[[dsn]], pc, whichtables)
        }
        # Collapse root data keys (pub, funding, archiveType, etc)
        D[[dsn]] <- collapse_root(D[[dsn]], ts[[i]], pc)
      }
      # Use the time series entry to overwrite the (old) raw data for this column
      D[[dsn]] <- collapse_table(D[[dsn]], ts[[i]], pc)

      D[[dsn]] <- structure(D[[dsn]],class = c("lipd",class(list())))
    }
  }, error=function(cond){
    print(paste0("Error: collapseTs: ", cond))
  })
  if (exists(quote(pb))){
    close(pb)
  }
  D <- rm_empty_fields(D)
  # Is there only one dataset after all this? Set it directly in D.
  if(length(D)==1){
    D <- D[[1]]
    D <- structure(D,class = c("lipd",class(list())))

  }else{
    for(dd in 1:length(D)){
      D[[dd]] <- structure(D[[dd]],class = c("lipd",class(list())))
    }
    D <- structure(D,class = c("multi_lipd",class(list())))
  }
  return(D)
}

is_include_key <- function(key, pc){
  exclude <- c("mode", "whichtables", "paleoNumber", "chronNumber", "tableNumber", "modelNumber", "timeID", "tableType",
               "raw", "depth", "depthUnits", "age", "ageUnits", "interpretation", "calibration", "hasResolution","inCompilationBeta", "physicalSample",
               "depthUnits","year","yearUnits")
  match_idx <- stringr::str_match_all(key, "(\\w+)(\\d+)[_](\\w+)")
  match_non_idx <- stringr::str_match_all(key, "(\\w+)[_](\\w+)")

  # Check for exact match
  if(key %in% exclude){
    return(FALSE)
  }

  # Split any keys that have underscores. (i.e. "interpretation1_scope").
  # Is the prefix (interpretation) in exclude?
  if(!isNullOb(match_idx[[1]])){
    if(match_idx[[1]][[2]] %in% exclude){
      return(FALSE)
    }
  }
  # Split any keys that have underscores without an index. (i.e. "hasResolution_hasMax").
  # Is the prefix (hasResolution) in exclude?
  if(!isNullOb(match_non_idx[[1]])){
    if(match_non_idx[[1]][[2]] %in% exclude){
      return(FALSE)
    }
  }

  # Catch any stragglers. Is the key an exact match or is the key a data table key? (i.e. "paleoData_<key> " or "chronData_<key> ")
  for(i in 1:length(exclude)){
    if(key == exclude[[i]] || grepl(pc, key)){
      return(FALSE)
    }
  }

  # If you made it past all the exclusions, congrats! You're a valid key.
  return(TRUE)
}

collapse_root <- function(d, entry, pc){

  ts_keys <- names(entry)
  pub <- list()
  funding <- list()
  # geo <- list(geometry=list(coordinates=list(NA, NA, NA), type="Point"), properties=list())
  geo <- list()
  for(i in 1:length(ts_keys)){
    key <- ts_keys[[i]]
    # Is this a key that should be added
    include <- is_include_key(key, pc)
    # Filter all the keys we don't want
    if(include){
      if(grepl("geo", key)){
        m <- stringr::str_match_all(key, "(\\w+)[_](\\w+)")
        g_key = m[[1]][[3]]
        geo[[g_key]] <- entry[[key]]
        #
        # if(g_key == "longitude" || g_key == "meanLon"){
        #   geo[["geometry"]][["coordinates"]][[1]] <- entry[[key]]
        # } else if (g_key == "latitude" || g_key == "meanLat"){
        #   geo[["geometry"]][["coordinates"]][[2]] <- entry[[key]]
        # } else if (g_key == "elevation" || g_key == "meanElev"){
        #   geo[["geometry"]][["coordinates"]][[3]] <- entry[[key]]
        # } else if (g_key == "type"){
        #   geo[["type"]] <- entry[[key]]
        # } else {
        #   geo[["properties"]][[g_key]] <- entry[[key]]
        # }
      } else if(grepl("pub", key)){
        pub <- collapse_block_indexed(entry, pub, key)
      } else if(grepl("funding", key)){
        funding <- collapse_block_indexed(entry, funding, key)
      } else{
        # Root key that is not a special case; move it right on over
        d[[key]] <- entry[[key]]
      }
    }
  }
  # Only add these lists if they're not empty
  if(!isNullOb(pub)){ d[["pub"]] <- pub }
  if(!isNullOb(funding)){ d[["funding"]] <- funding }
  if(!isNullOb(geo)){ d[["geo"]] <- geo }
  d[["@context"]] <- "context.jsonld"

  return(d)
}

collapse_author <- function(d, entry){
  return(d)
}

#' Collapse time series section; paleo or chron
#' @export
#' @param d Metadata
#' @param entry Time series entry
#' @param pc paleoData or chronData
#' @return d: Metadata
collapse_table <- function(d, entry, pc){
  # Get the crumbs to the target table
  # m <- get_crumbs(entry)
  # Get the existing target table
  # table <- get_table(d, m, pc)
  table <- get_table(d, entry, pc)
  # table <- list()
  table <- collapse_table_root(table, entry, pc)
  table <- collapse_column(table, entry, pc)
  # Put the new modified table back into the metadata
  # d <- put_table(d, m, pc, table)
  d <- put_table(d, entry, pc, table)
  return(d)
}

#' Collapse time series table root. All keys listed below are known table root keys.
#' @export
#' @param table Metadata
#' @param entry Time series entry
#' @param pc paleoData or chronData
#' @return table: Metadata
collapse_table_root <- function(table, entry, pc){
  root_keys <- c('filename', 'googleWorkSheetKey', 'tableName', "missingValue", "tableMD5", "dataMD5", "googWorkSheetKey")
  for(i in 1:length(root_keys)){
    key <- paste0(pc, "_", root_keys[[i]])
    if(key %in% names(entry)){
      table[[root_keys[[i]]]] <- entry[[key]]
    }
  }
  return(table)
}

#' Collapse time series column. Compile column entries and place new column in table.
#' @export
#' @param table Metadata
#' @param entry Time series entry
#' @param pc paleoData or chronData
#' @return table: Metadata
collapse_column <- function(table, entry, pc){
  new_column <- list()
  interp <- list()
  calib <- list()
  res <- list()
  phys <- list()
  inComp <- list()
  include <- c("paleoData", "chronData", "interpretation", "calibration", "hasResolution","inCompilationBeta")
  exclude <- c('filename', 'googleWorkSheetKey', 'tableName', "missingValue", "tableMD5", "dataMD5", "googWorkSheetKey", "pub", "geo")
  ts_keys <- names(entry)

  tryCatch({
    for(i in 1:length(ts_keys)){
      curr_key <- ts_keys[[i]]
      # Interpretation is indexed, so it needs special attention an processing.
      if (grepl("interpretation", curr_key)){
        interp <- collapse_block_indexed(entry, interp, curr_key)
      } else if (grepl("calibration", curr_key)){
        calib <- collapse_block(entry, calib, curr_key, pc)
      } else if (grepl("hasResolution", curr_key)){
        res <- collapse_block(entry, res, curr_key, pc)
      } else if (grepl("physicalSample", curr_key)){
        phys <- collapse_block(entry, phys, curr_key, pc)
      } else if (grepl("inCompilationBeta", curr_key)){
        inComp <- collapse_block_indexed(entry, inComp, curr_key)
      } else if (grepl(pc, curr_key)){
        new_column <- collapse_block(entry, new_column, curr_key, pc)
      }
    }
    # Add the special sub-lists if data was found
    if(!isNullOb(interp)){
      new_column[["interpretation"]] <- interp
    }
    if(!isNullOb(calib)){
      new_column[["calibration"]] <- calib
    }
    if(!isNullOb(res)){
      new_column[["hasResolution"]] <- res
    }
    if(!isNullOb(phys)){
      new_column[["physicalSample"]] <- phys
    }
    if(!isNullOb(inComp)){
      new_column[["inCompilationBeta"]] <- inComp
    }
    vn <- get_vn(new_column[["variableName"]], names(table))
    # Set the new column into the table using the variableName
    table[[vn]] <- list(100)
    table[[vn]] <- new_column
  }, error=function(cond){
    print(paste0("Error: collapse_column: ", cond))
    return(table)
  })
  return(table)
}

# Use regex to split the tableName into a crumbs path
# get_crumbs <- function(ts){
#   matches <- c()
#   tryCatch({
#     mode <- ts[["mode"]]
#     key <- paste0(mode, "Data_tableName")
#     m <- stringr::str_match_all(ts[[key]], "(\\w+)(\\d+)(\\w+)(\\d+)")
#     if(isNullOb(m[[1]])){
#       m <- stringr::str_match_all(ts[[key]], "(\\w+)(\\d+)(\\w+)(\\d+)(\\w+)(\\d+)")
#       matches <- c(m[[1]][[2]], m[[1]][[3]], m[[1]][[4]], m[[1]][[5]], m[[1]][[6]], m[[1]][[7]])
#     } else {
#       matches <- c(m[[1]][[2]], m[[1]][[3]], m[[1]][[4]], m[[1]][[5]])
#     }
#   }, error=function(cond){
#     stop(paste0("Error get_crumbs: ", cond))
#   })
#   return(matches)
# }


#' Collapses blocks: funding, publication calibration, interpretation
#' @description These follow the regex format of "<key1><idx>_<key2>"
#' @description match[[1]][[1]] = full key with underscore and index (ex. "interpretation1_variableDetail")
#' @description match[[1]][[2]] = first key (ex. "interpretation)
#' @description match[[1]][[3]] = index number (ex. the "1" from "interpretation1")
#' @description match[[1]][[4]] = second key (ex. "variableDetail")
#' @export
#' @param entry Time series entry
#' @param l Metadata (to add new data to)
#' @param key Current key from time series entry
#' @return l Metadata
collapse_block_indexed <- function(entry, l, key){
  # print(paste0("collapsing block: ", key))
  match <- stringr::str_match_all(key,"([A-Za-z]+)(\\d{1,})[_]([A-Za-z]+)")
  if(!isNullOb(match[[1]])){
    currIdx <- as.numeric(match[[1]][[3]])
    place_key <- match[[1]][[4]]
    # If there isn't a list initialized yet for this index, then make it
    if(length(l) < currIdx){
      for(idx in 1:currIdx){
        if(length(l) < currIdx){
          l[[currIdx]] <- list()
        } else if(isNullOb(l[[currIdx]])){
          l[[currIdx]] <- list()
        }
      }
    }
    l[[currIdx]][[match[[1]][[4]]]] <- entry[[key]]
  }
  return(l)
}

#' Collapses blocks: calibration, physicalSample, hasResolution,
#' These follow the regex format of "<key1>_<key2>"
#' match[[1]][[1]] = full key with underscore and index (ex. "physicalSample_tableName")
#' match[[1]][[2]] = first key (ex. "physicalSample")
#' match[[1]][[3]] = second key (ex. "tableName")
#'
#' @export
#'
#' @param entry Time series entry
#' @param l Metadata (to append to)
#' @param key Key from time series entry
#' @param pc paleo or chron? #deprecated
#'
#' @return l: Metadata
collapse_block <- function(entry, l, key, pc){
  exclude <- c('filename', 'googleWorkSheetKey', 'tableName', "missingValue", "tableMD5", "dataMD5", "googWorkSheetKey", "geo", "funding", "pub")
  # key_match / 1,1 "paleoData" / 1,2 "_" / 1,3 "someKey"
  key_match <- stringr::str_match_all(key, "(\\w+)[_](\\w+)")
  if(!isNullOb(key_match[[1]])){
    # Not a root table key, and pc matches the given mode
    if(!(key_match[[1]][[3]] %in% exclude)){
      # set ts entry value to new entry in column
      l[[key_match[[1]][[3]]]] <- entry[[key]]
    }
  }
  return(l)
}

#' Get the target table
#' @export
#' @param d Metadata
#' @param current Current time series entry
#' @param pc paleoData or chronData
#' @return table: Metadata
get_table <- function(d, current, pc){
  table <- list()
  # Use the path and indexing info to get the "in progress" table that belongs to this TS entry. We want to add this TS entry (column) to this table.
  tt <- current$tableType
  modelNumber <- current$modelNumber
  tableNumber <- current$tableNumber

  # Get the pcNumber. Dependent on mode.
  if(pc == "paleoData"){
    pcNumber <- current$paleoNumber
  } else {
    pcNumber <- current$chronNumber
  }

  #Check if the number was missing
  if(is.null(pcNumber) | is.na(pcNumber)){
    pcNumber <- 1#and assume 1 if so
  }

  # Measurement table
  if(tt == "meas"){
    tryCatch({
      table <- d[[pc]][[pcNumber]][["measurementTable"]][[tableNumber]]
    },error=function(cond){
      return(list())
    })
  }
  # Model tables
  else if (tt == "summ") {
    tryCatch({
      table <- d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]][[tableNumber]]
    },error=function(cond){
      return(list())
    })
  }
  else if (tt=="ens"){
    tryCatch({
      table <- d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]][[tableNumber]]
    },error=function(cond){
      return(list())
    })
  }
  if(is.null(table)){
    table <- list()
  }

  return(table)
}

#' Put the target table
#' @export
#' @param d Metadata
#' @param current Current time series entry
#' @param pc paleoData or chronData
#' @param table Metadata (to be placed)
#' @return d: Metadata
put_table <- function(d, current, pc, table){
  tt <- current$tableType
  modelNumber <- current$modelNumber
  tableNumber <- current$tableNumber

  # Get the pcNumber. Dependent on mode.
  if(pc == "paleoData"){
    pcNumber <- current$paleoNumber
  } else {
    pcNumber <- current$chronNumber
  }
  d <- build_structure(d, pc, tt, pcNumber, modelNumber, tableNumber)

  # Measurement tables
  if(tt == "meas"){
    # Best Case Scenario: The structure for placing this table is already existing, and we can place the table directly into that location.
    d[[pc]][[pcNumber]][["measurementTable"]][[tableNumber]] <- table
  }

  # Summary tables
  else if (tt == "summ") {
    # print(paste0("Inserting table: ", pc, pcNumber, "model", modelNumber, "summaryTable", tableNumber))
    # Best Case Scenario: The structure for placing this table is already existing, and we can place the table directly into that location.
    d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]][[tableNumber]] <- table
  }

  # Ensemble tables
  else if (tt=="ens"){
    # Best Case Scenario: The structure for placing this table is already existing, and we can place the table directly into that location.
    d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]][[tableNumber]] <- table
  }

  return(d)
}


#' Before you place a table, you must have the structure leading up to the location or you'll get errors. Build the structure
#' @export
#' @param d Metadata
#' @param pc paleoData or chronData
#' @param table_type meas, ens, or summ
#' @param pcNumber paleoData or chronData index number
#' @param modelNumber Model index number OR null (if meas table)
#' @param tableNumber Table index number
#' @return D: Metadata
build_structure <- function(d, pc, table_type, pcNumber, modelNumber, tableNumber){

  if(is.null(d)){
    d <-list()
  }

  if(!pc %in% names(d)){
    d[[pc]] <- list()
  }

  # Is there a list for this pc section?
  if(!is.list(d[[pc]])){
    # No, create one.
    d[[pc]] <- list()
  }
  # Is there a list at pcNumber index?
  if(length(d[[pc]]) < pcNumber){
    # No, create one.
    d[[pc]][[pcNumber]] <- list()
  }

  # Create Measurement structure
  if (table_type == "meas"){
    if(!"measurementTable" %in% names(d[[pc]][[pcNumber]])){
      d[[pc]][[pcNumber]][["measurementTable"]] <- list()
    }
    if(!is.list(d[[pc]][[pcNumber]][["measurementTable"]])){
      d[[pc]][[pcNumber]][["measurementTable"]] <- list()
    }
    if(length(d[[pc]][[pcNumber]][["measurementTable"]]) < tableNumber){
      d[[pc]][[pcNumber]][["measurementTable"]][[tableNumber]] <- list()
    }
  }

  if (table_type == "ens" || table_type == "summ"){

    #See if model exists, and is needed.
    if(!is.null(modelNumber)){

      # Create the Model indexing if needed
      if(!"model" %in% names(d[[pc]][[pcNumber]])){
        d[[pc]][[pcNumber]][["model"]] <- list()
      }
      if(!is.list(d[[pc]][[pcNumber]][["model"]])){
        d[[pc]][[pcNumber]][["model"]] <- list()
      }
      if(length(d[[pc]][[pcNumber]][["model"]]) < modelNumber){
        d[[pc]][[pcNumber]][["model"]][[modelNumber]] <- list()
      }

      # Create summary structure
      if (table_type == "summ"){
        # Create the Model indexing if needed
        if(!"summaryTable" %in% names(d[[pc]][[pcNumber]][["model"]][[modelNumber]])){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]] <- list()
        }
        if(!is.list(d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]])){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]] <- list()
        }
        if(length(d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]]) < tableNumber){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]][["summaryTable"]][[tableNumber]] <- list()
        }
      }

      # Create ensemble structure
      if (table_type == "ens"){
        # Create the Model indexing if needed
        if(!"ensembleTable" %in% names(d[[pc]][[pcNumber]][["model"]][[modelNumber]])){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]] <- list()
        }
        if(!is.list(d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]])){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]] <- list()
        }
        if(length(d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]]) < tableNumber){
          d[[pc]][[pcNumber]][["model"]][[modelNumber]][["ensembleTable"]][[tableNumber]] <- list()
        }
      }
    }
  }
  return(d)
}


#' Remove tables that correspond to 'whichtables' type. These tables will be collapsed next and need a clean slate.
#' @export
#' @param d Metadata
#' @param pc paleoData or chronData
#' @param whichtables all summ meas ens
#' @return d: Metadata
rm_existing_tables <- function(d, pc, whichtables){
  tryCatch({
    if(pc %in% names(d)){
      for(i in 1:length(d[[pc]])){
        if(whichtables %in% c("meas","all")){
          if("measurementTable" %in% names(d[[pc]][[i]])){
            for(j in 1:length(d[[pc]][[i]][["measurementTable"]])){
              d[[pc]][[i]][["measurementTable"]][[j]] <- list()
            }
          }
        }

        if(whichtables %in% c("ens", "summ","all")){
          if("model" %in% names(d[[pc]][[i]])){
            for(j in 1:length(d[[pc]][[i]][["model"]])){
              if(whichtables %in% c("summ")){
                if("summaryTable" %in% d[[pc]][[i]][["model"]][[j]]){
                  for(k in 1:length(d[[pc]][[i]][["model"]][[j]][["summaryTable"]])){
                    d[[pc]][[i]][["model"]][[j]][["summaryTable"]][[k]] <- list()
                  }
                }
              }
              if (whichtables %in% c("ens","all")){
                if("ensembleTable" %in% names(d[[pc]][[i]][["model"]][[j]])){
                  for(k in 1:length(d[[pc]][[i]][["model"]][[j]][["ensembleTable"]])){
                    d[[pc]][[i]][["model"]][[j]][["ensembleTable"]][[k]] <- list()
                  }
                }
              }
            }
          }
        }
      }
    }
    return(d)
  }, error=function(cond){
    stop(paste0("rm_existing_tables: ", cond))
    return(d)
  })
  return(d)
}

#' Put in paleoData and chronData as the base for this dataset using the oroginal dataset data.
#' @export
#'
#' @param force Build dataset without original data from lipd
#' @param entry ts entry
#' @param raw_datasets stored loaded data
#' @param dsn datasetname
#' @param mode paleo or chron mode
#' @param verbose detailed console output
#'
#' @return d: Metadata
put_base_data <- function(entry, raw_datasets, dsn, force, mode, verbose){
  d <- list()

  # We do not have the original datasets OR the user is requesting a collapseTs without using the original datasets
  if(force==TRUE || is.null(raw_datasets)){
    if(verbose){
    print("Attempting to collapse time series without the original raw datasets. Your results may be missing data.")
    }
    d[["paleoData"]] <- list()
    d[["chronData"]] <- list()
    d$dataSetName <- entry$dataSetName
  }

  # We have the original dataset(s). Use this data as a baseline to build and overwrite onto.
  # Only copy over the data OPPOSITE to the mode.
  # Example, for paleo mode we will rebuild the paleoData section and copy over the chronData section.
  else {

    if(!any(entry$dataSetName %in% names(raw_datasets))){#then it's either new, or the name changed
      #can we find the TSid?
      tmp <- extractTs(raw_datasets)
      all_tsid <- sapply(tmp,"[[","paleoData_TSid")
      all_dsn <- sapply(tmp,"[[","dataSetName")
      tsind <- which(entry$paleoData_TSid == all_tsid)
      if(length(tsind) == 1){
        L <- raw_datasets[[all_dsn[tsind]]]
      }else if(length(tsind) > 1){
        #see if it's one dataset
        datset <- unique(all_dsn[tsind])
        if(length(datset) > 1){
          print("Couldn't find an appropriate seed dataset. Attempting to collapse time series without the original raw datasets. Your results may be missing data.")
          d[["paleoData"]] <- list()
          d[["chronData"]] <- list()
          d$dataSetName <- entry$dataSetName
          return(d)
        }else{
          L <- raw_datasets[[datset]]
        }
      }else{
        print("Couldn't find an appropriate seed dataset. Attempting to collapse time series without the original raw datasets. Your results may be missing data.")
        d[["paleoData"]] <- list()
        d[["chronData"]] <- list()
        d$dataSetName <- entry$dataSetName
        return(d)
      }
    }else{
      L <- raw_datasets[[entry$dataSetName]]
    }
    # Is there paleoData? Find it and add it
    if("paleoData" %in% names(L)){
      d[["paleoData"]] <- L[["paleoData"]]
    }

    # Is there chronData? Find it and add it
    if("chronData" %in% names(L)){
      d[["chronData"]] <- L[["chronData"]]
    }

    # print(names(raw_datasets))
    # # Set the metadata to a variable
    # raw <- list()
    # table_type <- entry$whichtables
    # # Check if this dataset is n  ested or not.
    # if("paleoData" %in% names(raw_datasets)){
    #   L <- raw_datasets
    # } else if (dsn %in% names(raw_datasets)){
    #   L <- raw_datasets[[dsn]]
    # }
    # # Chron Mode: Get paleoData (all) and chronData (anything besides table_type)
    # if(mode == "chron"){
    #   # Is there paleoData? Find it and add it
    #   if("paleoData" %in% names(L)){
    #     print("Including paleoData")
    #     d[["paleoData"]] <- L[["paleoData"]]
    #   }
    #
    # }
    # # Paleo Mode: Get chronData (all) and paleoData (anything besides the table_type)
    # else if (mode == "paleo"){
    #   # Is there chronData? Find it and add it
    #   if("chronData" %in% names(L)){
    #     print("Including chronData")
    #     d[["chronData"]] <- L[["chronData"]]
    #   }
    #
    # }

  }

  return(d)
}

#' Retreieve the original datasets from the lipd R environment
#' @export
#' @return list tmp_storage: Temporary storage data that holds original datasets
get_ts_lipd <- function(){
  if(exists("TMP_ts_storage", envir = lipdEnv)){
    tmp_storage <- get("TMP_ts_storage", envir=lipdEnv)
  } else {
    stop("Error: Cannot collapse time series. 'TMP_ts_storage' not found in the lipd Environment. This data is created during 'extractTs' and is required for 'collapseTs'")
  }
  return(tmp_storage)
}

add_missing_ts_data <- function(entry){
  # Items needed to collapse:
  # mode, whichtables, paleoNumber* ,chronNumber*, tableNumber, modelNumber*, timeID*, tableType
  # *applicable depending on the mode and table type.
  # If any of these items are missing, add keys with assumed values.
  if(!("mode" %in% names(entry))){
    entry$mode = "paleo"
  }
  if(!("whichtables" %in% names(entry))){
    entry$whichtables = "all"
  }
  if(!("tableType" %in% names(entry))){
    entry$tableType = "meas"
  }
  if(!("tableNumber" %in% names(entry))){
    entry$tableNumber = 1
  }
  if(!("modelNumber" %in% names(entry)) && (entry$whichtables == "ens" || entry$whichtables == "summ")){
    entry$modelNumber = 1
  }
  if(!("mode" %in% names(entry))){
    entry$mode = "paleo"
  }
  if(!("paleoNumber" %in% names(entry)) && !("chronNumber" %in% names(entry))){
    entry$paleoNumber = 1
  }
  return(entry)
}
