###############################################
## Versions
## Updates the incoming LiPD to the most recent
## LiPD standards
###############################################


#' Add createdBy key to metdata
#' @export
#' @keywords internal
#' @param d Metadata
#' @return list d: Metadata
add_created_by <- function(d){
  if(!("createdBy" %in% names(d))){
    d[["createdBy"]] <- "unknown"
  }
  return(d)
}

#' Switch DOI from BibJSON structure 'identifier' key to a root level "doi" key
#' @keywords internal
#' @param d Metadata
#' @return list d: Metadata
fix_doi <- function(d){
  # DOI keys that we do not want. We want 'doi'
  dois = list("DOI", "Doi")
  # Loop for each publication
  for(p in 1:length(d$pub)){
    # Get the DOI from the BibJSON structure, if it exists
    identifier = d$pub[[p]]$identifier[[1]]$id
    # If there wasn't a DOI in that location, 'identifier' will be NULL
    if(!is.null(identifier)){
      # Found DOI identifier. Reassign and then remove from 'identifier' key
      d$pub[[p]]$doi <- identifier
      d$pub[[p]]$identifier <- NULL
    }
    # Loop for each DOI key we don't want
    for(n in 1:length(dois)){
      # Is this DOI key in the publication?
      if(dois[[n]] %in% names(d$pub[[p]])){
        # Reassign the DOI/Doi to doi key
        d$pub[[p]]$doi <- d$pub[[p]][[dois[[n]]]]
        # Remove the DOI/Doi key
        d$pub[[p]][[dois[[n]]]] <- NULL
      }
    }
  }
  # Return the metadata with the modified doi data
  return(d)
}


#' Check what version of LiPD this file is using. If none is found, assume it's using version 1.0
#' @keywords internal
#' @param d Metadata
#' @return list tmp: Version number and meta
get_lipd_version <- function(d){
  version <- NULL
  keys <- c("lipdVersion", "liPDVersion", "LiPDVersion")
  for (i in 1:length(keys)){
    key <- keys[[i]]
    if (key %in% names(d)){
      version <- d[[key]]
      d[[key]] <- NULL
    }
  }
  version <- as.numeric(version)
  if (isNullOb(version) || is.na(version)){
    # make a prompt that asks if they know what lipd version this file is.
    yn <- readline("I didn't find a LiPD version number for this file. Do you know the version number? (y/n)")
    if(yn == "y"){
      vn <- readline("Enter the version number (1.0, 1.1, 1.2, 1.3): ")
      if(vn %in% c(1, 1.0, 1.1, 1.2, 1.3)){
        version <- vn
      } else {
        stop("That's not a valid response. Please try writeLipd again.")
      }
    } else if(yn == "n"){
      print("I'll assume this is a current version (v1.3) file, but that may be an incorrect assumption. Please keep a backup!")
      version <- 1.3
    }
  }
  else if (!(version %in% c(1, 1.0, 1.1, 1.2, 1.3))){
    print(sprintf("LiPD version is invalid: %s", version))
  }
  tmp <- list()
  d[["lipdVersion"]] <- version
  tmp[["meta"]] <- d
  tmp[["version"]] <- version
  return(tmp)
}


#' Use the current version number to determine where to start updating from. Use "chain versioning" to make it
#' modular. If a file is a few versions behind, convert to EACH version until reaching current. If a file is one
#' version behind, it will only convert once to the newest.
#' @keywords internal
#' @param d Metadata
#' @return d Metadata
update_lipd_version <- function(d){
  tryCatch({
    # Get the lipd version number, and updated meta
    tmp <- get_lipd_version(d)
    version <- tmp[["version"]]
    d <- tmp[["meta"]]

    # d <- fix_authors(d[["pub"]])
    d <- fix_doi(d)

    # Legacy cleanup: rename any "inCompilationBeta" metadata to "inCompilation".
    # Runs for every file regardless of lipdVersion so old files are cleaned on read.
    d <- migrate_incompilation_keys(d)

    # Update from (N/A or 1.0) to 1.1
    if (version == 1.0 || version == "1.0"){
      d = update_lipd_v1_1(d)
      version <- 1.1
      # print("updating to 1.1")
    }

    # Update from 1.1 to 1.2
    if (version == 1.1 || version == "1.1"){
      d = update_lipd_v1_2(d)
      version = 1.2
      # print("updating to 1.2")
    }

    # Update from 1.2 to 1.3
    if(version == 1.2 || version == "1.2"){
      d = update_lipd_v1_3(d)
      version = 1.3
      # print("updating to 1.3")
    }
  }, error=function(cond){
    print(paste0("Error: update_lipd_version: v", version, ": ", cond))
  })
  return(d)
}


#' Update LiPD v1.0 to v1.1
#' - chronData entry is a list that allows multiple tables
#' - paleoData entry is a list that allows multiple tables
#' - chronData now allows measurement, model, summary, ensemble, calibratedAges tables
#' - Added 'lipdVersion' key
#' @keywords internal
#' @param d Metadata
#' @return d Metadata
update_lipd_v1_1 <- function(d){
  # Cannot v1.0 files, and I'm pretty sure there are less than 5 files in existence.
  return(d)
}

#' Update LiPD v1.1 to v1.2
#' - Added NOAA compatible keys : maxYear, minYear, originalDataURL, WDCPaleoURL, etc
#' - 'calibratedAges' key is now 'distribution' (handled in update_lipd_v1_3_keys instead)
#' - paleoData structure mirrors chronData. Allows measurement, model, summary, ensemble,
#'   distribution tables
#' @keywords internal
#' @param d Metadata
#' @return list d: Metadata
update_lipd_v1_2 <- function(d){
  if("paleoData" %in% names(d)){
    d <- update_lipd_v1_2_section(d, "paleoData")
  }
  if("chronData" %in% names(d)){
    d <- update_lipd_v1_2_section(d, "chronData")
  }
  d[["lipdVersion"]] <- 1.2
  return(d)
}

#' Update LiPD v1.1 to v1.2 - one section
#' @export
#' @keywords internal
#' @param d Metadata
#' @param pc paleoData or chronData
#' @return list d: Metadata
update_lipd_v1_2_section <- function(d, pc){
  if(!isNullOb(d[[pc]])){
    # Create the structure that we want.
    tmp <- list()
    tmp[["measurementTable"]] <- list()
    # Append the existing tables as mesurement tables in the new structure
    for(i in 1:length(d[[pc]])){
      tmp[["measurementTable"]][[i]] <- d[[pc]][[i]]
    }
    # Overwrite the old structure with the new structure
    d[[pc]] <- list()
    d[[pc]][[1]] <- tmp
  }
  return(d)
}

#' Update LiPD v1.2 to v1.3
#' - Added 'createdBy' key
#' - Top-level folder inside LiPD archives are named "bag". (No longer <datasetname>)
#' - .jsonld file is now generically named 'metadata.jsonld' (No longer <datasetname>.lpd )
#' - All "paleo" and "chron" prefixes are removed from "paleoMeasurementTable", "paleoModel", etc.
#' - Merge isotopeInterpretation and climateInterpretation into "interpretation" block
#' - ensemble table entry is a list that allows multiple tables
#' - summary table entry is a list that allows multiple tables
#' @keywords internal
#' @param d Metadata
#' @return d Metadata
update_lipd_v1_3 <- function(d){
  tryCatch({
    d <- update_lipd_v1_3_keys(d)
    d <- update_lipd_v1_3_structure(d)
    d <- add_created_by(d)
  }, error=function(cond){
    print(paste0("Error: update_lipd_v1_3: ", cond))
  })
  d[["lipdVersion"]] <- 1.3
  return(d)
}

#' Rename legacy "inCompilationBeta" metadata keys to "inCompilation": recursive
#'
#' The compilation-membership block on measurement-table columns was historically
#' named `inCompilationBeta`. It is now `inCompilation`. This walker recursively
#' descends the metadata list and renames any key beginning with
#' `inCompilationBeta` to the corresponding `inCompilation` key, so legacy files
#' are cleaned as they are read.
#' @keywords internal
#' @param d Metadata
#' @return d Metadata with any inCompilationBeta keys renamed to inCompilation
migrate_incompilation_keys <- function(d){
  keys <- names(d)

  # For lists indexed by name
  if(!isNullOb(keys) && sum(!is.na(keys)) > 0){
    for(i in 1:length(keys)){
      old_key <- keys[[i]]
      # Dive down first
      if(typeof(d[[old_key]]) == "list"){
        d[[old_key]] <- migrate_incompilation_keys(d[[old_key]])
      }
      # When bubbling back up, rename the key if it is a legacy beta key
      if(grepl("^inCompilationBeta", old_key)){
        new_key <- sub("^inCompilationBeta", "inCompilation", old_key)
        d[[new_key]] <- d[[old_key]]
        d[[old_key]] <- NULL
      }
    }
  } else {
    # For lists indexed by number
    if(typeof(d) == "list"){
      for(i in seq_along(d)){
        tryCatch({
          d[[i]] <- migrate_incompilation_keys(d[[i]])
        }, error=function(cond){
          print(paste0("Error: migrate_incompilation_keys: ", cond))
        })
      }
    }
  }
  return(d)
}

#' Update v1.2 keys to v1.3 keys: recursive
#' @importFrom stats setNames
#' @keywords internal
#' @param d Metadata
#' @return list d: Metadata
update_lipd_v1_3_keys <- function(d){
  v12keys <- c("paleoMeasurementTable", "chronMeasurementTable", "paleoModel", "chronModel", "paleoDataMD5", "chronDataMD5", "paleoEnsembleMD5",
               "chronEnsembleMD5", "paleoEnsembleTableMD5", "chronEnsembleTableMD5", "paleoMeasurementTableMD5", "chronMeasurementTableMD5", "name",
               "calibratedAges")
  v13keys <- c("measurementTable", "measurementTable", "model", "model", "dataMD5", "dataMD5", "tableMD5", "tableMD5", "tableMD5", "tableMD5",
               "tableMD5", "tableMD5", "tableName", "distributionTable")
  v13keymap <- setNames(as.list(v13keys), v12keys)
  v12keys_rm <- c("chronTableName", "paleoTableName", "paleoDataTableName", "chronDataTableName",
                  "chronMeasurementTableName", "paleoMeasurementTableName")

  # Keys that are in this list
  keys <- names(d)

  # For any lists that are indexed by name
  if(!isNullOb(keys) && sum(!is.na(keys)) > 0){
    for(i in 1:length(keys)){
      old_key <- keys[[i]]
      # Dive down first
      if(typeof(d[[old_key]]) == "list"){
        d[[old_key]] <- update_lipd_v1_3_keys(d[[old_key]])
      }
      # When you bubble back up, then check if this key should be switched
      if(old_key %in% v12keys){
        # Set the old
        new_key <- v13keymap[[old_key]]
        d[[new_key]] <- d[[old_key]]
        # Remove the old key
        d[[old_key]] <- NULL
      } else if(old_key %in% v12keys_rm){
        d[[old_key]] <- NULL
      }
    }
  } else {
    # For any lists that are indexed by number
    if(typeof(d)=="list"){
      for(i in 1:length(d)){
        tryCatch({
          d[[i]] <- update_lipd_v1_3_keys(d[[i]])
        }, error=function(cond){
          print(paste0("Error: update_lipd_v1_3_keys: ", cond))
        })
      }
    }
  }
  return(d)
}

#' Update the structure for summary and ensemble tables
#' @keywords internal
#' @param d Metadata
#' @return d Metadata
update_lipd_v1_3_structure <- function(d){

  # Tables that need changing
  change <- c("ensembleTable", "summaryTable")
  interp <- c("isotopeInterpretation", "climateInterpretation")

  # Keys in this data
  keys <- names(d)

  # For any lists that are indexed by name
  if(!isNullOb(keys) && sum(!is.na(keys)) > 0){
    for(i in 1:length(keys)){
      curr_key <- keys[[i]]
      # Dive down first
      if(typeof(d[[curr_key]]) == "list"){
        d[[curr_key]] <- update_lipd_v1_3_structure(d[[curr_key]])
      }
      # When you bubble back up, then check if this key should be switched
      if(curr_key %in% change){
        # Found a table. Do the 'ol switcheroo
        tmp <- d[[curr_key]]
        d[[curr_key]] <- list()
        d[[curr_key]][[1]] <- tmp
      } else if (curr_key %in% interp){
        d <- merge_interpretations(d, curr_key)
      }
    }
  } else {
    # For any lists that are indexed by number
    if(typeof(d)=="list"){
      for(i in 1:length(d)){
        tryCatch({
          d[[i]] <- update_lipd_v1_3_structure(d[[i]])
        }, error=function(cond){
          print(paste0("Error: update_lipd_v1_3_structure: ", cond))
        })
      }
    }
  }
  return(d)
}


#' Merge the old interpretation fields into the new, combined interpretation field
#' @export
#' @keywords internal
#' @param d Metadata
#' @param key climateInterpretation or isotopeInterpretation
#' @return list d: Metadata
merge_interpretations <- function(d, key){
  scope <- gsub("Interpretation", "", key)
  if (!("interpretation" %in% names(d))){
    d[["interpretation"]] <- list()
  }
  pos <- length(d[["interpretation"]]) + 1
  d[["interpretation"]][[pos]] <- d[[key]]
  d[["interpretation"]][[pos]][["scope"]] <-scope
  d[[key]] <- NULL
  return(d)
}


