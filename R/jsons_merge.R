###############################################
## Read LiPDs - Merge
## Merge metadata and csv into one LiPD object
###############################################

#' Using the given metadata dictionary, retrieve CSV data from CSV files, and insert the CSV
#' values into their respective metadata columns.
#' @keywords internal
#' @param path Wgere to read the csv file
#' @param dont.load.ensemble This option doesn't load in ensemble data, but stores them in a temporary directory. If when that object is then written back out using `writeLipd()`, if that temporary directory still exists it will add the ensemble data back in. Default = FALSE
#' @param d Metadata
#'
#' @return list d: Metadata
merge_csv_metadata <- function(d,path,dont.load.ensemble = FALSE){
  # Read in CSV data
  csvs <- read_csv_from_file(path, dont.load.ensemble = dont.load.ensemble)
  # Run for each section that exists
  if ("paleoData" %in% names(d)){
    d[["paleoData"]] <- merge_csv_section(d[["paleoData"]], "paleo", csvs)
  }
  if ("chronData" %in% names(d)){
    d[["chronData"]] <- merge_csv_section(d[["chronData"]], "chron", csvs)
  }

  return(d)
}

#' Merge CSV into each section
#' @export
#' @keywords internal
#' @param section Metadata
#' @param crumbs paleo or chron
#' @param csvs CSV, sorted by filename
#' @return list section: Metadata
merge_csv_section <- function(section, crumbs, csvs){
  tryCatch({
    for (i in 1:length(section)){
      # Measurement
      if ("measurementTable" %in% names(section[[i]])){
        section[[i]][["measurementTable"]] <- merge_csv_table(section[[i]][["measurementTable"]], paste0(crumbs, i, "measurement"), csvs)
      }
      # Model
      if("model" %in% names(section[[i]])){
        section[[i]][["model"]] <- merge_csv_model(section[[i]][["model"]], paste0(crumbs, i, "model"), csvs)
      }
    }
  }, error=function(cond){
    print(paste0("Error: merge_csv_section: ", cond))
  })
  return(section)
}


#' Merge CSV into each model
#' @keywords internal
#' @param crumbs Crumbs
#' @param models Models to merge
#' @param csvs CSV data
#'
#' @return list models: Metadata
merge_csv_model <- function(models, crumbs, csvs){
  tryCatch({
    for (i in 1:length(models)){
      if("summaryTable" %in% names(models[[i]])){
        models[[i]][["summaryTable"]] <- merge_csv_table(models[[i]][["summaryTable"]], paste0(crumbs, i, "measurement"), csvs)
      }
      if("ensembleTable" %in% names(models[[i]])){
        models[[i]][["ensembleTable"]] <- merge_csv_table(models[[i]][["ensembleTable"]], paste0(crumbs, i, "ensemble"), csvs)
      }
      if("distributionTable" %in% names(models[[i]])){
        models[[i]][["distributionTable"]] <- merge_csv_table(models[[i]][["distributionTable"]], paste0(crumbs, i, "distribution"), csvs)
      }
    }
  }, error=function(cond){
    print(paste0("Error: merge_csv_model: ", cond))
  })
  return(models)
}


#' Merge CSV data into each table
#' @keywords internal
#' @param crumbs Crumbs
#' @param tables tables to merge
#' @param csvs CSV data
#'
#' @return list models: Metadata
merge_csv_table <- function(tables, crumbs, csvs){
  tryCatch({
    for (i in 1:length(tables)){
      filename <- tables[[i]][["filename"]]
      if (!is.null(filename)){
        csv.cols <- csvs[[filename]]
        meta.cols <- tables[[i]][["columns"]]
        tables[[i]][["columns"]] <- merge_csv_columns(csv.cols, meta.cols)
        if(is.null(tables[[i]][["tableName"]])){
          # insert crumbs. this is the standardized table name
          tables[[i]][["tableName"]] <- paste0(crumbs, i)
        }
        # remove filename. we have the values imported, so we dont need it anymore. we'll make a new standarized one!
        tables[[i]][["filename"]] <- NULL
      }
    }
  }, error=function(cond){
    print(paste0("Error: merge_csv_table: ", cond))
  })
  return(tables)
}


#' Merge values into each column
#' @export
#' @keywords internal
#' @param csvs Values, sorted by column
#' @param meta Table metadata, sorted by column
#' @return list meta: Table metadata
merge_csv_columns <- function(csvs, meta){
  tryCatch({
    total_csv_cols <- length(csvs)
    max_claimed <- 0L

    for (i in 1:length(meta)){
      num <- meta[[i]][["number"]]

      if (is.null(num)){
        # Count how many null-number columns remain after position i
        after_indices <- if (i < length(meta)) (i + 1):length(meta) else integer(0)
        null_after <- sum(purrr::map_lgl(after_indices, function(k) is.null(meta[[k]][["number"]])))
        unclaimed <- total_csv_cols - max_claimed

        if (null_after == 0L && unclaimed > 0L){
          # This is the last null-number column and CSV columns remain.
          # Assign all unclaimed CSV columns (single or ensemble).
          if (unclaimed == 1L){
            num <- max_claimed + 1L
          } else {
            num <- as.list(seq(max_claimed + 1L, total_csv_cols))
          }
          meta[[i]][["number"]] <- num
        }
        # else: not the last null-number col, or no CSV cols remain; leave values NULL
      }

      if (!is.null(meta[[i]][["number"]])){
        num <- meta[[i]][["number"]]
        # special case for ensemble tables - a "column" that holds many columns
        if (is.list(num) | length(num) > 1){
          tmp <- list()
          for (j in 1:length(num)){
            tmp[[j]] <- csvs[[num[[j]]]]
          }
          meta[[i]][["values"]] <- matrix(unlist(tmp), ncol=length(tmp))
          max_claimed <- max(unlist(num))
        } else {
          idx <- num
          if(is.character(idx)){
            idx <- as.numeric(idx)
          }
          meta[[i]][["values"]] <- csvs[[idx]]
          max_claimed <- max(max_claimed, as.integer(idx))
        }
      }
    }
  }, error=function(cond){
    print(paste0("Error: merge_csv_columns: column: ",names(meta[[i]]),". CSV may be incorrectly formatted: ", cond))
  })
  return(meta)
}


