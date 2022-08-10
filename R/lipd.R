#' Loads a LiPD file from local path. Unzip, read, and process data
#' Steps: create tmp, unzip lipd, read files into memory, manipulate data, move to original dir, delete tmp.
#' @export
#' @keywords internal 
#' @param path Local path OR url location of LiPD file
#' @param jsonOnly Read data from jsonld only? The data will be included and the json file might be large (Typically only used for web connections)
#' @return j LiPD file data
lipd_read <- function(path,jsonOnly = FALSE){
  j <- new_lipd()
  tryCatch({
    if(jsonOnly){
      j.string <- readLines(path)
      # Remove all invalid unicode chars that would break fromJSON()
      j.string.clean <- gsub("[\001-\037]", "", j.string)
      # Parse the jsonld string data as a list
      j <- jsonlite::fromJSON(j.string.clean, simplifyDataFrame = FALSE)
      j <- rm_empty_fields(j)
      j <- update_lipd_version(j)
    }else{
    dir_tmp <- create_tmp_dir()
    unzipper(path, dir_tmp)
    data_dir <- find_data_dir(dir_tmp)
    j <- read_jsonld(data_dir)
    # j = rm_empty_doi(j)
    j <- rm_empty_fields(j)
    j <- update_lipd_version(j)
    j <- merge_csv_metadata(j,data_dir)
    j <- idx_num_to_name(j)
    # j = put_tsids(j)
    unlink(dir_tmp, recursive=TRUE)
    }
  }, error = function(cond){
    if(exists("dir_tmp")){
      unlink(dir_tmp, recursive=TRUE)
    }
    message(paste0("Error: lipd_read: ", cond))
  })

  return(j)
}

#' Saves current state of LiPD object data. Outputs to a LiPD file.
#' Steps: create tmp, create bag dir, get dsn, splice csv from json, write csv, clean json, write json, create bagit,
#' zip up bag folder, place lipd in target dst, move to original dir, delete tmp
#' @export
#' @keywords internal
#' @param j Metadata
#' @param path Destination path
#' @param dsn Dataset name
#' @param jsonOnly Write data to jsonld only? The data will be included and the json file might be large (Typically only used for web connections)
#' @return none:
lipd_write <- function(j, dir_original, path, dsn, ignore.warnings,removeNamesFromLists = FALSE,jsonOnly = FALSE){
  tryCatch({
    # dsn <- replace_invalid_chars(dsn)
    if(!jsonOnly){
    dir_tmp <- create_tmp_dir()
    # Create a lipd dir
    dir_zip <- file.path(dir_tmp, "zip")
    dir.create(dir_zip, showWarnings=FALSE)
    
    # Need an extra (identical) level for zipping later.
    dir.create(file.path(dir_zip,"bag"), showWarnings=FALSE)
    dir_bag <- file.path(dir_zip, "bag")

        #remove names from lists in key spots, since these cause errors in the json
    if(removeNamesFromLists){
    j <- remove_names_from_lists(j)
    }
    
    # look for ensemble data in PaleoData, and ask if you want to remove this data before writing the file.
    j <- warn_ensembles_in_paleo(j, ignore.warnings)
  
    j <- idx_name_to_num(j)
    tmp <- get_lipd_version(j)
    j <- tmp[["meta"]]
    dat <- get_csv_from_metadata(j, dsn)
    write_csv_to_file(dat[["csvs"]],dir_bag)
    j <- rm_empty_fields(dat[["meta"]])
    j <- jsonlite::toJSON(j, pretty=TRUE, auto_unbox = TRUE)
    write(j, file=file.path(dir_bag,"metadata.jsonld"))
    bagit(dir_bag)
    zipper(dir_original, dir_tmp, dsn, path)
    unlink(dir_tmp, recursive=TRUE)
    }else{
      j <- jsonlite::toJSON(j, pretty=TRUE, auto_unbox = TRUE)
      write(j, file=file.path(path,paste0(dsn,".jsonld")))
    }
  }, error=function(cond){
    print(paste0("Error: lipd_write: ", cond))
    if(exists("dir_tmp")){
    unlink(dir_tmp, recursive=TRUE)
    }
  })
}