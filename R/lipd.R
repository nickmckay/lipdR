#' Loads a LiPD file from local path. Unzip, read, and process data
#' Steps: create tmp, unzip lipd, read files into memory, manipulate data, move to original dir, delete tmp.
#' @export
#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @param path Local path OR url location of LiPD file
#' @param jsonOnly Read data from jsonld only? The data will be included and the json file might be large (Typically only used for web connections)
#' @param dont.load.ensemble This option doesn't load in ensemble data, but stores them in a temporary directory. If when that object is then written back out using `writeLipd()`, if that temporary directory still exists it will add the ensemble data back in. Default = FALSE
#' @return j LiPD file data
lipd_read <- function(path,jsonOnly = FALSE, dont.load.ensemble = FALSE){
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
      #any ensemble data?
      if(dont.load.ensemble & !any(grepl(list_files_recursive("csv",dir_tmp),pattern = "ensemble"))){#only allow this option if there are ensembles
        dont.load.ensemble <- FALSE
      }
      data_dir <- find_data_dir(dir_tmp)
      j <- read_jsonld(data_dir)
      # j = rm_empty_doi(j)
      j <- rm_empty_fields(j)
      j <- update_lipd_version(j)
      j <- merge_csv_metadata(j,data_dir,dont.load.ensemble = dont.load.ensemble)
      if(dont.load.ensemble){#then copy the unzipped lipd appropriately.
        if(is.null(j$datasetId)){
          j$datasetId <- createDatasetId()
        }
        ensembleStoreDir <- file.path(tempdir(),paste0(j$datasetId,"_",str_replace_all(as.character(lubridate::now()),pattern = "[^0-9-]",replacement = "-")))
        j$savedEnsembles <- ensembleStoreDir
        dir.create(ensembleStoreDir)
        unzipper(path, ensembleStoreDir)
      }
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
#' @importFrom stats complete.cases
#' @return none:
lipd_write <- function(j, dir_original, path, dsn, ignore.warnings,removeNamesFromLists = FALSE,jsonOnly = FALSE){
  tryCatch({
    # dsn <- replace_invalid_chars(dsn)
    if(!jsonOnly){
      dir_tmp <- create_tmp_dir()
      # Create a lipd dir
      dir_zip <- file.path(dir_tmp, "zip")
      dir.create(dir_zip, showWarnings=FALSE)

      #remove names from lists in key spots, since these cause errors in the json
      if(removeNamesFromLists){
        j <- remove_names_from_lists(j)
      }

      # Need an extra (identical) level for zipping later.
      dir.create(file.path(dir_zip,"bag"), showWarnings=FALSE)
      dir_bag <- file.path(dir_zip, "bag")


      # write csvs and metadata
      j <- warn_ensembles_in_paleo(j, ignore.warnings)
      j <- idx_name_to_num(j)
      tmp <- get_lipd_version(j)
      j <- tmp[["meta"]]
      dat <- get_csv_from_metadata(j, dsn)
      write_csv_to_file(dat[["csvs"]],dir_zip)

      if(!is.null(j$savedEnsembles)){#then we need to write in the ensembles stored in the this directory
        if(dir.exists(j$savedEnsembles)){
            enscsvs <- list_files_recursive(x = "csv",path = j$savedEnsembles)
            enscsvs <- enscsvs[grepl(pattern = "ensemble",enscsvs)]
            if(length(enscsvs) == 0){
              stop(glue::glue("This LiPD file has ensembles that were not loaded in, but there don't appear to be any csv files labeled 'ensemble' in the temporary folder here: {j$savedEnsembles}"))
            }
            purrr::walk(enscsvs,file.copy,to = dir_zip,overwrite = TRUE)
            if(grepl(j$savedEnsembles, pattern = tempdir())){#make sure it's a tempdir() before deleting
              unlink(j$savedEnsembles, recursive = TRUE)
            }
            j$savedEnsembles <- NULL
        }else{
          stop(glue::glue("This LiPD file has ensembles that were not loaded in, and that should be stored in a temporary folder here: {j$savedEnsembles}, however that directory doesn't exist. To write the file without the ensembles, set L$savedEnsembles <- NULL."))
      }

      }


      j <- rm_empty_fields(dat[["meta"]])
      j <- jsonlite::toJSON(j, pretty=TRUE, auto_unbox = TRUE)
      write(j, file=file.path(dir_zip,"metadata.jsonld"))

      #Calculate Payload-Oxum
      OctetCount <- sum(nchar(tmp[["meta"]], type = "bytes"),
                        nchar(dat[["csvs"]], type = "bytes")) #this is slow for big csvs

      StreamCount <- length(list.files(dir_zip))
      payloadOxum <- paste(OctetCount, StreamCount, sep = ".")


      #create data subfolder and move payload files
      dir.create(file.path(dir_bag,"data"), showWarnings=FALSE)
      dir_data <- file.path(dir_bag, "data")

      payloadFiles <- list.files(dir_zip)["bag" != list.files(dir_zip)]

      file.copy(from = paste0(dir_zip, "/", payloadFiles),
                to = paste0(dir_data, "/", payloadFiles))

      file.remove(from = paste0(dir_zip, "/", payloadFiles))

      #write MD5 payload manifest
      manifest1 <- tools::md5sum(paste(dir_data, list.files(dir_data), sep = "/"))
      manifest1 <- data.frame(manifest1, list.files(dir_data))
      write.table(x = manifest1,
                  file = file.path(dir_bag, "manifest-md5.txt"),
                  row.names = F,
                  col.names = F,
                  quote = F,
                  sep = '\t')

      #write bagit.txt - we're just faking this one
      write("BagIt-Version: 0.97\nTag-File-Character-Encoding: UTF-8", file=file.path(dir_bag,"bagit.txt"))

      #write bag-info.txt
      write(paste0("Bag-Software-Agent: lipd_write <https://github.com/nickmckay/lipdR/tree/main/R/lipd.R>\n",
                   "Bagging-Date: ", Sys.Date(), "\n",
                   "Payload-Oxum: ", payloadOxum),
            file=file.path(dir_bag,"bag-info.txt"))

      #write MD5 tag manifest
      manifest2 <- tools::md5sum(paste(dir_bag, list.files(dir_bag), sep = "/"))
      manifest2 <- data.frame(manifest2, list.files(dir_bag))
      manifest2 <- manifest2[complete.cases(manifest2),]
      write.table(x = manifest2,
                  file = file.path(dir_bag, "tagmanifest-md5.txt"),
                  row.names = F,
                  col.names = F,
                  quote = F,
                  sep = '\t')



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
    return(1)
  })
}
