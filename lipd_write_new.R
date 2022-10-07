

D <- readLipd("C:/Users/dce72/Downloads/testData/TriangleLakeBog.Jensen.2021.lpd")


path="C:/Users/dce72/Desktop/lipdR/TestPath"
ignore.warnings=FALSE
removeNamesFromLists = FALSE
jsonOnly = FALSE

fileSelect <- FALSE
if(is.null(path)){
  if(is.lipd(D)){
    path <- browse_dialog("s")
  }else if(is.multiLipd(D)){
    path <- browse_dialog("d")
  }else{
    stop("The input object should be of class lipd or multiLipd")
  }
  fileSelect <- TRUE
}

if(fileSelect & is.lipd(D)){
  if(stripExtension(basename(path)) != D$dataSetName){
    ans <- askYesNo(glue::glue("Your selected file name: '{path}' does not match the dataSetName: '{D$dataSetName}'.\n Do you want to continue?"))
    if(!ans){stop("You chose not to continue")}
  }
}

#normalize the path
path <- normalizePath(path,mustWork = FALSE)

if(isDirectory(path)){
  dir_original <- path
}else{
  dir_original <- dirname(path)
}

j=D
dir_original=dir_original
path=path
dsn=D[["dataSetName"]]
ignore.warnings=ignore.warnings
removeNamesFromLists = removeNamesFromLists
jsonOnly = jsonOnly

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
      j <- rm_empty_fields(dat[["meta"]])
      j <- jsonlite::toJSON(j, pretty=TRUE, auto_unbox = TRUE)
      write(j, file=file.path(dir_zip,"metadata.jsonld"))
      
      #Calculate Payload-Oxum
      OctetCount <- sum(nchar(tmp[["meta"]], type = "bytes"),
      nchar(dat[["csvs"]], type = "bytes"))
      
      StreamCount <- length(list.files(dir_zip))
      payloadOxum <- paste(OctetCount, StreamCount, sep = ".")
      
      
      
      #create data subfolder
      dir.create(file.path(dir_bag,"data"), showWarnings=FALSE)
      dir_data <- file.path(dir_zip, "data")

      
      
      #write MD5 manifest
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
      read_csv_from_file(file.path(dir_data,"metadata.jsonld"))


      
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