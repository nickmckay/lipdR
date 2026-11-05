#' Unzip LiPD file to the temporary directory
#' @importFrom utils unzip
#' @keywords internal
#' @param path Path
#' @param dir_tmp Temporary directory
#' @return none
unzipper <- function(path, dir_tmp){
  if(length(path)>0){
    success <- unzip(path, exdir = dir_tmp)
    if(length(success) == 0){
      stop(paste0("No LiPD file at the given path: ", path))
    }
  }
}

#' Zip a directory, and move up a level
#' @importFrom utils zip
#' @keywords internal
#' @param dir_original Directory to move the LiPD to
#' @param dir_tmp  Temp directory
#' @param compression_level Integer 0-9 passed to zip. Lower is faster and produces
#'   larger files; higher is slower and produces smaller files. LiPD files with large
#'   ensemble tables spend nearly all of their write time here, so the default of 2 is a
#'   good speed/size trade-off (roughly 4-5x faster than the old default of 6, for ~7\%
#'   larger files). Set to 6-9 if minimizing file size matters more than write speed.
#' @return none
zipper <- function(dir_original, dir_tmp, dsn, path, compression_level = 2){
  # zip the top lipd directory. zip file is create one level up
  zipPath <- file.path(dir_tmp, "zip")
  orig_dir <- getwd()
  include.files <- list.files(zipPath, recursive = TRUE,full.names = FALSE)
  compression_level <- max(0L, min(9L, as.integer(compression_level)))
  tryCatch({
    setwd(zipPath)#I think I have to do this
    zip(zipPath, include.files, flags = paste0("-q -X -", compression_level))
    setwd(orig_dir)
    # rename
    if (file.exists(file.path(dir_tmp,"zip.zip"))){
      if(isDirectory(path)){#then name by dataset name
        path <- file.path(path,paste0(dsn,".lpd"))
      }

      file.rename(file.path(dir_tmp,"zip.zip"), file.path(dir_tmp,basename(path)))
      file.copy(file.path(dir_tmp,basename(path)),file.path(path),overwrite=TRUE)

    }else{
      stop("zip file not created")
    }

  }, error=function(cond){
    setwd(orig_dir)
    print(paste0("Zipping failed: ", cond))
  })

}
