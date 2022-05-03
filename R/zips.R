#' Unzip LiPD file to the temporary directory
#' @export
#' @importFrom utils unzip
#' @keywords internal
#' @param path Path
#' @param tmp Temporary directory
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
#' @export
#' @importFrom utils zip
#' @keywords internal
#' @param dir_original Directory to move the LiPD to
#' @param dir_tmp  Temp directory
#' @return none
zipper <- function(dir_original, dir_tmp, dsn, path){
  # zip the top lipd directory. zip file is create one level up
  zipPath <- file.path(dir_tmp, "zip")
  include.files <- list.files(file.path(zipPath), recursive = TRUE,full.names = FALSE)
  tryCatch({
    zip(zipPath, include.files,flags="-q")
    # rename
    if (file.exists(file.path(dir_tmp,"zip.zip"))){
      if(isDirectory(path)){#then name by dataset name
        file.rename(file.path(dir_tmp,"zip.zip"), file.path(dir_tmp,paste0(dsn, ".lpd")))
        if(file.path(dir_tmp,paste0(dsn, ".lpd"))){
          file.copy(file.path(dir_tmp,paste0(dsn, ".lpd")), 
                    file.path(dir_original,paste0(dsn, ".lpd")), 
                    overwrite=TRUE)
        }
      }else{
        file.rename(file.path(dir_tmp,"zip.zip"), file.path(dir_tmp,path))
        file.copy(file.path(dir_tmp,path),
                  file.path(dir_original,path), 
                  overwrite=TRUE)
        
      }
    }
    
  }, error=function(cond){
    print(paste0("Zipping failed: ", cond))
  })
  
}
