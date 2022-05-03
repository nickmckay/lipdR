###############################################
## Read/Write Lipds
###############################################
#create an environment for package variables
lipdEnv <- new.env()


#' stripExtension
#'
#' @param filename file name or path
#'
#' @return dataset name without extension
#' @export
stripExtension <- function(filename){
  basef <- basename(filename)
  if(stringr::str_detect(basef,"[.]")){#there's a period, strip
    dsn <- stringr::str_sub(basef,1,max(stringr::str_locate_all(basef,pattern = "[.]")[[1]][,1])-1)
  }else{#if not then just return the base name
    dsn <- basef
  }
  return(dsn)
}

#' Read LiPD files into R workspace
#' @export
#' @author Chris Heiser
#' @author Nick McKay
#' @import stringr
#' @keywords internal
#' @param path Source path (optional)  char
#' @param jsonOnly Load data from json only (not lpd file? Typically only used for web connections)
#' @return D : LiPD dataset(s)
#' @examples
#' \dontrun{
#' #' read in multiple datasets - no path argument
#' D <- readLipd()   # choose option 'd' for directory
#'
#' read in multiple datasets - with path argument
#' D <- readLipd("/Users/bobsmith/Desktop/lipd_files")
#' 
#' read in one dataset - no path argument
#' L <- readLipd()  # choose option "s" for single file
#' 
#' read in one dataset - with path argument
#' L <- readLipd("/Users/bobsmith/Desktop/lipd_files/dataset.lpd")
#' }
readLipd <- function(path=NULL,jsonOnly = FALSE){
  D = list()
  # Silence warnings
  options(warn = -1)
  # Ask user where files are stored, or sort the given path parameter
  path <- get_src_or_dst(path)
  
  # If this is a URL, download the file and return the local path where the file is saved. 
  path <- download_from_url(path)
  
  if(startsWith(tolower(tools::file_ext(path)),"json")){
    jsonOnly <- TRUE
  }else if(startsWith(tolower(tools::file_ext(path)),"lpd")){
    jsonOnly <- FALSE
  }

  # Get the explicit full paths for each lipd file
  entries <- get_lipd_paths(path,jsonOnly = jsonOnly)
  
  if (isNullOb(entries)){
    # Files is empty. Either not lipd files were in that path, or we had an error somewhere
    print("LiPD file(s) not found in the given path")
    print(paste0("Path: ", path))
  } else {
    for (i in 1:length(entries)){
      j <- list()
      # Entry is one file path
      entry <- entries[[i]]
      print(paste0("reading: ", basename(entry)))
      if(!jsonOnly){
        # Do initial set up
        dir_source <- dirname(entry)
        # assign("directory_source", directory_source, envir = lipdEnv)
      }
      j <- lipd_read(entry,jsonOnly = jsonOnly)
      # Get the datasetname
      dsn <- get_datasetname(j, stripExtension(entry))
      # Set the data in D using the datasetname
      D[[dsn]] <- j
    }
    if(length(D) == 1){
      D <- D[[1]]
      D <- new_lipd(D)
    }else{
      D <- new_multiLipd(D)
    }
  }
  
  return(D)
}



#' Write LiPD data onto disk as LiPD files
#' @export
#' @author Chris Heiser
#' @keywords internal
#' @param D LiPD datasets  list
#' @param path Destination path  char
#' @param jsonOnly Write data to jsonld only? The data will be included and the json file might be large (Typically only used for web connections)
#' @importFrom pkgbuild find_rtools
#' @return none
#' @examples 
#' \dontrun{
#' # write - without path argument
#' writeLipd(D)
#' 
#' # write - with path argument
#' writeLipd(D, "/Users/bobsmith/Desktop/lipd_files")
#' }
writeLipd <- function(D, path=NULL, ignore.warnings=FALSE,removeNamesFromLists = FALSE,jsonOnly = FALSE){
  if(get_os() == "windows" & pkgbuild::find_rtools() == FALSE){
    stop("Rtools package required to use writeLipd. Please go to https://cran.r-project.org/bin/windows/Rtools/ and install Rtools.")
  }
  
  tryCatch({
    fileSelect <- FALSE
    
    if(missing(path)){
      path <- browse_dialog("d")
      fileSelect <- TRUE
    }
    
    #normalize the path
    path <- normalizePath(path,mustWork = FALSE)
    
    if(isDirectory(path)){
      dir_original <- path
    }else{
      dir_original <- dirname(path)
    }
    
    
    set_bagit()
    if ("paleoData" %in% names(D)){
      print(paste0("writing: ", D[["dataSetName"]]," to ",path))
      lipd_write(D, dir_original, path, D[["dataSetName"]], ignore.warnings, removeNamesFromLists = removeNamesFromLists, jsonOnly = jsonOnly)
    } else {
      if(!isDirectory(path)){
        path <- dir_original
      }
      dsns <- names(D)
      for (i in 1:length(dsns)){
        print(paste0("writing: ", basename(dsns[i])))
        entry <- dsns[[i]]
        lipd_write(D[[entry]],dir_original, path, entry, ignore.warnings,removeNamesFromLists = removeNamesFromLists, jsonOnly = jsonOnly)
      }
    }
  }, error=function(cond){
    print(paste0("Error: writeLipd: ", cond))
  })
  
  
  
}