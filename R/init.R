###############################################
## Read/Write Lipds
###############################################
#create an environment for package variables
lipdEnv <- new.env()

#' update the query table with most recent lipdverse version
#'
#' @export
#'
updateQueryTable <- function(){
  if(checkZipMd5() == FALSE){
    message("Updating Query Table\n")
    #download queryTable
    queryTable <- newQueryTable()
    #Replace local copy
    replaceLocalZipMD5()

  }else{
    message("Query Table up to date")
  }
}


#' Download the remote query table
#'
#' @return queryTable
#'
getQueryTable <- function(){
  temp <- tempdir()
  zip_path <- file.path(temp, "/queryTable.zip")
  getRemote <- FALSE

  if(exists("queryTable", envir = lipdEnv)){
    queryTable <- get("queryTable",lipdEnv)
  }else{

    if(file.exists(zip_path)){
      fileMd5 <- tools::md5sum(zip_path)
      if(fileMd5 == readLines("https://lipdverse.org/lipdverse/lipdverseQuery.md5", warn=FALSE)){
        queryTable <- read.csv(fPth)
      }else{
        getRemote <- TRUE
      }
    }else{
      getRemote <- TRUE
    }
  }

  if(getRemote){
    queryTable <- newQueryTable()
  }


  assign("queryTable", queryTable, envir = lipdEnv)
  return(queryTable)
}

#' Download the remote query table
#'
#' @return queryTable
#'
newQueryTable <- function(){
  query_url <- "https://lipdverse.org/lipdverse/lipdverseQuery.zip"
  temp <- tempdir()
  zip_dir <- paste0(temp, "/queryTable.zip")
  download.file(query_url, zip_dir)
  unzip(zipfile = zip_dir, files = "lipdverseQuery.csv", exdir = temp)
  fPth <- paste0(temp, "/lipdverseQuery.csv")
  queryTable <- read.csv(fPth)

  replaceLocalZipMD5()
  assign("queryTable", queryTable, envir = lipdEnv)
  return(queryTable)
}

updateStandardTables <- function(){
  standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS"),"rb")
  assign("standardTables", standardTables, envir = lipdEnv)
  return(standardTables)
}

#' Compare the MD5 sums for the queryTable zip between locally stored and current remote
#'
#' @return out
#'
checkZipMd5 <- function(){
  out <- tryCatch(
    {
      ZIPmd5Remote <- readLines("https://lipdverse.org/lipdverse/lipdverseQuery.md5", warn=FALSE)
      if(exists("ZIPmd5Local",envir = lipdEnv)){
        ZIPmd5Local <- get("ZIPmd5Local",envir = lipdEnv)
      }else{
        ZIPmd5Local <- readLines(file.path(tempdir(),"lipdverseQuery.md5"), warn = FALSE)
      }
      out <- ZIPmd5Local == ZIPmd5Remote
    },
    error=function(cond){
      return(FALSE)
    },
    warning = function(cond){
      return(FALSE)
    },
    finally = {}
  )
  return(out)
}



#' Replace the query zip file MD5 sums after replacing the query table
#'
#'
replaceLocalZipMD5 <- function(){
  ZIPmd5Remote <- readLines("https://lipdverse.org/lipdverse/lipdverseQuery.md5", warn = FALSE)
  ZIPmd5Local <- ZIPmd5Remote
  write(ZIPmd5Local,file = file.path(tempdir(),"lipdverseQuery.md5"))
  assign("ZIPmd5Local", ZIPmd5Remote, envir = lipdEnv)
}




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
#' @import stringr furrr purrr
#' @keywords high-level
#' @param path A string specifying a file, url, or directory, or a vector of strings specifying files or urls. Alternatively, no entry (default) will open a selection windw..
#' @param jsonOnly Load data from json only (not lpd file? Typically only used for web connections)
#' @param parallel load data in parallel? Default = FALSE. Can greatly increase load time for large collections of data. Uses furrr, so you'll need to run future::plan() before calling the function. e.g. `future::plan(multisession,workers = 8)`
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
readLipd <- function(path=NULL,jsonOnly = FALSE,parallel = FALSE){
  D = list()
  # Silence warnings
  options(warn = -1)
  # Ask user where files are stored, or sort the given path parameter
  orig.path <- path
  path <- get_src_or_dst(path)

  # If this is a URL, download the file and return the local path where the file is saved.
  path <- download_from_url(path)
  if(any(path == "download-error")){
    warning(glue::glue("Failed to download {orig.path}, returning a null object"))
    return(NULL)
  }

  if(!all(file.exists(path))){
    stop(glue::glue("{path} does not exist - are you pointing to the right file(s)"))
  }

  if(all(startsWith(tolower(tools::file_ext(path)),"json"))){
    jsonOnly <- TRUE
  }else if(all(startsWith(tolower(tools::file_ext(path)),"lpd")) | all(isDirectory(path))){
    jsonOnly <- FALSE
  }else{
    stop("needs to be all json or all lpd")
  }

  # Get the explicit full paths for each lipd file
  entries <- get_lipd_paths(path,jsonOnly = jsonOnly)

  if (isNullOb(entries)){
    # Files is empty. Either not lipd files were in that path, or we had an error somewhere
    print("LiPD file(s) not found in the given path")
    print(paste0("Path: ", path))
  } else {

    if(length(path) > 1){
      print(paste0("Loading ", length(entries)," datasets from the supplied list of paths..."))
    }else{
      print(paste0("Loading ", length(entries)," datasets from ",path,"..."))
    }

    if(length(entries) < 20){
      few <- TRUE
    }else{
      few <- FALSE
      pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                           max = length(entries), # Maximum value of the progress bar
                           style = 3,    # Progress bar style (also available style = 1 and style = 2)
                           char = "=")   # Character used to create the bar
    }

    errors <- parseFail <- c()

if(parallel & !few){

  FO <- furrr::future_map(entries,purrr::quietly(lipd_read),jsonOnly = jsonOnly,.progress = TRUE)

  D <- purrr::map(FO,purrr::pluck,"result")
  dsn <- purrr::map_chr(D,"dataSetName")
  names(D) <- dsn


  #check for parsing failures

  allWarnings <- purrr::map(FO,purrr::pluck,"warnings") |>
    purrr::map_chr(.f = \(x) ifelse(any(grepl(pattern = "parsing fail",x)),yes = "parse_fail",no = NA))

  allErrors <- map(FO,purrr::pluck,"messages") |>
    purrr::map_chr(.f = \(x) ifelse(length(x) > 0,yes = "read_failure",no = NA))

  parseFail <- dsn[which(!is.na(allWarnings))]
  errors <- dsn[which(!is.na(allErrors))]

}else{#old way, may not be necessary


    for (i in 1:length(entries)){
      j <- list()
      # Entry is one file path
      entry <- entries[[i]]

      if(few){
        print(paste0("reading: ", basename(entry)))
      }else{
        setTxtProgressBar(pb, i)
      }

      if(!jsonOnly){
        # Do initial set up
        dir_source <- dirname(entry)
        # assign("directory_source", directory_source, envir = lipdEnv)
      }

      J <- purrr::map(entry,purrr::quietly(lipd_read),jsonOnly = jsonOnly)
      j <- J[[1]]$result

      # Get the datasetname
      dsn <- get_datasetname(j, stripExtension(entry))

      parseFail[i] <- ifelse(
        any(grepl(pattern = "parsing fail",J[[1]]$warnings)),
        yes = dsn,
        no = NA)

      errors[i] <- ifelse(
      length(J[[1]]$messages) > 0,
        yes = dsn,
        no = NA)

      # assign most recent version to datasetVersion
      j$datasetVersion <- getVersion(j)

      # Set the data in D using the datasetname
      D[[dsn]] <- j
    }
}


    if(length(D) == 1){
      D <- D[[1]]
      D <- new_lipd(D)
    }else{
      D <- new_multiLipd(D)
      succ <- length(D)
      # for (jkl in 1:succ){
      #   class(D[[jkl]]) <- c("lipd",class(list()))
      #   cat("setting class of lipd #",jkl, "\n")
      # }
      parsingFailures <- sum(!is.na(parseFail))
      failed <- sum(!is.na(errors))
      cat("\n\n")

      if(parsingFailures > 0){
        print(paste0("Loaded ", succ," datasets, with ",failed," error(s) and ",parsingFailures," parsing failure(s) in datasets:"))
        print(as.character(na.omit(parseFail)))
      }else{
        print(paste0("Successfully loaded ", succ," datasets, with ",failed," failure(s)."))
      }

      if(failed > 0){
        cat("\n")
        print(paste0("The following dataset(s) loaded with errors:"))
        print(as.character(na.omit(errors)))
      }

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
writeLipd <- function(D,
                      path=NULL,
                      ignore.warnings=FALSE,
                      removeNamesFromLists = FALSE,
                      jsonOnly = FALSE){
  if(get_os() == "windows" & pkgbuild::find_rtools() == FALSE){
    stop("Rtools package required to use writeLipd. Please go to https://cran.r-project.org/bin/windows/Rtools/ and install Rtools.")
  }

  tryCatch({
    fileSelect <- FALSE

    if(missing(path)){
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


    if ("paleoData" %in% names(D)){
      print(paste0("writing: ", D[["dataSetName"]]," to ",path))
      lipd_write(D,
                 dir_original,
                 path,
                 D[["dataSetName"]],
                 ignore.warnings,
                 removeNamesFromLists = removeNamesFromLists,
                 jsonOnly = jsonOnly)
    } else {
      if(!isDirectory(path)){
        path <- dir_original
      }

      dsns <- names(D)
      print(paste0("Writing ", length(dsns)," datasets to ",path,"..."))

      pbc<- txtProgressBar(min = 0,      # Minimum value of the progress bar
                           max = length(dsns), # Maximum value of the progress bar
                           style = 3,    # Progress bar style (also available style = 1 and style = 2)
                           char = "=")   # Character used to create the bar

      error <- c()
      for (i in 1:length(dsns)){
        if(length(dsns) < 10){
          print(paste0("writing: ", basename(dsns[i])))
        }else{
          setTxtProgressBar(pbc, i)
        }
        entry <- dsns[[i]]
        o <- lipd_write(D[[entry]],
                   dir_original,
                   path,
                   entry,
                   ignore.warnings,
                   removeNamesFromLists = removeNamesFromLists,
                   jsonOnly = jsonOnly)

        if(o != 0){
          error <- dsns[[i]]
        }
      }
      nerror <- length(error)
      nsuccess <- length(dsns)-nerror
      print(glue::glue("Successfully wrote {nsuccess} files with {nerror} failure(s)."))
      if(length(error) > 0){
        cat("\n\n")
        print("The following files had issues writing:")
        print(error)
      }

    }


  }, error=function(cond){
    print(paste0("Error: writeLipd: ", cond))
  })



}

lipdFromEntry <- function(entry,jsonOnly){


  if(!jsonOnly){
    # Do initial set up
    dir_source <- dirname(entry)
    # assign("directory_source", directory_source, envir = lipdEnv)
  }

  J <- purrr::map(entry,purrr::quietly(lipd_read),jsonOnly = jsonOnly)

  return(J)
}
