###############################################
## Read/Write Lipds
###############################################
#create an environment for package variables
lipdEnv <- new.env()

#' update the query table with most recent lipdverse version
#'
#' @export
#'
update_queryTable <- function(){

  if(checkZIPmd5() == FALSE){
    message("Updating Query Table\n")
    #download queryTable
    queryTable <- newQueryTable()
    #Replace local copy
    usethis::use_data(queryTable, overwrite = TRUE, compress = "xz")
    replaceLocalZipMD5()
    #devtools::reload()
    #load("data/queryTable.rda", .GlobalEnv)
    #replace local MD5
    assign("queryTable", newQueryTable(), envir = .GlobalEnv)

  }else{
    message("Query Table up to date")
  }
}

#' Download the remote query table
#'
#' @return queryTable
#'
newQueryTable <- function(){
  query_url <- "http://lipdverse.org/lipdverse/lipdverseQuery.zip"
  temp <- tempdir()
  zip_dir <- paste0(temp, "/queryTable.zip")
  download.file(query_url, zip_dir)
  unzip(zipfile = zip_dir, files = "lipdverseQuery.csv", exdir = temp)
  fPth <- paste0(temp, "/lipdverseQuery.csv")
  queryTable <- read.csv(fPth)
  unlink(temp)
  #assign("queryTable", queryTable, envir = lipdEnv)
  return(queryTable)
}

#' Compare the MD5 sums for the queryTable zip between locally stored and current remote
#'
#' @return out
#'
checkZIPmd5 <- function(){
  out <- tryCatch(
    {
      ZIPmd5Remote <- readLines("https://lipdverse.org/lipdverse/lipdverseQuery.md5", warn=FALSE)

      ZIPmd5Local == ZIPmd5Remote
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
  message("New MD5: ", ZIPmd5Local)
  usethis::use_data(ZIPmd5Local, overwrite = TRUE)
  assign("ZIPmd5Local", ZIPmd5Remote, envir = .GlobalEnv)
  #load("data/ZIPmd5Local.rda", .GlobalEnv)
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
#' @import stringr
#' @keywords high-level
#' @param path A string specifying a file, url, or directory, or a vector of strings specifying files or urls. Alternatively, no entry (default) will open a selection windw..
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

  if(all(startsWith(tolower(tools::file_ext(path)),"json"))){
    jsonOnly <- TRUE
  }else if(all(startsWith(tolower(tools::file_ext(path)),"lpd"))){
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


      # Set the data in D using the datasetname
      D[[dsn]] <- j
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
