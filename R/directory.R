#' Ask if user wants to read one file or a directory with multiple files.
#' @export
#' @keywords internal
#' @return ans Answer to prompt (s/m)
ask_how_many <- function(){
  ans <- readline(prompt="Do you want to load a single file (s) or directory (d)? ")
  # Test if input matches what we expect. Keep prompting until valid input.
  if(!grepl("\\<s\\>",ans) & !grepl("\\<d\\>", ans))
  { return(ask_how_many()) }
  # Return a valid answer
  return(as.character(ans))
}


#' Open a file browsing gui to let the user pick a file or directory
#' @export
#' @keywords internal
#' @param ans single (s) or directory (d)
#' @return char path Directory or file path
browse_dialog <- function(ans){
  tryCatch(
    { path <- file.choose() },
    error=function(cond){
      print("Error: File must be selected")
      quit(1)
    })

  # Since R cannot choose directories, we have to manually get the directory path from the file that was chosen
  if (ans == "d" || is.null(ans)){
    path = dirname(path)
  }
  return(path)
}

#' 'Create' a temp directory (really its the same directory path for the whole R session, so it's not that new)
#' Recreate it if it's non-existant
#' @export
#' @keywords internal
#' @return char path:
create_tmp_dir <- function(){
  dir_tmp <- tempfile()
  if(!dir.exists(dir_tmp)){
    dir.create(dir_tmp)
  }
  return(dir_tmp)
}

#' Ask user where local file/directory location is.
#' @export
#' @keywords internal
#' @param path Target path
#' @return char path Directory or file path
get_src_or_dst<- function(path){
  tryCatch({
    if (!(isNullOb(path))){
      # If the provided path is not a directory and not a lipd file path, then it's not valid
      vers <- NA #initialize as NULL for future testing
      if(is(path,"data.frame")){
        if(!any("datasetId" %in% names(path))){
          stop("To use a data.frame as input, one of the columns must be named 'datasetId' and include datasetIds from the LiPDverse.")
        }
        if(any("datasetVersion" %in% names(path))){
          vers <- c(path$datasetVersion) |> str_replace_all("\\.","_")
        }else{
          vers <- rep(NA,length(path))
        }
        path <- c(path$datasetId)
      }
      if(length(path) > 1){
        if(!all(purrr::map_lgl(path,~ tools::file_ext(.x) == "lpd"))){
          if(all(purrr::map_lgl(path,is.character))){#all dsids
            if(any(is.na(vers))){
              message("Getting version information from lipdverse")
            }
            path <- purrr::map2_chr(path,vers,
                                    purrr::possibly(
                                      purrr::insistently(convert_dsid_to_path,
                                                         rate = purrr::rate_backoff(pause_base = 1,
                                                                                    pause_cap = 60,
                                                                                    pause_min = 1,
                                                                                    max_times = 20)),
                                      otherwise = NA),
                                    .progress = TRUE)
          }else{
            stop("Error: The provided vector of paths must all point to lipd files (.lpd extensions) that exist (check for full paths), or dsids on lipdverse")
          }
        }
      }else{
        #try expanding directory to a relative path
        if(!dir.exists(path)){
          if(dir.exists(file.path(getwd(),path)) |
             file.exists(file.path(getwd(),path))){
            path <- file.path(getwd(),path)
          }
        }

        if (!isDirectory(path) &&
            tools::file_ext(path) != "lpd" &&
            !startsWith(tools::file_ext(path),"json") &&
            !is.url(path)){
          if(is.character(path)){
            path <- convert_dsid_to_path(path,vers)
          }else{
            # Not a lipd file and not a directory. Stop execution and quit.
            stop("Error: The provided path must be a directory, LiPD file, a vector of paths to LiPD files, a datasetId on lipdverse, or a direct URL to a LiPD file")
          }
        }
      }
    } else {
      # Path was not given. Start prompts
      ans <- ask_how_many()
      path <- browse_dialog(ans)
    }
  }, error=function(cond){
    stop(paste0("Error: get_src_or_dst: ", cond))
  })
  return(path)
}


#' Convert datasetId to url to lipdverse lipd file
#'
#' @param dsid Dataset ID
#' @param vers Optionally specify a dataset version as a character string (e.g. "0_1_2"). Default is NA, which will get the most recent version.
#' @return path to most recent lipd file
#' @export
convert_dsid_to_path <- function(dsid,vers = NA){
  webpath <- paste0("https://lipdverse.org/data/",dsid)
  if(is.na(vers)){#get most recent
    vers <- try(stringr::str_extract(string = paste(readLines(webpath),collapse = " ") , pattern = "\\d{1,}_\\d{1,}_\\d{1,}"),silent = TRUE)

    if(is(vers,"try-error")){
      stop("Cant get dataset version from path.")
    }
  }
  path <- file.path(webpath,vers,"lipd.lpd")
  return(path)
}

#' Get a list of paths to LiPD files
#' @export
#' @keywords internal
#' @param path Directory or file path
#' @return list files File paths to LiPD files
get_lipd_paths <- function(path,jsonOnly = FALSE){
  if(length(path) > 1){
    if(length(tools::file_ext(path[1])) > 0){
      files <- path
      # }else{#it's a dsid
      #   files <- purrr::map_chr(path,convert_dsid_to_path)
    }
  }else{

    files <- list()
    if(jsonOnly){
      if (isDirectory(path)){
        if(startsWith(path,"http") | startsWith(path,"www")){
          files <- path
        }else{
          files <- list.files(path=path, pattern='\\.jsonld$', full.names = TRUE)
        }
      } else if(grepl(pattern = "jsonld",x = path)){
        files[[1]] <- path
      }
    }else{
      if (isDirectory(path)){
        files <- list.files(path=path, pattern='\\.lpd$', full.names = TRUE)
      } else if(tools::file_ext(path) == "lpd"){
        files[[1]] <- path
        # }else if(tools::file_ext(path) == ""){
        #   files[[1]] <- convert_dsid_to_path(path)
      }else{
        stop("don't recognize input")
      }
    }
  }
  return(files)
}

#' Recursive file list for current directory and below
#' @export
#' @keywords internal
#' @param x File type
#' @return char files: Matching file paths
list_files_recursive <- function(x,path){
  # create the file type filter string
  ft <- paste0("\\.", x, "$")
  # get the list of filenames from the current directory and below
  files <- list.files(path=path, pattern=ft, recursive=TRUE,full.names = TRUE)
  return(files)
}


#' Use a recursive file search to find the "data" directory of a LiPD file
#' @export
#' @keywords internal
#' @param path what is the path to the jsonld
#' @return char files: Matching file paths
find_data_dir <- function(path){
  # If there is a jsonld file, then that means we're in the data directory
  files <- list.files(path=path, pattern="\\.jsonld$", recursive=TRUE,full.names = TRUE)
  if (any(isNullOb(files))){
    stop("Error: Unable to find the 'data' directory in the LiPD file")
  }
  # Use the directory name from the jsonld path
  dir_data <- dirname(files[[1]])
  return(dir_data)
}

#' Checks if a path is a directory or not a directory
#' @export
#' @keywords internal
#' @param s Target path
#' @return boolean
isDirectory <- function(s){
  # Get the basename (last item in file path), and check it for a file extension
  # If there is not a file extension (like below), then we can assume that it's a directory
  if (all(tools::file_ext(basename(s)) == "")){
    if(all(dir.exists(s))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  # No file extension. Assume it's a file and not a directory
  return(FALSE)
}


