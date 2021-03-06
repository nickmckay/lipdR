#' Get measTables
#'
#' @param L a Lipd file
#' @param pc paleo or chron tables? (default= "all)
#'
#' @return a list of data.frames
#' @export
getMeasurementTables <- function(L,pc = "all"){
  if(pc == "all"){
    pc <- c("paleo","chron")
  }  
  
  at <- list()#initialize alltables
  for(tpc in pc){
    PC <- L[[paste0(tpc,"Data")]]
    
    for(ni in 1:length(PC)){
      for(mi in 1:length(PC[[ni]]$measurementTable)){
        TT <- PC[[ni]]$measurementTable[[mi]]
        loTT <- TT[purrr::map_lgl(TT,is.list)]
        tt <- loTT[[1]]$values
        tnames <- loTT[[1]]$variableName
        if(length(loTT) > 1){
          for(c in 2:length(loTT)){
            tt <- cbind(tt,loTT[[c]]$values)
            tnames <- c(tnames,loTT[[c]]$variableName)
          }
        }
        tt <- as.data.frame(tt)
        names(tt) <- tnames
        
        #add into a list
        at[[paste0(tpc,ni,"meas",mi)]] <- tt
        
      }
    }
  }
  return(at)
}

#' Replace all blank values in csv matrices
#' @export
#' @keywords internal
#' @param csv All csv data
#' @return csv All csv data
clean_csv <- function(csvs){
  tryCatch({
    # blanks <- c("", " ", "NA", "NaN", "NAN", "nan")
    for (file in 1:length(csvs)){
      for (j in 1:length(csvs[[file]])){
        # get one column (matrix)
        column <- csvs[[file]][[j]]
        # replace all blanks in it
        # col[is.na(col) | is.nan(col)] <- NA
        column <- lapply(column, f=function(x) ifelse(is.na(x), "NaN", x), how="replace" )
        # set column back in columns
        csvs[[file]][[j]]<- column
      }
    }
  }, error=function(cond){
    print(paste0("Error: clean_csv: ", cond))
  })
  return(csvs)
}

#' Opens the target CSV file and creates a dictionary with one list for each CSV column.
#' @export
#' @importFrom readr read_csv
#' @importFrom utils count.fields
#' @keywords internal
#' @return data.list List of data for one LiPD file
read_csv_from_file <- function(){
  c <- list_files_recursive("csv")
  c.data <- vector(mode="list",length=length(c))
  # import each csv file
  for (ci in seq_along(c)){
    # Robust column type guessing with minimal overhead. Use all rows to guess 
    # but no more.
    # Get n.rows before reading in file
    
    n.rows <- length(count.fields(c[ci], blank.lines.skip = FALSE))
    
    df <- readr::read_csv(c[ci], col_names = FALSE,
                          na = c("nan", "NaN", "NAN", "NA", ""),
                          col_types = readr::cols(), guess_max = n.rows)
    
    #check column types #nope, not now.
    
    
    # #deal with missing characters
    # blanks <- c(""," ", "NA", "NaN", "NAN", "nan","")
    # blanks <- "\\s"
    # censor <- function(x){stringr::str_replace(x,  c("", " ", "NaN", "NAN", "nan"), "NA")}
    # dplyr::mutate_all(df, dplyr::funs(censor))
    # 
    #remove rows that are all NAs
    goodRows = which(rowSums(!is.na(df))>0)
    # If there are 0 good rows, then we need to make 8 rows of NA's
    if(length(goodRows)<1){
      # Create N columns with one NA value in each
      col <- ncol(df)
      tmp <- list()
      for(j in seq_along(df)){
        tmp[[j]] <- as.double(rep(NA,8)) 
      }
      c.data[[c[ci]]]=tmp
    } else {
      # Normal case: all data is here
      c.data[[c[ci]]]=df[goodRows,]
    }
  }
  return(c.data)
}


#' Write out each CSV file for this LiPD recorde
#' csvs format: [ some_filename.csv $columns.data ]
#' @export
#' @importFrom utils write.table
#' @keywords internal
#' @param list csvs: CSV data
#' @return bool success: CSV write success or fail
write_csv_to_file <- function(csvs){
  tryCatch({
    success <- TRUE
    # csvs <- clean_csv(csvs)
    entries <- names(csvs)
    
    # loop for csv file
    for (f in 1:length(entries)){
      tmp <- matrix()
      
      # one csv file: list of lists. [V1: [column values], V2: [columns values], etc.]
      entry <- entries[[f]]
      if(!isNullOb(csvs[[entry]])){
        # Loop over csv cols
        for (i in 1:length(csvs[[entry]])){
          # one column of values
          col <- csvs[[entry]][[i]]
          # check if data.frame
          if (is.data.frame(col)){
            col <- as.matrix(col)
          }
          
          # convert to numeric if needed
          if (is.list(col)){
            col <- as.numeric(col)
          }
          # replace all NA values with "NaN" before writing to file
          col <- replace(col, is.na(col), "NaN")
          
          # check if tmp matrix has data or is fresh.
          if(all(is.na(tmp))){
            # fresh, so just bind the col itself
            tmp <- tryCatch({
              cbind(col, deparse.level = 0)
            }, error = function(cond){
              print(sprintf("cbind error: %s", entry))
              return(NULL)
            })
          }else{
            # not fresh, bind the existing with the col
            tmp <- tryCatch({
              cbind(tmp, col, deparse.level = 0)
            }, error = function(cond){
              if(is.matrix(col)){
                tmp <- tryCatch({
                  col <- t(col)
                  cbind(tmp, col, deparse.level = 0)
                }, error = function(cond){
                  print(sprintf("cbind error: %s", entry))
                  return(NULL)
                })
              }
              else{
                return(NULL)
              }
            })
            # cbind didn't work here, it's possible the matrix is transposed wrong.
            # give it another try after transposing it.
            # if (is.null(tmp) & is.matrix(col)){
            #   
            # }
          }
        }
      }
      if (!is.null(tmp)){
        success <- tryCatch({
          write.table(tmp, file=entry, col.names = FALSE, row.names=FALSE, sep=",")
          success <- TRUE
        }, error=function(cond){
          print(paste0("Error: write_csv_to_file: write.table: ", entry, cond))
          print("Check data for unequal row or column lengths")
          return(NULL)
        })
      }
    }
  }, error=function(cond){
    print(paste0("Error: write_csv_to_file: ", cond))
  })
}
