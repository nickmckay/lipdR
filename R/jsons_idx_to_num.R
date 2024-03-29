#' Switch all indexing from names to numbers. 
#' @export
#' @keywords internal
#' @param d Metadata
#' @return list d: Metadata
idx_name_to_num <- function(d){
  if ("paleoData" %in% names(d)){
    d[["paleoData"]] <- export_section(d[["paleoData"]], "paleoData")
  }
  if ("chronData" %in% names(d)){
    d[["chronData"]] <- export_section(d[["chronData"]], "chronData")
  }
  d <- unindex_geo(d)
  return(d)
}

#' Index a section. paleoData or chronData
#' @export
#' @keywords internal
#' @param section Metadata
#' @param pc paleoData or chronData
#' @return list d: Metadata
export_section <- function(section, pc){
  tryCatch({
      if(!isNullOb(section)){
        for (i in 1:length(section)){
          if("measurementTable" %in% names(section[[i]])){
            section[[i]][["measurementTable"]] <- idx_table_by_num(section[[i]][["measurementTable"]])
          }
          if("model" %in% names(section[[i]])){
            section[[i]][["model"]] <- export_model(section[[i]][["model"]])
          }
          # TODO WHAT IS THIS???          
          # if section contains a bunch of table names, then use idx_table_by_num.
          # if section contains an array of data tables, then we're all set and no need to do this part.
          # if (!is.null(section) && !isNullOb(names(section)) && !"measurementTable" %in% names(section)){
          #   # Table(s) indexed by name. Move table(s) up and move the tableName inside the table
          #   section = idx_table_by_num_what(section, key1, "measurementTable")
          # }
        }
      } # end section
  }, error=function(cond){
    print(paste0("Error: export_section: ", cond))
  })
  return(section)
}

#' Index model tables
#' @export
#' @keywords internal
#' @param models Metadata
#' @return list models: Metadata
export_model <- function(models){
  tryCatch({
    for (i in 1:length(models)){
      if ("summaryTable" %in% names(models[[i]])){
        models[[i]][["summaryTable"]] <- idx_table_by_num(models[[i]][["summaryTable"]])
      }
      if ("ensembleTable" %in% names(models[[i]])){
        models[[i]][["ensembleTable"]] <- idx_table_by_num(models[[i]][["ensembleTable"]])
      }
      if ("distributionTable" %in% names(models[[i]])){
        models[[i]][["distributionTable"]] <- idx_table_by_num(models[[i]][["distributionTable"]])
      }
    }
  }, error=function(cond){
    print(paste0("Error: export_model: ", cond))
  })
  return(models)
}

#' Index tables in a loop
#' @export
#' @keywords internal
#' @param tables Metadata
#' @return list tables: Metadata
idx_table_by_num <- function(tables){
  for (i in 1:length(tables)){
    table <- tables[[i]]
    if (!is.null(table)){
      new <- idx_col_by_num(table)
      tables[[i]] <- new
    }
  }
  return(tables)
}

#' Remove column names indexing. Set them to index by their column number
#' Place the new columns under a "columns" list
#' @export
#' @keywords internal
#' @param table Metadata
#' @return list table: Metadata
idx_col_by_num <- function(table){

  tmp <- list()
  new.cols <- list()

  # get a list of variableNames from the columns
  tnames <- names(table)
  tryCatch({
    for (i in 1:length(table)){
      # if it's a list (only list types *should* be columns), then add it to tmp by index number
      if (is.list(table[[i]])){
        # set the column data into the new.cols at the current index
        new.cols[[length(new.cols) + 1]] <- table[[i]]
        # # attempt to get the variable name from this table column
        # vn <- tryCatch({
        #   vn <- table[[i]][["variableName"]]
        # }, error = function(cond){
        #   # if you don't get the variable name beacuse it's missing the key, return none.
        #   return(NULL)
        # })
        # # variableName not found, 
        # if (is.null(vn)){
        #   new.cols[[i]][["variableName"]] <- tnames[[i]]
        # }
      }
      else {
        # table item is not a column (list). Therefore, it's a root item so set it at the root of the new table
        tmp[[tnames[[i]]]] <- table[[i]]
      }
    }
    
    # set columns inside [["columns"]] list in table
    tmp[["columns"]] <- new.cols
    
  }, error=function(cond){
    print(paste0("Error: idx_col_by_num ", cond))
  })
  return(tmp)
}
