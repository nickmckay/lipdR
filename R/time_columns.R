# Column-level time scales.
#
# Every time column (age, year, their ensembles, and any column with calendar
# time units) carries its time scale in the LiPD object:
#   timeUnits     standardized label, e.g. "yr BP", "ka", "yr AD", "yr b2k"
#   timeDatum     year CE of time zero (1950 for BP, 0 for CE, 2000 for b2k)
#   timeDirection "prograde" (counts forward, like CE) or "retrograde" (counts back, like BP)
#   timeExponent  base-10 exponent of the units (0 for yr, 3 for ka, 6 for Ma)
#   timeSource    where the scale came from:
#                   "metadata" the file states it (timeDatum, timeDirection and timeExponent)
#                   "units"    parsed from the column's units
#                   "assumed"  the units don't say, so it was guessed from the variable name
#
# readLipd() and extractTs() add these fields; writeLipd() keeps them only when
# timeSource is "metadata", so a curator can state a scale in a file, and
# derived scales aren't written.

time_scale_fields <- c("timeUnits", "timeDatum", "timeDirection", "timeExponent", "timeSource")

#' Could a column be a time axis?
#'
#' TRUE for columns named `age`, `year`, `ageEnsemble` or `yearEnsemble`, and
#' for columns whose units are calendar time units (e.g. "yr BP", "ka",
#' "yr AD", "CE", "yr b2k"). Radiocarbon ages and uncertainty or range
#' columns are not time axes.
#' @keywords internal
#' @param variableName variableName of the column
#' @param units the column's units
#' @return logical
is_time_column <- function(variableName, units){
  vn <- tolower(trimws(paste(variableName, collapse = " ")))
  if(grepl("unc|err|sigma|\\bsd\\b|^sd|min$|max$|low|high|range|14c", vn)){
    return(FALSE)
  }
  u <- tolower(trimws(paste(units, collapse = " ")))
  if(grepl("14c", u)){
    return(FALSE)
  }
  # columns with these names count whatever their units say (some are mislabeled, e.g. "count/yr")
  if(vn %in% c("age", "year", "ageensemble", "yearensemble")){
    return(TRUE)
  }
  time_units_recognized(units)
}

#' Do these units name a calendar time scale?
#' @keywords internal
#' @param units a units string
#' @return logical
time_units_recognized <- function(units){
  u <- tolower(trimws(paste(units, collapse = " ")))
  if(length(u) == 0 || is.na(u) || u %in% c("", "null", "na", "unitless")){
    return(FALSE)
  }
  grepl("^(yr|yrs|year|years|ka|kyr|ma|myr|ga|gyr|bp|ad|ce|bc|bce|b2k|cal)\\b|^a( |$)|\\b(bp|ad|ce|bce|b2k|ka|kyr|ma|myr)\\b", u)
}

has_field <- function(x, f){
  v <- x[[f]]
  !is.null(v) && length(v) > 0 && !all(is.na(unlist(v)))
}

#' The time scale of one column
#'
#' @keywords internal
#' @param col a column (list with variableName, units, values, ...)
#' @param name the column's name in its table, used when it has no variableName
#' @return a list of `time_scale_fields`, or NULL if the column isn't a time column
column_time_scale <- function(col, name = NULL){
  if(!is.list(col)){
    return(NULL)
  }
  explicit <- all(vapply(c("timeDatum", "timeDirection", "timeExponent"), has_field, logical(1), x = col))
  if(explicit && (is.null(col$timeSource) || identical(col$timeSource, "metadata"))){
    datum <- as.numeric(unlist(col$timeDatum)[1])
    direction <- as.character(unlist(col$timeDirection)[1])
    exponent <- as.numeric(unlist(col$timeExponent)[1])
    if(!direction %in% c("prograde", "retrograde")){
      stop("Column ", col$variableName %||% name, " has timeDirection '", direction,
           "'; it must be 'prograde' or 'retrograde'.", call. = FALSE)
    }
    units <- if(has_field(col, "timeUnits")) as.character(unlist(col$timeUnits)[1]) else
      standardize_time_units(exponent, direction, paste(col$units, collapse = " "))
    return(list(timeUnits = units, timeDatum = datum, timeDirection = direction,
                timeExponent = exponent, timeSource = "metadata"))
  }
  vn <- col$variableName %||% name %||% ""
  if(!is_time_column(vn, col$units)){
    return(NULL)
  }
  meta <- time_metadata_from_units(vn, col$units)
  meta$timeSource <- if(time_units_recognized(col$units)) "units" else "assumed"
  meta
}

#' Apply a function to every column of every table in a LiPD object
#' @keywords internal
#' @param L one LiPD dataset
#' @param f function(column, name) returning the column
#' @return L
map_lipd_columns <- function(L, f){
  map_table <- function(tab){
    if(!is.list(tab)){
      return(tab)
    }
    is_col <- function(x) is.list(x) && (!is.null(x$values) || !is.null(x$variableName))
    if(is.list(tab$columns) && is.null(names(tab$columns))){
      for(k in seq_along(tab$columns)){
        if(is_col(tab$columns[[k]])){
          tab$columns[[k]] <- f(tab$columns[[k]], NULL)
        }
      }
      return(tab)
    }
    for(nm in names(tab)){
      if(nzchar(nm) && is_col(tab[[nm]])){
        tab[[nm]] <- f(tab[[nm]], nm)
      }
    }
    tab
  }
  map_tables <- function(tabs){
    for(k in seq_along(tabs)){
      tabs[[k]] <- map_table(tabs[[k]])
    }
    tabs
  }
  for(pc in c("paleoData", "chronData")){
    for(p in seq_along(L[[pc]])){
      if(!is.list(L[[pc]][[p]])){
        next
      }
      if(length(L[[pc]][[p]]$measurementTable) > 0){
        L[[pc]][[p]]$measurementTable <- map_tables(L[[pc]][[p]]$measurementTable)
      }
      for(m in seq_along(L[[pc]][[p]]$model)){
        for(tt in c("summaryTable", "ensembleTable", "distributionTable")){
          if(length(L[[pc]][[p]]$model[[m]][[tt]]) > 0){
            L[[pc]][[p]]$model[[m]][[tt]] <- map_tables(L[[pc]][[p]]$model[[m]][[tt]])
          }
        }
      }
    }
  }
  L
}

is_single_dataset <- function(D){
  any(names(D) == "dataSetName") || !is.null(D$paleoData) || !is.null(D$chronData)
}

#' Add time scales to every time column
#'
#' @description Gives every time column of a LiPD dataset (age, year, their
#'   ensembles, and any column with calendar time units) its time scale:
#'   `timeUnits`, `timeDatum`, `timeDirection`, `timeExponent` and
#'   `timeSource`. [readLipd()] and [extractTs()] do this already; use it on
#'   columns created in memory, for example by an age model.
#' @param D one LiPD dataset or several
#' @return D, with a time scale on every time column
#' @export
#' @family time
addTimeScales <- function(D){
  add_time_scales(D)
}

add_time_scales <- function(D){
  annotate <- function(col, name){
    scale <- column_time_scale(col, name)
    if(is.null(scale)){
      # a column that is no longer a time column (e.g. its units changed) loses a derived scale
      if(!is.null(col$timeSource) && !identical(col$timeSource, "metadata")){
        col[time_scale_fields] <- NULL
      }
      return(col)
    }
    col[names(scale)] <- scale
    col
  }
  if(is_single_dataset(D)){
    return(map_lipd_columns(D, annotate))
  }
  for(i in seq_along(D)){
    if(is.list(D[[i]])){
      D[[i]] <- map_lipd_columns(D[[i]], annotate)
    }
  }
  D
}

#' Remove derived time scales before writing
#'
#' @keywords internal
#' @param L one LiPD dataset
#' @return L, keeping time scales only where `timeSource` is "metadata"
strip_derived_time_scales <- function(L){
  map_lipd_columns(L, function(col, name){
    if(is.null(col$timeSource)){
      return(col)
    }
    if(!identical(col$timeSource, "metadata")){
      col[c("timeUnits", "timeDatum", "timeDirection", "timeExponent")] <- NULL
    }
    col$timeSource <- NULL
    col
  })
}
