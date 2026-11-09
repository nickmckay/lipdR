`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' Parse a time units string into datum, direction and exponent
#' @keywords internal
#' @param units a units string, e.g. "yr BP", "ka", "CE", "yr AD", "yr b2k"
#' @param variableName optional variable name ("age" or "year") used when the units are ambiguous
#' @return list(timeUnits, timeDatum, timeDirection, timeExponent, radiocarbon)
time_spec <- function(units, variableName = ""){
  meta <- time_metadata_from_units(variableName, units)
  meta$radiocarbon <- identical(meta$timeUnits, "yr 14C BP")
  meta
}

#' Time metadata of a timeseries entry, from its fields
#' @keywords internal
#' @param datum,direction,exponent values of timeDatum, timeDirection and timeExponent (may be NULL)
#' @param units value of timeUnits (may be NULL)
#' @return a time_spec() list
time_spec_from_fields <- function(datum, direction, exponent, units){
  first <- function(x){
    x <- unlist(x)
    if(length(x) == 0 || is.na(x[1])) NULL else x[1]
  }
  datum <- first(datum)
  direction <- first(direction)
  exponent <- first(exponent)
  units <- first(units)
  if(!is.null(units)){
    spec <- time_spec(units)
  }else{
    spec <- NULL
  }
  if(!is.null(datum) && !is.null(direction) && !is.null(exponent)){
    spec <- list(timeUnits = units %||% standardize_time_units(as.numeric(exponent), direction, NA_character_),
                 timeDatum = as.numeric(datum),
                 timeDirection = as.character(direction),
                 timeExponent = as.numeric(exponent),
                 radiocarbon = isTRUE(spec$radiocarbon))
  }
  spec
}

#' Convert time values between time scales
#'
#' @description Converts ages or years from one time scale to another, using
#'   each scale's datum (the year CE of time zero), direction ("prograde" for
#'   time counting forward, like year CE; "retrograde" for time counting back,
#'   like yr BP) and base-10 exponent (3 for ka, 6 for Ma). Radiocarbon years
#'   ("yr 14C BP") cannot be converted, since they need calibration.
#'
#' @param x numeric vector or matrix of time values
#' @param from the scale of `x`: a units string (e.g. "yr BP", "ka", "CE",
#'   "yr b2k"), or a list with `timeDatum`, `timeDirection` and `timeExponent`
#'   (as created by [extractTs()])
#' @param to the target scale, as a units string or list (default = "yr BP")
#'
#' @return `x` on the target scale, with the same shape
#' @export
#' @family time
#' @examples
#' convertTime(c(1850, 1950, 2000), from = "CE", to = "yr BP")
#' convertTime(c(1.2, 11.7), from = "ka", to = "yr BP")
#' convertTime(c(50, 100), from = "yr b2k", to = "CE")
convertTime <- function(x, from, to = "yr BP"){
  from <- as_time_spec(from)
  to <- as_time_spec(to)
  same <- from$timeDatum == to$timeDatum &&
    from$timeDirection == to$timeDirection &&
    from$timeExponent == to$timeExponent &&
    from$radiocarbon == to$radiocarbon
  if(same){
    return(x)
  }
  if(from$radiocarbon || to$radiocarbon){
    stop("Can't convert radiocarbon years (yr 14C BP) to or from calendar years. Calibrate them first.", call. = FALSE)
  }
  sgn <- function(direction) if(direction == "prograde") 1 else -1
  ce <- from$timeDatum + sgn(from$timeDirection) * x * 10^from$timeExponent
  (ce - to$timeDatum) * sgn(to$timeDirection) / 10^to$timeExponent
}

as_time_spec <- function(x){
  if(is.list(x)){
    spec <- time_spec_from_fields(x$timeDatum, x$timeDirection, x$timeExponent, x$timeUnits)
    if(is.null(spec)){
      stop("A time scale given as a list needs timeDatum, timeDirection and timeExponent, or timeUnits.", call. = FALSE)
    }
    return(spec)
  }
  if(!is.character(x) || length(x) != 1 || is.na(x)){
    stop("A time scale must be a units string such as 'yr BP', 'ka' or 'CE', or a list of time metadata.", call. = FALSE)
  }
  time_spec(x)
}

#' Coerce one timeseries entry to a list
#' @keywords internal
#' @param ts one element of a TS list, or a one-row data frame
#' @return a named list
as_ts_entry <- function(ts){
  if(is.data.frame(ts)){
    if(nrow(ts) != 1){
      stop("Expected one timeseries (one row), but got ", nrow(ts), " rows.", call. = FALSE)
    }
    ts <- purrr::transpose(as.list(ts))[[1]]
  }
  if(!is.list(ts)){
    stop("Expected one timeseries: an element of a TS list or a one-row timeseries tibble.", call. = FALSE)
  }
  ts
}

has_values <- function(x){
  v <- suppressWarnings(as.numeric(unlist(x)))
  length(v) > 0 && any(is.finite(v))
}

#' The values and time scale of one time variable of a timeseries entry
#' @keywords internal
#' @param ts a timeseries entry (list)
#' @param variable name of the field, e.g. "age", "year", "ageEnsemble"
#' @return list(values, scale, source, units, variable), or NULL if there are no values
ts_time_variable <- function(ts, variable){
  if(!has_values(ts[[variable]])){
    return(NULL)
  }
  pre <- function(f) ts[[paste0(variable, f)]]
  scale <- time_spec_from_fields(pre("TimeDatum"), pre("TimeDirection"), pre("TimeExponent"), pre("TimeUnits"))
  source <- unlist(pre("TimeSource"))[1] %||% NA_character_
  rawUnits <- pre("Units")
  if(is.null(scale)){
    # timeseries extracted before lipdR 0.8.0: parse the units
    if(grepl("ensemble", tolower(variable)) && is.null(rawUnits)){
      base <- sub("Ensemble$", "", variable)
      rawUnits <- ts[[paste0(base, "Units")]]
    }
    isYear <- grepl("year", tolower(variable))
    if(!isYear && !grepl("age", tolower(variable)) && !time_units_recognized(rawUnits)){
      stop("'", variable, "' is not a time variable of this timeseries.", call. = FALSE)
    }
    if(is.null(rawUnits)){
      scale <- time_spec(if(isYear) "yr AD" else "yr BP", if(isYear) "year" else "age")
      source <- "assumed"
    }else{
      scale <- time_spec(rawUnits, if(isYear) "year" else "age")
      source <- if(time_units_recognized(rawUnits)) "units" else "assumed"
    }
  }
  # an ensemble whose own units don't say borrows the scale of its age or year column
  if(identical(source, "assumed") && grepl("Ensemble$", variable)){
    base <- ts_time_variable_scale_only(ts, sub("Ensemble$", "", variable))
    if(!is.null(base) && !identical(base$source, "assumed")){
      scale <- base$scale
      source <- base$source
    }
  }
  values <- ts[[variable]]
  values <- if(is.matrix(values) || is.data.frame(values)) as.matrix(values) else unlist(values)
  storage.mode(values) <- "double"
  list(values = values, scale = scale, source = source, units = rawUnits, variable = variable)
}

ts_time_variable_scale_only <- function(ts, variable){
  if(!has_values(ts[[variable]])){
    return(NULL)
  }
  out <- tryCatch(ts_time_variable(ts, variable), error = function(e) NULL)
  out
}

#' Get the time values of a timeseries on a given time scale
#'
#' @description Returns a time axis of one timeseries, converted to `units`.
#'   By default this is the primary time axis chosen by [extractTs()]
#'   (`variable = "time"`). `time` points to its source column
#'   (`timeVariableName`), and the column is read, so edits to it after
#'   extraction are respected. With `ensemble = TRUE`, an age or year ensemble
#'   is used when there is one, so age uncertainty is kept. Any other time
#'   variable of the entry can be asked for by name, e.g. `variable = "year"`.
#'
#' @param ts one timeseries: an element of a TS list, or one row of a timeseries tibble
#' @param units the time scale to return, e.g. "yr BP" (default), "ka", "CE". NA returns the values unconverted.
#' @param ensemble use an age or year ensemble when there is one? (default = TRUE)
#' @param variable which time variable: "time" (the primary time axis, default), or the name of a time column, e.g. "age", "year", "ageEnsemble"
#' @param warn.assumed warn when the variable's time scale was assumed, because its units don't say? (default = TRUE)
#'
#' @return a numeric vector, or a matrix (rows are observations, columns ensemble members), on the requested scale. Its attributes give the scale ("timeUnits"), the variable used ("timeVariableName") and where its scale came from ("timeSource": "metadata", "units" or "assumed").
#' @export
#' @family time
getTsTime <- function(ts, units = "yr BP", ensemble = TRUE, variable = "time", warn.assumed = TRUE){
  ts <- as_ts_entry(ts)
  tv <- NULL
  if(ensemble){
    ensVars <- switch(variable,
                      time = c("ageEnsemble", "yearEnsemble"),
                      age = "ageEnsemble",
                      year = "yearEnsemble",
                      character(0))
    for(ev in ensVars){
      tv <- ts_time_variable(ts, ev)
      if(!is.null(tv)) break
    }
  }
  if(is.null(tv) && variable == "time"){
    src <- unlist(ts[["timeVariableName"]])[1]
    if(!is.null(src) && !is.na(src) && src != "time"){
      tv <- tryCatch(ts_time_variable(ts, src), error = function(e) NULL)
      if(!is.null(tv)){
        check_time_cache(ts, tv)
      }
    }
    if(is.null(tv) && has_values(ts[["time"]])){
      scale <- time_spec_from_fields(ts[["timeDatum"]], ts[["timeDirection"]], ts[["timeExponent"]], ts[["timeUnits"]])
      if(is.null(scale)){
        stop("This timeseries has time values but no timeUnits or timeDatum/timeDirection/timeExponent, so its scale is unknown.", call. = FALSE)
      }
      values <- unlist(ts[["time"]])
      storage.mode(values) <- "double"
      tv <- list(values = values, scale = scale,
                 source = unlist(ts[["timeSource"]])[1] %||% NA_character_,
                 units = ts[["timeUnits"]], variable = "time")
    }
    if(is.null(tv)){
      tv <- ts_time_variable(ts, "age") %||% ts_time_variable(ts, "year")
    }
    if(is.null(tv)){
      stop("This timeseries has no time, age or year values.", call. = FALSE)
    }
  }
  if(is.null(tv)){
    tv <- ts_time_variable(ts, variable)
    if(is.null(tv)){
      stop("This timeseries has no '", variable, "' values. Its time variables are: ",
           paste(tsTimeVariables(ts), collapse = ", "), ".", call. = FALSE)
    }
  }

  if(warn.assumed && identical(tv$source, "assumed")){
    warning(time_assumed_message(ts, tv), call. = FALSE)
  }
  if(length(units) == 1 && is.na(units)){
    out <- tv$values
    attr(out, "timeUnits") <- tv$scale$timeUnits
  }else{
    out <- convertTime(tv$values, from = tv$scale, to = units)
    attr(out, "timeUnits") <- as_time_spec(units)$timeUnits
  }
  attr(out, "timeVariableName") <- tv$variable
  attr(out, "timeSource") <- tv$source
  out
}

#' The time variables of a timeseries
#'
#' @description Lists the time variables of one timeseries that [getTsTime()]
#'   can return: "time" (the primary time axis) and every time column that
#'   [extractTs()] carried into the entry, such as "age", "year" or "ageEnsemble".
#' @inheritParams getTsTime
#' @return a character vector
#' @export
#' @family time
tsTimeVariables <- function(ts){
  ts <- as_ts_entry(ts)
  nm <- names(ts)
  vars <- sub("TimeDatum$", "", nm[grepl(".TimeDatum$", nm)])
  legacy <- intersect(c("age", "year", "ageEnsemble", "yearEnsemble"), nm)
  vars <- unique(c(vars, legacy))
  vars <- vars[vapply(vars, function(v) has_values(ts[[v]]), logical(1))]
  if(has_values(ts[["time"]])){
    vars <- c("time", vars)
  }
  vars
}

time_assumed_message <- function(ts, tv){
  who <- paste(c(unlist(ts[["dataSetName"]])[1], unlist(ts[["paleoData_variableName"]])[1]), collapse = " ")
  u <- paste(unlist(tv$units), collapse = " ")
  glue::glue("{who}: the units of '{tv$variable}' ('{if(nzchar(u)) u else 'none'}') don't say which time scale it is on, so {tv$scale$timeUnits} was assumed. Set its units, or timeDatum, timeDirection and timeExponent, to say.")
}

# time is a copy of its source column. If they disagree (the column or time
# was edited after extractTs()), the column wins, with a warning.
check_time_cache <- function(ts, tv){
  if(!has_values(ts[["time"]])){
    return(invisible(TRUE))
  }
  cacheScale <- time_spec_from_fields(ts[["timeDatum"]], ts[["timeDirection"]], ts[["timeExponent"]], ts[["timeUnits"]])
  if(is.null(cacheScale) || is.matrix(tv$values)){
    return(invisible(TRUE))
  }
  cache <- suppressWarnings(as.numeric(unlist(ts[["time"]])))
  expected <- tryCatch(convertTime(tv$values, from = tv$scale, to = cacheScale), error = function(e) NULL)
  same <- !is.null(expected) && length(cache) == length(expected) &&
    isTRUE(all.equal(cache, as.numeric(expected), tolerance = 1e-8, check.attributes = FALSE))
  if(!same){
    warning("'time' no longer matches its source column '", tv$variable,
            "', so '", tv$variable, "' was used. Re-run extractTs() or convertTsTime() to update 'time'.", call. = FALSE)
  }
  invisible(same)
}

#' Put every timeseries on the same time scale
#'
#' @description Converts the `time` of every timeseries to `units`, and
#'   updates `timeUnits`, `timeDatum`, `timeDirection`, `timeExponent`,
#'   `timeMin` and `timeMax` to match. Records whose primary time is year CE
#'   and records in yr BP or ka can then be analysed together. Timeseries
#'   without `time` get one from `age` or `year`. Ensembles are not changed;
#'   use [getTsTime()] for those.
#'
#' @param TS a TS list ([extractTs()]), or a timeseries tibble ([ts2tibble()])
#' @param units the target time scale (default = "yr BP")
#'
#' @return `TS`, of the same class, with converted time
#' @export
#' @family time
#' @examples
#' \dontrun{
#' TS <- extractTs(D)
#' TS <- convertTsTime(TS, "yr BP")
#' }
convertTsTime <- function(TS, units = "yr BP"){
  target <- as_time_spec(units)
  convert_one <- function(ts){
    tm <- getTsTime(ts, units = units, ensemble = FALSE, warn.assumed = FALSE)
    src <- attr(tm, "timeVariableName")
    how <- attr(tm, "timeSource")
    tm <- as.vector(tm)
    ts[["time"]] <- tm
    if(is.null(ts[["timeVariableName"]]) && !identical(src, "time")){
      ts[["timeVariableName"]] <- src
    }
    if(is.null(ts[["timeSource"]])){
      ts[["timeSource"]] <- how
    }
    ts[["timeUnits"]] <- target$timeUnits
    ts[["timeDatum"]] <- target$timeDatum
    ts[["timeDirection"]] <- target$timeDirection
    ts[["timeExponent"]] <- target$timeExponent
    v <- tm[is.finite(tm)]
    if(length(v) > 0){
      ts[["timeMin"]] <- min(v)
      ts[["timeMax"]] <- max(v)
    }
    ts
  }

  if(is.data.frame(TS)){
    rows <- purrr::map(purrr::transpose(as.list(TS)), convert_one)
    TS$time <- purrr::map(rows, "time")
    for(f in c("timeUnits", "timeDirection", "timeVariableName", "timeSource")){
      TS[[f]] <- purrr::map_chr(rows, ~ as.character(.x[[f]] %||% NA_character_))
    }
    for(f in c("timeDatum", "timeExponent", "timeMin", "timeMax")){
      TS[[f]] <- purrr::map_dbl(rows, ~ .x[[f]] %||% NA_real_)
    }
    return(TS)
  }
  cl <- class(TS)
  out <- purrr::map(TS, convert_one)
  class(out) <- cl
  out
}

#' Describe a time scale
#'
#' @description Turns a units string (e.g. "yr BP", "ka", "CE", "yr b2k"), a
#'   list of time metadata, or a timeseries entry into a standard description
#'   of its time scale. Use it to compare scales or check their direction.
#'
#' @param x a units string, a list with `timeDatum`, `timeDirection` and
#'   `timeExponent` (or `timeUnits`), or one timeseries (whose `time` scale is used)
#' @return a list: `timeUnits` (standardized label), `timeDatum` (year CE of
#'   time zero), `timeDirection` ("prograde" or "retrograde"), `timeExponent`
#'   (base-10 exponent) and `radiocarbon` (logical)
#' @export
#' @family time
#' @examples
#' timeScale("ka")
#' timeScale("CE")$timeDirection
timeScale <- function(x){
  as_time_spec(x)
}

#' Does a time scale count forward?
#'
#' @description TRUE for scales that count forward in time, like year CE
#'   ("prograde"), FALSE for scales that count back, like yr BP, ka or b2k
#'   ("retrograde"). On a prograde scale larger values are younger; on a
#'   retrograde scale larger values are older.
#' @inheritParams timeScale
#' @return logical
#' @export
#' @family time
#' @examples
#' isPrograde("yr AD")
#' isPrograde("ka")
isPrograde <- function(x){
  identical(timeScale(x)$timeDirection, "prograde")
}

ts_entries <- function(TS){
  if(is.data.frame(TS)){
    return(purrr::transpose(as.list(TS)))
  }
  if(is.list(TS) && !is.null(TS[["paleoData_values"]] %||% TS[["chronData_values"]])){
    return(list(TS))
  }
  unclass(TS)
}

ts_label <- function(ts){
  paste(c(unlist(ts[["dataSetName"]])[1], unlist(ts[["paleoData_variableName"]] %||% ts[["chronData_variableName"]])[1]),
        collapse = " ")
}

#' Get the time axis of every timeseries, on one time scale
#'
#' @description Runs [getTsTime()] on every timeseries in a TS list or
#'   timeseries tibble, converting each to `units`. Instead of a warning per
#'   record, it gives one warning that lists the records whose time scale was
#'   assumed, and one for records without the time variable.
#'
#' @param TS a TS list ([extractTs()]) or a timeseries tibble ([ts2tibble()])
#' @inheritParams getTsTime
#' @param missing what to do with timeseries that don't have the time
#'   variable: "warn" (default; their element is NULL), "error" or "quiet"
#' @return a list with one element per timeseries: a numeric vector, or a
#'   matrix for an ensemble, on the `units` scale (NULL where there was no
#'   time). Its "timeUnits" attribute gives the scale.
#' @export
#' @family time
getTsTimes <- function(TS, units = "yr BP", variable = "time", ensemble = TRUE,
                       warn.assumed = TRUE, missing = c("warn", "error", "quiet")){
  missing <- match.arg(missing)
  entries <- ts_entries(TS)
  assumed <- character(0)
  absent <- character(0)
  out <- vector("list", length(entries))
  for(i in seq_along(entries)){
    tm <- tryCatch(getTsTime(entries[[i]], units = units, ensemble = ensemble,
                             variable = variable, warn.assumed = FALSE),
                   error = function(e){
                     if(grepl("has no", conditionMessage(e))) NULL else stop(ts_label(entries[[i]]), ": ", conditionMessage(e), call. = FALSE)
                   })
    if(is.null(tm)){
      absent <- c(absent, ts_label(entries[[i]]))
      next
    }
    if(identical(attr(tm, "timeSource"), "assumed")){
      assumed <- c(assumed, ts_label(entries[[i]]))
    }
    out[i] <- list(tm)
  }
  list_some <- function(x){
    x <- unique(x)
    paste0(paste(utils::head(x, 10), collapse = "; "), if(length(x) > 10) paste0("; and ", length(x) - 10, " more") else "")
  }
  if(length(absent) > 0){
    msg <- paste0(length(absent), " timeseries have no '", variable, "' and were skipped: ", list_some(absent))
    if(missing == "error"){
      stop(msg, call. = FALSE)
    }else if(missing == "warn"){
      warning(msg, call. = FALSE)
    }
  }
  if(warn.assumed && length(assumed) > 0){
    warning("The time scale of ", length(unique(assumed)), " timeseries was assumed, because their units don't say: ",
            list_some(assumed), ". Check them with getTsTime(), and set their units, or timeDatum, timeDirection and timeExponent.",
            call. = FALSE)
  }
  if(!(length(units) == 1 && is.na(units))){
    attr(out, "timeUnits") <- timeScale(units)$timeUnits
  }
  out
}

#' The time scale to use for a set of timeseries
#'
#' @description If every timeseries is on the same time scale, returns it, so
#'   results stay on the records' own axis. Otherwise returns `default` with a
#'   message. Analysis functions use this when the user doesn't choose a scale.
#'
#' @inheritParams getTsTimes
#' @param default the scale to use when the records are on different scales (default = "yr BP")
#' @param quiet don't give the message? (default = FALSE)
#' @return a standardized units string
#' @export
#' @family time
commonTimeUnits <- function(TS, variable = "time", ensemble = FALSE, default = "yr BP", quiet = FALSE){
  entries <- ts_entries(TS)
  scales <- unique(unlist(lapply(entries, function(ts){
    tm <- tryCatch(getTsTime(ts, units = NA, ensemble = ensemble, variable = variable, warn.assumed = FALSE),
                   error = function(e) NULL)
    if(is.null(tm)) NULL else attr(tm, "timeUnits")
  })))
  if(length(scales) == 1){
    return(scales)
  }
  if(!quiet && length(scales) > 1){
    message("The timeseries are on different time scales (", paste(scales, collapse = ", "), "), so ",
            timeScale(default)$timeUnits, " is used. Set the time units to choose another.")
  }
  timeScale(default)$timeUnits
}
