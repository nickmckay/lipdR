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

#' Get the time values of a timeseries on a given time scale
#'
#' @description Returns the time axis of one timeseries, converted to `units`.
#'   With `ensemble = TRUE`, an age or year ensemble (`ageEnsemble`,
#'   `yearEnsemble`) is used when there is one, so age uncertainty is kept.
#'   Otherwise it uses `time` and its metadata (added by [extractTs()]), and
#'   falls back to `age` or `year` for timeseries without them.
#'
#' @param ts one timeseries: an element of a TS list, or one row of a timeseries tibble
#' @param units the time scale to return, e.g. "yr BP" (default), "ka", "CE". NA returns the values unconverted.
#' @param ensemble use an age or year ensemble when there is one? (default = TRUE)
#'
#' @return a numeric vector, or a matrix (rows are observations, columns ensemble members), on the requested scale. Its "timeUnits" attribute gives the scale.
#' @export
#' @family time
getTsTime <- function(ts, units = "yr BP", ensemble = TRUE){
  ts <- as_ts_entry(ts)
  values <- NULL
  if(ensemble){
    if(has_values(ts[["ageEnsemble"]])){
      values <- ts[["ageEnsemble"]]
      from <- time_spec(ts[["ageEnsembleUnits"]] %||% ts[["ageUnits"]] %||% "yr BP", "age")
    }else if(has_values(ts[["yearEnsemble"]])){
      values <- ts[["yearEnsemble"]]
      from <- time_spec(ts[["yearEnsembleUnits"]] %||% ts[["yearUnits"]] %||% "yr AD", "year")
    }
  }
  if(is.null(values) && has_values(ts[["time"]])){
    values <- ts[["time"]]
    from <- time_spec_from_fields(ts[["timeDatum"]], ts[["timeDirection"]], ts[["timeExponent"]], ts[["timeUnits"]])
    if(is.null(from)){
      stop("This timeseries has time values but no timeUnits or timeDatum/timeDirection/timeExponent, so its scale is unknown.", call. = FALSE)
    }
  }
  if(is.null(values) && has_values(ts[["age"]])){
    values <- ts[["age"]]
    from <- time_spec(ts[["ageUnits"]] %||% "yr BP", "age")
  }
  if(is.null(values) && has_values(ts[["year"]])){
    values <- ts[["year"]]
    from <- time_spec(ts[["yearUnits"]] %||% "yr AD", "year")
  }
  if(is.null(values)){
    stop("This timeseries has no time, age or year values.", call. = FALSE)
  }

  values <- if(is.matrix(values) || is.data.frame(values)) as.matrix(values) else unlist(values)
  storage.mode(values) <- "double"
  if(length(units) == 1 && is.na(units)){
    attr(values, "timeUnits") <- from$timeUnits
    return(values)
  }
  out <- convertTime(values, from = from, to = units)
  attr(out, "timeUnits") <- as_time_spec(units)$timeUnits
  out
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
    tm <- getTsTime(ts, units = units, ensemble = FALSE)
    attr(tm, "timeUnits") <- NULL
    ts[["time"]] <- tm
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
    for(f in c("timeUnits", "timeDirection")){
      TS[[f]] <- purrr::map_chr(rows, f)
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
