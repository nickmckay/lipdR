# Filtering timeseries by their interpretations.
#
# In a timeseries, interpretations are stored by position:
# interpretation1_variable, interpretation1_seasonality, interpretation2_variable, ...
# The climate interpretation can be number 1 in one record and number 3 in
# another, so getInterpretations() turns them into one row per interpretation,
# and filterByInterpretation() matches conditions within each interpretation.

interp_key_pattern <- "^interpretation(\\d+)_(.+)$"

# one field value as a single string (NA when empty)
interp_value_string <- function(x){
  x <- unlist(x)
  if(length(x) == 0 || all(is.na(x))){
    return(NA_character_)
  }
  x <- as.character(x[!is.na(x)])
  x <- x[nzchar(trimws(x))]
  if(length(x) == 0){
    return(NA_character_)
  }
  paste(x, collapse = "; ")
}

# a field of every timeseries entry, as a character vector
ts_field_strings <- function(tsList, key){
  vapply(tsList, function(e) interp_value_string(e[[key]]), character(1), USE.NAMES = FALSE)
}

#' One row per interpretation
#'
#' @description Interpretations are stored by position in a timeseries
#'   (`interpretation1_variable`, `interpretation2_variable`, ...), so the same
#'   kind of interpretation can have a different number in different records.
#'   This returns them as a table with one row per interpretation of each
#'   timeseries, for exploring the vocabulary or filtering. A missing `scope`
#'   is treated as "climate".
#' @param ts a `lipd_ts` list, a `lipd_ts_tibble`, or a long timeseries tibble
#'   from [tidyTs()]
#' @return a tibble with columns `index` (position of the timeseries in `ts`),
#'   `TSid`, `dataSetName`, `variableName`, `number` (the interpretation's
#'   number in its timeseries), `scope`, and one character column for every
#'   other interpretation field present (e.g. `variable`, `seasonality`,
#'   `direction`, `rank`). Multiple values in one field are joined with "; ".
#' @export
#' @family interpretations
#' @examples
#' \dontrun{
#' ts <- extractTs(readLipd())
#' interp <- getInterpretations(ts)
#' table(interp$scope, interp$variable)
#' }
getInterpretations <- function(ts){
  tsList <- as.lipdTs(ts)
  n <- length(tsList)
  keys <- unique(unlist(lapply(tsList, names)))
  keys <- keys[grepl(interp_key_pattern, keys)]

  base <- tibble::tibble(
    index = seq_len(n),
    TSid = ts_field_strings(tsList, "paleoData_TSid"),
    dataSetName = ts_field_strings(tsList, "dataSetName"),
    variableName = ts_field_strings(tsList, "paleoData_variableName")
  )

  empty <- tibble::add_column(base[0, ], number = integer(0), scope = character(0))
  if(length(keys) == 0){
    return(empty)
  }

  num <- as.integer(sub(interp_key_pattern, "\\1", keys))
  field <- sub(interp_key_pattern, "\\2", keys)
  fields <- unique(c("scope", setdiff(field, "scope")))

  rows <- lapply(sort(unique(num)), function(k){
    out <- base
    out$number <- rep(k, n)
    for(f in fields){
      key <- keys[num == k & field == f]
      out[[f]] <- if(length(key) == 1) ts_field_strings(tsList, key) else rep(NA_character_, n)
    }
    # keep only timeseries that have this interpretation
    has <- Reduce(`|`, lapply(keys[num == k], function(key) !is.na(out[[field[keys == key]]])))
    out[has, , drop = FALSE]
  })
  out <- dplyr::bind_rows(rows)
  if(nrow(out) == 0){
    return(empty)
  }
  out$scope[is.na(out$scope)] <- "climate"
  out <- out[order(out$index, out$number), , drop = FALSE]
  tibble::as_tibble(out)
}

normalize_interp <- function(x){
  tolower(trimws(as.character(x)))
}

#' Filter timeseries by their interpretations
#'
#' @description Keeps the timeseries whose interpretations match a set of
#'   conditions. Conditions are checked within each interpretation, so
#'   `variable = "temperature", seasonality = "annual"` needs one
#'   interpretation that is both, not a temperature interpretation and a
#'   separate annual one.
#'
#' @param ts a `lipd_ts` list, a `lipd_ts_tibble`, or a long timeseries tibble
#'   from [tidyTs()]
#' @param ... conditions on one interpretation, combined with AND:
#'   * named arguments, `field = values`, match when the field equals any of
#'     the values (case and surrounding spaces ignored), e.g.
#'     `variable = "temperature", seasonality = c("annual", "summer")`.
#'   * unnamed R expressions are evaluated on each interpretation (a row of
#'     [getInterpretations()]), for OR across fields or other logic, e.g.
#'     `variable == "temperature" | variableDetail == "air@surface"`. Values
#'     are compared as written here (case matters), and a missing value
#'     counts as no match.
#' @param scope only consider interpretations with this scope (or one of
#'   these scopes), e.g. "climate" or "isotope". Case is ignored. A missing
#'   scope counts as "climate". NULL (default) considers all.
#' @param match how the interpretations of one timeseries combine:
#'   * "any" (default): at least one interpretation matches
#'   * "all": it has interpretations (within `scope`), and all of them match
#'   * "none": no interpretation matches (timeseries without
#'     interpretations are kept)
#'   * "primary": the top interpretation matches: the lowest `rank` where
#'     ranks are given, otherwise the lowest number
#'
#'   For AND across interpretations ("a temperature and a precipitation
#'   interpretation"), call the function twice.
#' @return `ts` with only the matching timeseries, in the same class
#' @export
#' @family interpretations
#' @examples
#' \dontrun{
#' ts <- extractTs(readLipd())
#' # any interpretation of annual temperature
#' filterByInterpretation(ts, variable = "temperature", seasonality = "annual")
#' # the primary climate interpretation is temperature or precipitation
#' filterByInterpretation(ts, variable = c("temperature", "precipitation"),
#'                        scope = "climate", match = "primary")
#' # OR across fields
#' filterByInterpretation(ts, variable == "temperature" | variableDetail == "air@surface")
#' # a temperature interpretation AND a precipitation interpretation
#' ts |>
#'   filterByInterpretation(variable = "temperature") |>
#'   filterByInterpretation(variable = "precipitation")
#' # records without any isotope interpretation
#' filterByInterpretation(ts, scope = "isotope", match = "none")
#' }
filterByInterpretation <- function(ts, ..., scope = NULL, match = c("any", "all", "none", "primary")){
  match <- match.arg(match)
  conds <- rlang::enquos(...)
  tsList <- as.lipdTs(ts)
  interp <- getInterpretations(tsList)

  if(!is.null(scope)){
    interp <- interp[normalize_interp(interp$scope) %in% normalize_interp(scope), , drop = FALSE]
  }

  hit <- rep(TRUE, nrow(interp))
  nms <- names(conds)
  if(is.null(nms)){
    nms <- rep("", length(conds))
  }
  for(i in seq_along(conds)){
    if(nzchar(nms[i])){
      f <- nms[i]
      values <- rlang::eval_tidy(conds[[i]])
      if(!f %in% names(interp)){
        warning("No interpretation has a '", f, "' field, so nothing matches it. ",
                "Fields present: ", paste(setdiff(names(interp), c("index", "TSid", "dataSetName", "variableName")), collapse = ", "),
                call. = FALSE)
        hit <- rep(FALSE, nrow(interp))
        next
      }
      hit <- hit & !is.na(interp[[f]]) & normalize_interp(interp[[f]]) %in% normalize_interp(values)
    }else{
      res <- rlang::eval_tidy(conds[[i]], data = interp)
      if(!is.logical(res) || !(length(res) %in% c(1, nrow(interp)))){
        stop("Each unnamed condition must give TRUE or FALSE for each interpretation.", call. = FALSE)
      }
      res <- rep_len(res, nrow(interp))
      hit <- hit & !is.na(res) & res
    }
  }

  n <- length(tsList)
  idx <- interp$index
  keep <- switch(match,
    any = seq_len(n) %in% idx[hit],
    none = !seq_len(n) %in% idx[hit],
    all = seq_len(n) %in% idx & !seq_len(n) %in% idx[!hit],
    primary = {
      rank <- if("rank" %in% names(interp)) suppressWarnings(as.numeric(interp$rank)) else rep(NA_real_, nrow(interp))
      # order within each timeseries: ranked interpretations first by rank, then by number
      o <- order(idx, is.na(rank), rank, interp$number)
      first <- o[!duplicated(idx[o])]
      seq_len(n) %in% idx[first][hit[first]]
    }
  )

  subset_ts(ts, keep, tsList)
}

# keep entries of a timeseries object (list, wide tibble or long tibble), preserving its class
subset_ts <- function(ts, keep, tsList){
  if(is.lipdTsTibbleLong(ts) || (is.data.frame(ts) && !is.lipdTsTibble(ts) && looks_like_long_ts(ts))){
    tsid <- ts_field_strings(tsList, "paleoData_TSid")[keep]
    out <- ts[ts$paleoData_TSid %in% tsid, , drop = FALSE]
  }else if(is.data.frame(ts)){
    out <- ts[keep, , drop = FALSE]
  }else{
    out <- ts[keep]
  }
  class(out) <- class(ts)
  out
}
