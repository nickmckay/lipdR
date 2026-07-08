#' Filter a time series by compilation membership
#'
#' Keeps only the time series entries (columns) that belong to a named compilation,
#' optionally constrained by version. Compilation membership is stored on each column
#' in the `inCompilation<i>_compilationName` and `inCompilation<i>_compilationVersion`
#' fields (a column can belong to more than one compilation). Legacy files that used the
#' older `inCompilationBeta` name are migrated automatically when read, so this function
#' only needs to look at `inCompilation`.
#'
#' @param ts A `lipd_ts` list or a `lipd_ts_tibble`.
#' @param name Character. Compilation name to match (case-insensitive, partial match).
#' @param version Optional. A version to compare compilation membership against. When
#'   `NULL` (default), entries are matched on `name` only.
#' @param version.op Character. Comparison operator applied to `version`; one of
#'   `">="` (default), `">"`, `"=="`, `"<="`, `"<"`. For example, `version = "2",
#'   version.op = ">="` keeps entries in version 2 or greater of the compilation.
#' @return A filtered object of the same class as `ts` (list in, list out; tibble in,
#'   tibble out).
#' @export
#' @examples
#' \dontrun{
#' ts <- extractTs(readLipd())
#' # everything in the Temp12k compilation
#' filterByCompilation(ts, "Temp12k")
#' # only version 1.0.0 or greater
#' filterByCompilation(ts, "Temp12k", version = "1.0.0", version.op = ">=")
#' }
filterByCompilation <- function(ts, name, version = NULL, version.op = ">="){
  isTibble <- is.lipdTsTibble(ts)
  tsList <- as.lipdTs(ts)

  version.op <- match.arg(version.op, c(">=", ">", "==", "<=", "<"))
  cmp <- switch(version.op,
                ">=" = `>=`, ">" = `>`, "==" = `==`, "<=" = `<=`, "<" = `<`)

  keep <- vapply(tsList, function(entry){
    entry_in_compilation(entry, name, version, cmp)
  }, logical(1))

  out <- structure(tsList[keep], class = c("lipd_ts", class(list())))
  if(isTibble){
    out <- as.lipdTsTibble(out)
  }
  return(out)
}

#' Does one time series entry belong to a compilation?
#' @keywords internal
#' @param entry A single time series entry (list)
#' @param name Compilation name to match (case-insensitive, partial)
#' @param version Optional version constraint
#' @param cmp Comparison function to apply against `version`
#' @return logical
entry_in_compilation <- function(entry, name, version, cmp){
  nm <- names(entry)
  nameKeys <- nm[grepl("^inCompilation[0-9]+_compilationName$", nm)]
  if(length(nameKeys) == 0){
    return(FALSE)
  }
  for(nk in nameKeys){
    cn <- unlist(entry[[nk]])
    if(is.null(cn) || length(cn) == 0){
      next
    }
    if(any(grepl(tolower(name), tolower(cn), fixed = TRUE))){
      if(is.null(version)){
        return(TRUE)
      }
      vk <- sub("_compilationName$", "_compilationVersion", nk)
      cv <- entry[[vk]]
      if(!is.null(cv) && compilation_version_matches(cv, version, cmp)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Compare compilation version(s) against a target version
#' @keywords internal
#' @param cv Compilation version value(s) for one compilation block (may be a vector)
#' @param version Target version
#' @param cmp Comparison function
#' @return logical: TRUE if any recorded version satisfies cmp(recorded, target)
compilation_version_matches <- function(cv, version, cmp){
  cv <- unlist(cv)
  target <- tryCatch(numeric_version(as.character(version)), error = function(e) NA)
  for(v in cv){
    recorded <- tryCatch(numeric_version(as.character(v)), error = function(e) NA)
    if(!is.na(recorded) && !is.na(target)){
      if(isTRUE(cmp(recorded, target))){
        return(TRUE)
      }
    } else if(identical(as.character(v), as.character(version))){
      return(TRUE)
    }
  }
  return(FALSE)
}
