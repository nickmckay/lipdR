#' Compilation-specific metadata (csm)
#'
#' Some metadata is a judgement made *by a compilation about a dataset* rather
#' than a property of the dataset itself: a QC certification, a compilation's
#' own identifier for a record, a flag saying whether the record is used in a
#' particular analysis.
#'
#' Historically these were stored as flat keys in the same namespace as
#' everything else, so several compilations wrote the same field and the last
#' one to run silently overwrote the others. They are now nested inside the
#' `inCompilation` entry that already records membership:
#'
#' ```
#' inCompilation[[i]] = list(
#'   compilationName    = "iso2k",
#'   compilationVersion = c("1_0_0", "1_0_1"),
#'   csm                = list(certification = "B", UI = "...")
#' )
#' ```
#'
#' In a flattened time series these appear as `<compilation>_csm_<field>`, e.g.
#' `iso2k_csm_certification`. The compilation, not the array index, keys the
#' name: index is positional and a compilation appears at different indices in
#' different datasets, so index-based names would scatter one logical field
#' across several columns.
#'
#' `csm` holds one current value per field. It is not versioned; history belongs
#' in the compilation's own records.
#'
#' @section Filtering and collapse:
#' [extractTs()] always emits **every** compilation's csm, never a subset.
#' [collapseTs()] writes back exactly what the timeseries carries: it does not
#' consult the stored original, so a timeseries stripped of some csm keys is
#' written without them.
#'
#' Recovering dropped keys from the original was considered and rejected,
#' because it would make it impossible to ever delete a csm field through the
#' timeseries. The consequence is a rule rather than a guard: **filter or rename
#' a timeseries only when you are not going to collapse it.** Editing values in
#' a full timeseries and collapsing is always safe; each compilation is folded
#' into its own entry, so changing one never disturbs another.
#'
#' @name csm
NULL

#' Sanitise a compilation name for use in a flat key
#'
#' Hyphens and underscores are removed so that `<compilation>_csm_<field>` is
#' the only key shape containing two underscores, which makes it unambiguous to
#' parse. `SISAL-LiPD` becomes `SISALLiPD` and `DAMP21k_Lakes` becomes
#' `DAMP21kLakes`.
#'
#' @param x Compilation name.
#' @return A key-safe name.
#' @export
csmCompilationKey <- function(x){
  gsub("[^A-Za-z0-9]", "", as.character(x))
}

#' Expand nested csm into flat, compilation-named keys
#'
#' Turns `inCompilation<i>_csm` (a list) into `<compilation>_csm_<field>`
#' entries. Called by [extractTs()].
#'
#' @param entry A single time series entry.
#' @return The entry, with csm expanded.
#' @keywords internal
expand_csm_entry <- function(entry){
  nm <- names(entry)
  csmKeys <- grep("^inCompilation[0-9]+_csm$", nm, value = TRUE)
  if(length(csmKeys) == 0){
    return(entry)
  }
  for(ck in csmKeys){
    idx <- sub("^inCompilation([0-9]+)_csm$", "\\1", ck)
    cname <- entry[[paste0("inCompilation", idx, "_compilationName")]]
    vals <- entry[[ck]]
    if(is.null(cname) || length(cname) == 0 || !is.list(vals) || length(vals) == 0){
      next
    }
    key <- csmCompilationKey(cname[[1]])
    if(!nzchar(key)){
      next
    }
    for(f in names(vals)){
      if(!nzchar(f)) next
      entry[[paste0(key, "_csm_", f)]] <- vals[[f]]
    }
    # Drop the nested form so there is a single representation in the flat
    # timeseries. collapseTs() rebuilds it.
    entry[[ck]] <- NULL
  }
  return(entry)
}

#' Fold flat csm keys back into the nested inCompilation structure
#'
#' The inverse of [expand_csm_entry()]. Called by [collapseTs()].
#'
#' Each value is folded into the `inCompilation` entry whose `compilationName`
#' matches, so editing one compilation's metadata never disturbs another's.
#'
#' This writes exactly what the entry carries. It does not consult the stored
#' original, so csm dropped from the timeseries is written back missing — see
#' the note on filtering in [csm].
#'
#' A flat key naming a compilation with no membership entry is left in place and
#' warned about rather than dropped: the sanitised key cannot be inverted to the
#' original compilation name, so no entry can be created for it safely.
#'
#' @param entry A single time series entry.
#' @return The entry, with csm nested.
#' @keywords internal
contract_csm_entry <- function(entry){
  nm <- names(entry)
  flat <- grep("^[A-Za-z0-9]+_csm_", nm, value = TRUE)
  if(length(flat) == 0){
    return(entry)
  }

  nameKeys <- grep("^inCompilation[0-9]+_compilationName$", names(entry), value = TRUE)
  lut <- list()
  for(nk in nameKeys){
    idx <- sub("^inCompilation([0-9]+)_compilationName$", "\\1", nk)
    cn <- entry[[nk]]
    if(is.null(cn) || length(cn) == 0) next
    lut[[csmCompilationKey(cn[[1]])]] <- idx
  }

  orphaned <- character()
  for(k in flat){
    comp <- sub("^([A-Za-z0-9]+)_csm_.*$", "\\1", k)
    fld  <- sub("^[A-Za-z0-9]+_csm_", "", k)
    idx  <- lut[[comp]]
    if(is.null(idx)){
      orphaned <- c(orphaned, k)
      next
    }
    tgt <- paste0("inCompilation", idx, "_csm")
    cur <- entry[[tgt]]
    if(!is.list(cur)) cur <- list()
    cur[[fld]] <- entry[[k]]
    entry[[tgt]] <- cur
    entry[[k]] <- NULL
  }

  if(length(orphaned) > 0){
    warning(paste0("csm keys with no matching compilation membership, left unfolded: ",
                   paste(orphaned, collapse = ", ")), call. = FALSE)
  }
  return(entry)
}

#' Expand csm across a whole time series
#' @param ts A time series list.
#' @return The time series, with csm expanded.
#' @keywords internal
expand_csm <- function(ts){
  if(length(ts) == 0) return(ts)
  for(i in seq_along(ts)){
    ts[[i]] <- expand_csm_entry(ts[[i]])
  }
  return(ts)
}

#' List the compilation-specific metadata present in a time series
#'
#' @param ts A `lipd_ts` list or a `lipd_ts_tibble`.
#' @return A data.frame of `compilation`, `field`, `key` and the number of
#'   entries carrying it.
#' @export
#' @examples
#' \dontrun{
#' ts <- extractTs(readLipd())
#' csmFields(ts)
#' }
csmFields <- function(ts){
  tsList <- as.lipdTs(ts)
  keys <- unlist(lapply(tsList, function(e) grep("^[A-Za-z0-9]+_csm_", names(e), value = TRUE)))
  if(length(keys) == 0){
    return(data.frame(compilation = character(), field = character(),
                      key = character(), n = integer(), stringsAsFactors = FALSE))
  }
  tab <- table(keys)
  out <- data.frame(key = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  out$compilation <- sub("^([A-Za-z0-9]+)_csm_.*$", "\\1", out$key)
  out$field <- sub("^[A-Za-z0-9]+_csm_", "", out$key)
  out <- out[order(out$compilation, out$field), c("compilation", "field", "key", "n")]
  rownames(out) <- NULL
  return(out)
}
