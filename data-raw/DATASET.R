## code to prepare `DATASET` dataset goes here

if (!requireNamespace("googlesheets4", quietly = TRUE)) {
  stop(
    "Package 'googlesheets4' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
    call. = FALSE
  )
}

if (!requireNamespace("usethis", quietly = TRUE)) {
  stop(
    "Package 'usethis' must be installed to use this function. Install it from github using `remotes::install_github('neotomadb/neotoma2')`",
    call. = FALSE
  )
}

#neotoma conversions
nc <- googlesheets4::read_sheet("1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q")
cconv <- googlesheets4::read_sheet(ss = "1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q",sheet = "chronColumns")



#Get past thesaurus

getPastDataframe <- function(){
  past_rdf <- rdflib::rdf_parse("https://www.ncei.noaa.gov/access/paleo-search/skos/past-thesaurus.rdf")
  out <- tempfile("file", fileext = ".json")
  jweb <- rdflib::rdf_serialize(rdf = past_rdf, doc = out, format = "jsonld")
  PaST <- jsonlite::read_json(out)
  PaST <- PaST$`@graph`

  allIdUrls <- purrr::map_chr(PaST,"@id")
  ids <- stringr::str_extract(allIdUrls,"\\d{1,}")

  definitions <- purrr::map_chr(PaST,\(x){y <- x$`http://www.w3.org/2004/02/skos/core#definition`$`@value`; ifelse(is.null(y),NA,y)})

  prefLabel <- purrr::map_chr(PaST,\(x){y <- x$`http://www.w3.org/2004/02/skos/core#prefLabel`$`@value`; ifelse(is.null(y),NA,y)})

  #related <- purrr::map_chr(PaST,\(x){y <- x$`http://www.w3.org/2004/02/skos/core#related`$`@id`; ifelse(is.null(y),NA,y)})


  #past <- data.frame(id = ids, url = allIdUrls,name = prefLabel, definition = definitions, related = related)


  past <- data.frame(id = ids, url = allIdUrls,name = prefLabel, definition = definitions)

  return(past)
}


past <- getPastDataframe()
usethis::use_data(past,nc,cconv,overwrite = FALSE)
