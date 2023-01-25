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


nc <- googlesheets4::read_sheet("1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q")

cconv <- googlesheets4::read_sheet(ss = "1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q",sheet = "chronColumns")
usethis::use_data(nc,cconv,queryTable, overwrite = TRUE,internal = TRUE)



#Download and use the queryTable

query_url <- "https://github.com/DaveEdge1/lipdverseQuery/raw/main/queryZip.zip"
temp <- tempdir()
zip_dir <- paste0(temp, "/queryTable.zip")
download.file(query_url, zip_dir)
unzip(zip_dir, exdir = temp)
fPth <- paste0(temp, "/queryTable.csv")
queryTable <- read.csv(fPth)
usethis::use_data(queryTable, overwrite = TRUE, compress = "xz")

#Get the query zip file MD5 sums
ZIPmd5Remote <- readLines("https://raw.githubusercontent.com/DaveEdge1/lipdverseQuery/main/ZIPmd5.txt")
ZIPmd5Local <- ZIPmd5Remote
usethis::use_data(ZIPmd5Local, overwrite = TRUE)

#Get standardization tables for lipd keys

#consider using this directory to get all the standardization tables:
allKeys <- googlesheets4::read_sheet("16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY")


variableName <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18KBNY_x6lZ90k_NF_Cw-6RZ6VhMRR97bzXy49qtq6IU/edit#gid=1697518669")
archiveType <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/16OxSagfVVp7KO3jrbjh5npWDNVOMCIZr4ToVgHvZgJE/edit#gid=253751289")
seasonality <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1se8aXqKjlgh6HSGCVnG-nRQuPH3Jts8eNj439FrvvE4/edit#gid=874608133")
interpretation <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qwewgHin2YLVkZS9E66i6E8A7EBrm9VKj3y-vyBYgCs/edit#gid=400551674")
proxy <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-SonhUl_yhZRnmBDDACY9sByl7jt-Ov5b6n21PXPzXQ/edit#gid=279748030")
units <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a_QLvT-im7RZmW-vpJg-RnE4Ecu500pc9fua5u1R1zc/edit#gid=1220915696")
standardTables <- list("paleoData_variableName" = variableName, "paleoData_proxy" = proxy, "paleoData_units" = units,
"seasonality" = seasonality, "archiveType" = archiveType, "interpretation1_variable" = interpretation)
usethis::use_data(standardTables, overwrite = TRUE)

#Get past thesaurus

getPastDataframe <- function(filename = "past.json"){
  PaST <- jsonlite::read_json(filename)
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

past_rdf <- rdflib::rdf_parse("https://www.ncei.noaa.gov/access/paleo-search/skos/past-thesaurus.rdf")
out <- tempfile("file", fileext = ".json")
rdflib::rdf_serialize(rdf = past_rdf, doc = out, format = "jsonld")
past <- getPastDataframe(out)
usethis::use_data(past)
