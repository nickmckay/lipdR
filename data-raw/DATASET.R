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

#delete current queryTable in queryTable.rda
#Install the project
#run this code to rebuild the rda file
# usethis::use_data(queryTable)
# tools::resaveRdaFiles("data/")

nc <- googlesheets4::read_sheet("1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q")

cconv <- googlesheets4::read_sheet(ss = "1Z44xjSxEDlWnThvYLsHFS9aFAN0FnYh2EdroMs9qe_Q",sheet = "chronColumns")
usethis::use_data(nc,cconv,queryTable, overwrite = TRUE,internal = TRUE)
