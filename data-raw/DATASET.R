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

# badChecks <- data.frame(col = NA,
#                         row=NA)
# imax<-ncol(queryTable) * nrow(queryTable)
# pb <- txtProgressBar(min = 0, max = imax, style = 3)
# its <- 0
# for (i in 1:ncol(queryTable)){
#   for (j in 1:nrow(queryTable)){
#     its <- its+1
#     setTxtProgressBar(pb, its)
#     flagNonASCII <- grepl("[^ -~]", queryTable[j,i])
#     if (flagNonASCII){
#       badChecks <- rbind(badChecks, c(i,j))
#
#     }
#   }
# }
# queryTable[397,13]
