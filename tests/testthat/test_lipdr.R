context("lipdR")

test_lipdread <- function(path){
  tryCatch({
    D <- readLipd(path)
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}


test_extractTs <- function(path){
  tryCatch({
    L <- readLipd(path)
    ts <- extractTs(L)
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}


test_extractTsCorrectCount <- function(path){
  tryCatch({
    L <- readLipd(path)
    ts <- extractTs(L)
    return(length(ts))
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}

test_extractTsUniqueTsids <- function(path){
  skip("needs to be updated")
  tryCatch({
    L <- readLipd(path)
    ts <- extractTs(L)
    ids <- geoChronR::pullTsVariable(ts, "paleoData_TSid")
    if(any(duplicated(ids))){
      return(-1)
    }
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}

test_that("stripExtension() Works",{ 
  expect_match(stripExtension("~/asd/asd/asdas.lpd"),"asdas")
  expect_match(stripExtension("~/asd/asd/asdas"),"asdas")
  expect_match(stripExtension("~/asd/as.d/as.asdas.lpd"),"asdas")
})  
  

test_that("LiPD Read: v1.3 with all table types", {
  print("LiPD Read: v1.3 with all table types")
  sink("log")
  expect_equal(test_lipdread("./ODP1098B13.lpd"), 0)
  sink()
})

test_that("Time Series with repeated variableNames", {
  print("Time Series with repeated variableNames")
  sink("log")
  expect_equal(test_extractTs("./Carre.Saloum.2018.lpd"), 0)
  sink()
})

test_that("Time Series has correct number of entries", {
  print("Time Series has correct number of entries")
  sink("log")
  expect_equal(test_extractTsCorrectCount("./Carre.Saloum.2018.lpd"), 4)
  sink()
})


test_that("Time Series has unique TSids", {
  print("Time Series has unique TSids")
  sink("log")
  expect_equal(test_extractTsUniqueTsids("./Carre.Saloum.2018.lpd"), 0)
  sink()
})

test_that("LiPD Write: v1.3 with all table types", {
  print("LiPD Write: v1.3 with all table types")
  sink("log")
  # expect_equal(test_lipdwrite("./ODP1098B13.lpd"), 0)
  sink()
})

test_that("Filter Time Series", {
  print("Filter Time Series")
  sink("log")
  # expect_equal(test_filterts("./ODP1098B13.lpd"), 0)
  sink()
})

test_that("Query Time Series", {
  print("Query Time Series")
  sink("log")
  # expect_equal(test_queryts("./ODP1098B13.lpd"), 0)
  sink()
})

