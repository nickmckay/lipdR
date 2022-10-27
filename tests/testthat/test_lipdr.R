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

test_lipdSummary <- function(path){
  tryCatch({
    L <- readLipd(path)
    lipdSummary(L)
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}

test_lipdreadwrite <- function(path){
  tryCatch({
    D <- readLipd(path)
    writeLipd(D,path = file.path(tempdir(),"test.lpd"))
    writeLipd(D,path = file.path(tempdir()))
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}

test_lipdreaddsid <- function(){
  tryCatch({
    L <- readLipd("GkDQ95n4IDauHjdPWhtP")
    if(!lipdR:::validLipd(L)){
      stop("Single readLipd DSID fail")
    }
    D <- readLipd(c("GkDQ95n4IDauHjdPWhtP","PD0bcb3d51ff3e498d04b365456415c132"))
    if(!all(purrr::map(D,lipdR:::validLipd))){
      stop("Multi readLipd DSID fail")
    }
    return(0)
  }, error=function(cond){
    print(cond)
    return(-1)
  })
}

test_lipdreadurls <- function(){
  tryCatch({
    L <- readLipd("https://lipdverse.org/data/PD0bcb3d51ff3e498d04b365456415c132/1_0_3/LittleSaltSpring.Clausen.1979.lpd")
    if(!lipdR:::validLipd(L)){
      stop("Single readLipd DSID fail")
    }
    D <- readLipd(c("https://lipdverse.org/data/PD0bcb3d51ff3e498d04b365456415c132/1_0_3/LittleSaltSpring.Clausen.1979.lpd","https://lipdverse.org/data/AfgsdvijWJwsyaa8lZLA/1_0_1/Lygne.Hardeng.2022.lpd"))
    if(!all(purrr::map(D,lipdR:::validLipd))){
      stop("Multi readLipd DSID fail")
    }
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
  tryCatch({
    L <- readLipd(path)
    ts <- extractTs(L)
    tts <- ts2tibble(ts)
    ids <- tts$paleoData_TSid
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

test_that("LiPD Read: by 1 or more datasetId", {
  print("LiPD Read: by 1 or more datasetId")
  sink("log")
  expect_equal(test_lipdreaddsid(), 0)
  sink()
})


test_that("LiPD Summary: v1.3 with all table types", {
  print("LiPD Summary: v1.3 with all table types")
  sink("log")
  expect_equal(test_lipdSummary("./ODP1098B13.lpd"), 0)
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
  expect_equal(test_lipdreadwrite("./ODP1098B13.lpd"), 0)
  sink()
})



