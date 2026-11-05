web <- "https://lipdverse.org/data/abc"

url_condition <- function(class, message = class){
  structure(class = c(class,"error","condition"), list(message = message, call = NULL))
}

#a missing file is a 404, the way read_url_lines() reports one
fake_reader <- function(version_txt = NULL, page = NULL){
  function(url){
    out <- if(endsWith(url,"version.txt")) version_txt else page
    if(is.null(out)) stop(url_condition("url_not_found","HTTP status was '404 Not Found'"))
    out
  }
}

test_that("version.txt is used when it is there", {
  local_mocked_bindings(read_url_lines = fake_reader("1_0_12\n"))
  expect_equal(get_dsid_version(web),"1_0_12")
  expect_equal(convert_dsid_to_path("abc"),file.path(web,"1_0_12","lipd.lpd"))
})

test_that("a missing version.txt falls back on the dataset page", {
  local_mocked_bindings(read_url_lines = fake_reader(page = c("<html>","url=1_0_3/index.html","</html>")))
  expect_equal(get_dsid_version(web),"1_0_3")
})

test_that("a version.txt that is really a page is not trusted", {
  local_mocked_bindings(read_url_lines = fake_reader(version_txt = c("<html>","shell 0_6_3"),
                                                     page = "url=2_1_0/index.html"))
  expect_equal(get_dsid_version(web),"2_1_0")
})

test_that("no version anywhere is an error, not an NA path", {
  local_mocked_bindings(read_url_lines = fake_reader(page = "<html>no version here</html>"))
  expect_error(get_dsid_version(web),"no version found")
  expect_error(convert_dsid_to_path("abc"),"no version found")
})

test_that("an unreachable dataset is an error", {
  local_mocked_bindings(read_url_lines = fake_reader())
  expect_error(get_dsid_version(web),"Can't get dataset version")
})

test_that("a version that was asked for skips the lookup", {
  local_mocked_bindings(read_url_lines = function(url) stop("should not be called"))
  expect_equal(convert_dsid_to_path("abc","1_0_0"),file.path(web,"1_0_0","lipd.lpd"))
})

test_that("only a 404 on version.txt falls back on the page, which can be older", {
  for(class in c("url_transient","url_error")){
    local_mocked_bindings(read_url_lines = function(url){
      if(endsWith(url,"version.txt")) stop(url_condition(class))
      "url=1_0_3/index.html"
    })
    expect_error(get_dsid_version(web),class = class)
  }
})

test_that("url failures are classified from the message R gives", {
  expect_equal(url_error_class("cannot open URL 'x': HTTP status was '404 Not Found'"),"url_not_found")
  expect_equal(url_error_class("cannot open URL 'x': HTTP status was '503 Service Unavailable'"),"url_transient")
  expect_equal(url_error_class("cannot open URL 'x': HTTP status was '429 Unknown Error'"),"url_transient")
  expect_equal(url_error_class("cannot open URL 'x': HTTP status was '403 Forbidden'"),"url_error")
  expect_equal(url_error_class("URL 'x': status was 'SSL peer certificate or SSH remote key was not OK'"),"url_error")
  expect_equal(url_error_class("URL 'x': status was 'Couldn't resolve host name'"),"url_error")
  expect_equal(url_error_class("URL 'x': status was 'Timeout was reached'"),"url_transient")
  #digits in a url aren't a status
  expect_equal(url_error_class("URL 'https://x/503/y': status was 'SSL peer certificate or SSH remote key was not OK'"),"url_error")
})

test_that("temporary problems are retried, and others fail straight away", {
  pauses <- 0
  count_pauses <- function(s) pauses <<- pauses + 1

  tries <- 0
  flaky <- function(){
    tries <<- tries + 1
    if(tries < 3) stop(url_condition("url_transient"))
    "ok"
  }
  expect_equal(retry_transient(flaky,sleep = count_pauses)(),"ok")
  expect_equal(tries,3)
  expect_equal(pauses,2)

  tries <- 0
  always <- function(){
    tries <<- tries + 1
    stop(url_condition("url_transient"))
  }
  expect_error(retry_transient(always,max_times = 4,sleep = count_pauses)(),class = "url_transient")
  expect_equal(tries,4)

  tries <- 0
  permanent <- function(){
    tries <<- tries + 1
    stop(url_condition("url_error"))
  }
  expect_error(retry_transient(permanent,sleep = stop)(),class = "url_error")
  expect_equal(tries,1)
})

test_that("failed lookups are left out and named", {
  local_mocked_bindings(convert_dsid_to_path = function(dsid,vers = NA){
    if(dsid == "bad") stop(url_condition("url_error","bad certificate"))
    paste0("path/",dsid)
  })
  output <- capture.output(paths <- lookup_dsid_paths(c("a","bad","b")))
  expect_equal(paths,c("path/a","path/b"))
  expect_match(paste(output,collapse = " "),"bad")
  expect_match(paste(output,collapse = " "),"bad certificate")
})

test_that("if every lookup fails it stops with the reason", {
  local_mocked_bindings(convert_dsid_to_path = function(dsid,vers = NA){
    stop(url_condition("url_error","bad certificate"))
  })
  expect_error(lookup_dsid_paths(c("a","b")),"any of the 2 datasetIds.*bad certificate")
})

test_that("a data.frame of datasetIds doesn't need a version column", {
  local_mocked_bindings(convert_dsid_to_path = function(dsid,vers = NA) paste0("path/",dsid,".lpd"))
  df <- data.frame(datasetId = c("a","b","c"),other = 1:3)
  expect_equal(suppressMessages(get_src_or_dst(df)),c("path/a.lpd","path/b.lpd","path/c.lpd"))
})
