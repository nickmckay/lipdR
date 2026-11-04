web <- "https://lipdverse.org/data/abc"

fake_reader <- function(version_txt = NULL, page = NULL){
  function(url){
    out <- if(endsWith(url,"version.txt")) version_txt else page
    if(is.null(out)) stop("cannot open the connection")
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
