test_that("downloads get at least an hour, and the timeout is restored afterwards", {
  withr::local_options(timeout = 60)
  seen <- NULL
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    seen <<- getOption("timeout")
    0L
  })
  download_with_timeout("https://example.org/a.zip", tempfile())
  expect_equal(seen, lipd_download_timeout)
  expect_equal(getOption("timeout"), 60)
})

test_that("a longer timeout set by the user is kept", {
  withr::local_options(timeout = 7200)
  seen <- NULL
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    seen <<- getOption("timeout")
    0L
  })
  download_with_timeout("https://example.org/a.zip", tempfile())
  expect_equal(seen, 7200)
})

test_that("downloads use libcurl, on Windows too", {
  skip_if_not(isTRUE(capabilities("libcurl")))
  seen <- NULL
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    seen <<- method
    0L
  })
  download_with_timeout("https://example.org/a.zip", tempfile())
  expect_equal(seen, "libcurl")
})

test_that("a timeout says how to allow longer, and keeps the real reason", {
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    warning("downloaded length 1000 != reported length 285000000")
    warning("Timeout of 3600 seconds was reached")
    stop("download from 'https://example.org/a.zip' failed")
  })
  err <- expect_error(download_with_timeout("https://example.org/a.zip", tempfile()))
  expect_match(conditionMessage(err), "Timeout of 3600 seconds was reached", fixed = TRUE)
  expect_match(conditionMessage(err), "options(timeout", fixed = TRUE)
})

test_that("a revocation failure names the Windows workaround", {
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    warning("URL 'https://lipdverse.org/a.lpd': status was 'SSL connect error' schannel: next InitializeSecurityContext failed: CRYPT_E_REVOCATION_OFFLINE (0x80092013) - The revocation function was unable to check revocation because the revocation server was offline.")
    stop("cannot open URL 'https://lipdverse.org/a.lpd'")
  })
  err <- expect_error(download_with_timeout("https://lipdverse.org/a.lpd", tempfile()))
  expect_match(conditionMessage(err), "CRYPT_E_REVOCATION_OFFLINE", fixed = TRUE)
  expect_match(conditionMessage(err), "R_LIBCURL_SSL_REVOKE_BEST_EFFORT", fixed = TRUE)
  expect_no_match(conditionMessage(err), "options(timeout", fixed = TRUE)
})

test_that("a non-zero status from download.file() is an error", {
  local_mocked_bindings(do_download = function(url, destfile, method, quiet) 22L)
  expect_error(download_with_timeout("https://example.org/a.zip", tempfile()), "status 22")
})

test_that("a failed single-file download still returns download-error, with the reason shown", {
  local_mocked_bindings(do_download = function(url, destfile, method, quiet){
    stop("cannot open URL")
  })
  expect_message(out <- download_from_url("https://example.org/data/X/1_0_0/Site.Author.2020.lpd"),
                 "Couldn't download")
  expect_equal(out, "download-error")
})
