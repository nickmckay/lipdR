test_that("a file with no version key is assumed current, without prompting", {
  # The old code called readline() here. Under R CMD check stdin is empty, so
  # this test would hang or fall through to NA rather than fail loudly.
  r <- get_lipd_version(list(dataSetName = "x"))
  expect_equal(r$version, 1.3)
  expect_equal(r$meta$lipdVersion, 1.3)
})

test_that("an explicit version is kept, whichever key spelling it uses", {
  for (k in c("lipdVersion", "liPDVersion", "LiPDVersion")) {
    d <- list(dataSetName = "x"); d[[k]] <- 1.2
    r <- get_lipd_version(d)
    expect_equal(r$version, 1.2)
    # The key is normalised to lipdVersion and the variant dropped.
    expect_equal(r$meta$lipdVersion, 1.2)
    if (k != "lipdVersion") expect_null(r$meta[[k]])
  }
})

test_that("a version given as a string still parses", {
  r <- get_lipd_version(list(lipdVersion = "1.3"))
  expect_equal(r$version, 1.3)
})

test_that("an unrecognised version warns rather than printing", {
  expect_warning(get_lipd_version(list(lipdVersion = 2.5)), "invalid")
})

test_that("a real file round trips with a concrete version, needing no input", {
  f <- test_path("ODP1098B13.lpd")
  skip_if_not(file.exists(f))
  L <- suppressWarnings(readLipd(f))
  expect_false(is.null(L$lipdVersion))
  expect_false(is.na(L$lipdVersion))
  expect_true(L$lipdVersion %in% LIPD_VERSIONS)

  # And survives a write, which is where an NA version used to land in the file.
  d <- withr::local_tempdir()
  suppressWarnings(writeLipd(L, path = d, removeNamesFromLists = TRUE))
  B <- suppressWarnings(readLipd(file.path(d, basename(f))))
  expect_equal(B$lipdVersion, L$lipdVersion)
})
