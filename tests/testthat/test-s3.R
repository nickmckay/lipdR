# Test script for S3 methods and classes

# Helper function to create a basic, valid LiPD object for testing
create_mock_lipd <- function(dsn = "TestDSN", id = "TestID") {
  L <- list(
    "dataSetName" = dsn,
    "datasetId" = id,
    "archiveType" = "marine sediment",
    "lipdVersion" = 1.3,
    "geo" = list(
      "latitude" = 0,
      "longitude" = 0,
      "siteName" = "Test Site"
    ),
    "paleoData" = list(
      list(
        "measurementTable" = list(
          list(
            "tableName" = "P1M1",
            "age" = list(
              "variableName" = "age",
              "values" = 1:5,
              "units" = "yr BP",
              "TSid" = "ID001"
            ),
            "temp" = list(
              "variableName" = "temp",
              "values" = 20:24,
              "units" = "degC",
              "TSid" = "ID002"
            )
          )
        )
      )
    )
  )
  return(new_lipd(L))
}


test_that("is.* functions correctly identify classes", {
  L <- create_mock_lipd()
  D <- new_multiLipd(list("TestDSN" = L))
  ts <- extractTs(L)
  tts <- as.lipdTsTibble(ts)
  ltts <- as.lipdTsTibbleLong(tts)

  expect_true(is.lipd(L))
  expect_false(is.multiLipd(L))

  expect_true(is.multiLipd(D))
  expect_false(is.lipd(D))

  expect_true(is.lipdTs(ts))
  expect_true(is.lipdTsTibble(tts))
  expect_true(is.lipdTsTibbleLong(ltts))

  expect_false(is.lipd(list()))
})

test_that("as.* functions correctly coerce objects", {
  L <- create_mock_lipd()
  ts_from_l <- as.lipdTs(L)
  expect_s3_class(ts_from_l, "lipd_ts")
  expect_length(ts_from_l, 2) # age and temp

  tib_from_ts <- as.lipdTsTibble(ts_from_l)
  expect_s3_class(tib_from_ts, "lipd_ts_tibble")
  expect_equal(nrow(tib_from_ts), 2)

  long_from_tib <- as.lipdTsTibbleLong(tib_from_ts)
  expect_s3_class(long_from_tib, "lipd_ts_tibble_long")
  # one row per value, but only for non-age vars
  expect_equal(nrow(long_from_tib), 10)

  # Test round trip
  ts_from_long <- untidyTs(long_from_tib)
  expect_s3_class(ts_from_long, "lipd_ts")

  # Note: exact object equality is hard after all the transformations
  # Instead, check for key content preservation
  expect_equal(length(ts_from_long), 2)
  expect_equal(ts_from_long[[2]]$paleoData_variableName, "temp")

})


test_that("print and summary methods run without errors", {
  L <- create_mock_lipd()
  D <- new_multiLipd(list("TestDSN" = L, "TestDSN2" = create_mock_lipd("DSN2", "ID2")))
  ts <- extractTs(D)

  # Capture output to avoid printing to console during tests
  expect_no_error(print(L))
  expect_no_error(summary(L))

  summary_out <- capture.output(summary(L))
  expect_true(any(grepl("### Paleo Data ###", summary_out)))
  expect_true(any(grepl("age", summary_out)))
  expect_true(any(grepl("temp", summary_out)))

  expect_no_error(print(D))
  expect_no_error(summary(D))
  summary_out_multi <- capture.output(summary(D))
  expect_true(any(grepl("Multi LiPD contains 2 LiPD files", summary_out_multi)))

  expect_no_error(print(ts))
  expect_no_error(summary(ts))
})

test_that("as.lipd and as.multiLipd work correctly", {
  ts <- extractTs(create_mock_lipd())
  L_collapsed <- as.lipd(ts)
  expect_s3_class(L_collapsed, "lipd")
  expect_equal(L_collapsed$dataSetName, "TestDSN")

  # create a ts from two lipd files
  L1 <- create_mock_lipd("DSN1", "ID1")
  L2 <- create_mock_lipd("DSN2", "ID2")
  D <- new_multiLipd(list("DSN1" = L1, "DSN2" = L2))
  ts_multi <- extractTs(D)

  D_collapsed <- as.multiLipd(ts_multi)
  expect_s3_class(D_collapsed, "multi_lipd")
  expect_length(D_collapsed, 2)
  expect_named(D_collapsed, c("DSN1", "DSN2"))
})
