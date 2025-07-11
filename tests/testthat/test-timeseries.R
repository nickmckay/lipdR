# Test script for extractTs, collapseTs, filterTs, queryTs
test_that("extractTs creates a valid ts object from a single LiPD", {
  L <- create_test_lipd_object()
  ts <- extractTs(L, mode = "paleo")

  expect_s3_class(ts, "lipd_ts")
  # paleoData has 2 variables: age, temp
  expect_length(ts, 2)

  # Check content of one TS object
  temp_ts_obj <- ts[[2]] # should be 'temp'
  expect_equal(temp_ts_obj$paleoData_variableName, "temp")
  expect_equal(temp_ts_obj$paleoData_values, 20:24)
  expect_equal(temp_ts_obj$dataSetName, "TestDSN")
  expect_equal(temp_ts_obj$age, 1:5) # special columns are extracted
})

test_that("collapseTs correctly reconstructs a LiPD object with multiple modifications", {
  # 1. Create a base object
  L_orig <- create_test_lipd_object()

  # 2. Add an interpretation block to the original object for testing
  L_orig$paleoData[[1]]$measurementTable[[1]]$temp$interpretation <- list(
    list(variable = "mean annual temperature", scope = "climate")
  )

  # 3. Extract to a time series object
  # extractTs populates the lipdEnv, which collapseTs needs
  ts <- extractTs(L_orig)

  # 4. Modify various fields in the time series
  # Note: For dataset-level metadata (like createdBy, geo, pub), the change must be
  # made to all TS objects from that dataset to be consistently applied.

  # Modify root-level metadata
  ts[[1]]$createdBy <- "NewEditor"
  ts[[2]]$createdBy <- "NewEditor"

  # Modify geo metadata
  ts[[1]]$geo_siteName <- "New Site Name"
  ts[[2]]$geo_siteName <- "New Site Name"

  # Modify pub metadata
  ts[[1]]$pub1_year <- 2025
  ts[[2]]$pub1_year <- 2025

  # Modify a core data value in one variable
  ts[[2]]$paleoData_values <- ts[[2]]$paleoData_values + 10 # increase temp

  # Modify interpretation metadata in the same variable
  ts[[2]]$interpretation1_variable <- "MAT"
  ts[[2]]$paleoData_notes <- "Modified by test" # Add a note to the variable

  # 5. Collapse the modified time series back into a LiPD object
  L_collapsed <- suppressMessages(collapseTs(ts))

  # 6. Verify that all modifications have been correctly reconstructed
  expect_s3_class(L_collapsed, "lipd")

  # Check root metadata
  expect_equal(L_collapsed$createdBy, "NewEditor")

  # Check geo metadata
  expect_equal(L_collapsed$geo$siteName, "New Site Name")

  # Check pub metadata
  expect_equal(L_collapsed$pub[[1]]$year, 2025)

  # Check the data and notes modification
  temp_col <- L_collapsed$paleoData[[1]]$measurementTable[[1]]$temp
  expect_equal(temp_col$values, 30:34)
  expect_equal(temp_col$notes, "Modified by test")

  # Check the interpretation modification
  expect_equal(temp_col$interpretation[[1]]$variable, "MAT")

  # Check that other data is still there and unchanged (age column)
  expect_equal(L_collapsed$paleoData[[1]]$measurementTable[[1]]$age$values, 1:5)
})

test_that("filterTs and queryTs work as expected", {
  L1 <- create_test_lipd_object("MarineSed", "ID1")
  L2 <- create_test_lipd_object("LakeSed", "ID2")
  L2$archiveType <- "lake sediment"
  L2$paleoData[[1]]$measurementTable[[1]]$d18O <- list("variableName" = "d18O", "values" = 1:5, "units" = "permil", "TSid" = "ID003")

  D <- new_multiLipd(list("MarineSed" = L1, "LakeSed" = L2))
  ts <- extractTs(D)
  # Total TS objects: 2 from L1 paleo, 3 from L2 paleo = 5
  expect_length(ts, 5)

  # Query for indices
  marine_idx <- queryTs(ts, "archiveType == marine sediment")
  expect_length(unlist(marine_idx), 2)

  d18O_idx <- queryTs(ts, "paleoData_variableName == d18O")
  expect_length(unlist(d18O_idx), 1)

  # Filter for data
  lake_ts <- filterTs(ts, "archiveType == lake sediment")
  expect_length(lake_ts, 3) # 3 from paleo
  expect_equal(lake_ts[[1]]$dataSetName, "LakeSed")

  # Test multiple expressions
  marine_temp_ts <- filterTs(ts, list("archiveType == marine sediment", "paleoData_variableName == temp"))
  expect_length(marine_temp_ts, 1)
  expect_equal(marine_temp_ts[[1]]$paleoData_variableName, "temp")
  expect_equal(marine_temp_ts[[1]]$dataSetName, "MarineSed")

  # Test with no matches
  no_match_ts <- filterTs(ts, "paleoData_variableName == salinity")
  expect_length(no_match_ts, 0)
})

