# Test script for all changelog-related functions

context("Changelog initialization and simple updates")

test_that("initializeChangelog works correctly", {
  L <- create_test_lipd_object()
  L$changelog <- NULL # Remove default changelog for testing

  # Initialize it
  L_init <- initializeChangelog(L)

  expect_length(L_init$changelog, 1)
  expect_equal(L_init$changelog[[1]]$version, "1.0.0")
  expect_equal(L_init$changelog[[1]]$notes, "Starting the changelog")

  # It should not re-initialize
  L_reinit <- suppressWarnings(initializeChangelog(L_init))
  expect_warning(initializeChangelog(L_init))
  expect_equal(length(L_reinit$changelog), 1) # Should not have added another entry
})

test_that("getVersion, getTimestamp, and getChangelog work", {
  L <- create_test_lipd_object() # This helper now creates a changelog

  # getVersion
  expect_equal(getVersion(L), "1.0.0")

  # getTimestamp
  expect_true(is.character(getTimestamp(L)))
  expect_true(nchar(getTimestamp(L)) > 0)

  # getChangelog
  newest_log <- getChangelog(L, "newest")
  expect_equal(newest_log$version, "1.0.0")

  # Test with an updated object
  L_updated <- L
  L_updated$changelog <- append(list(list(version = "1.0.1", timestamp = "2024-01-01T12:00:00Z")), L_updated$changelog)
  expect_equal(getVersion(L_updated), "1.0.1")
  expect_equal(getChangelog(L_updated, "newest")$version, "1.0.1")
  expect_equal(getChangelog(L_updated, "oldest")$version, "1.0.0")
})

context("createChangelog and updateChangelog workflow")

test_that("createChangelog detects metadata changes", {
  L_old <- create_test_lipd_object()

  # 1. Change root metadata
  L_new1 <- L_old
  L_new1$dataSetName <- "NewDSN"

  cl1 <- createChangelog(L_old, L_new1)
  expect_equal(nrow(cl1), 1)
  expect_equal(cl1$type, "Base metadata")
  expect_true(grepl("'TestDSN' has been replaced by 'NewDSN'", cl1$change))

  # 2. Change geo metadata
  L_new2 <- L_old
  L_new2$geo$latitude <- 41.0

  cl2 <- createChangelog(L_old, L_new2)
  expect_equal(nrow(cl2), 1)
  expect_equal(cl2$type, "Geographic metadata")
  expect_true(grepl("'40' has been replaced by '41'", cl2$change))
})

test_that("createChangelog detects paleoData column changes", {
  L_old <- create_test_lipd_object()

  # 1. Add a new column
  L_new1 <- L_old
  L_new1$paleoData[[1]]$measurementTable[[1]]$salinity <- list(
    "variableName" = "salinity", "values" = 35:39, "units" = "psu", "TSid" = "NewTSID"
  )
  cl1 <- createChangelog(L_old, L_new1)
  expect_true(grepl("Column 'NewTSID', with variable name 'salinity', was added", cl1$change))

  # 2. Remove a column
  L_new2 <- L_old
  L_new2$paleoData[[1]]$measurementTable[[1]]$temp <- NULL
  cl2 <- createChangelog(L_old, L_new2)
  expect_true(grepl("Column 'tsid-temp-TestDSN', with variable name 'temp', was removed", cl2$change))

  # 3. Change a column's metadata (units)
  L_new3 <- L_old
  L_new3$paleoData[[1]]$measurementTable[[1]]$temp$units <- "Kelvin"
  cl3 <- createChangelog(L_old, L_new3)
  expect_equal(nrow(cl3), 1)
  expect_true(grepl("temp (tsid-temp-TestDSN): paleoData_units: 'degC' has been replaced by 'Kelvin'", cl3$change, fixed = TRUE))
})

test_that("createChangelog detects paleoData value changes", {
  L_old <- create_test_lipd_object()

  L_new <- L_old
  L_new$paleoData[[1]]$measurementTable[[1]]$temp$values <- c(25, 26, 27, 28, 29)

  cl <- createChangelog(L_old, L_new)
  expect_equal(nrow(cl), 1)
  expect_equal(cl$type, "PaleoData values")
  expect_true(grepl("The paleoData_values have changed", cl$change))
})

test_that("updateChangelog correctly appends and increments version", {
  L_old <- create_test_lipd_object()
  L_new <- L_old
  L_new$geo$siteName <- "A New Site"

  # Create a changelog based on the difference
  changes <- createChangelog(L_old, L_new)
  expect_equal(nrow(changes), 1)

  # Update the original object with these changes
  L_updated <- updateChangelog(L_old, changes)

  # Check the result
  expect_length(L_updated$changelog, 2)

  # Newest entry should be at the top
  new_entry <- L_updated$changelog[[1]]
  expect_equal(new_entry$version, "1.0.1") # Patch increment
  expect_equal(new_entry$lastVersion, "1.0.0")
  expect_true(is.list(new_entry$changes))
  expect_true("Geographic metadata" %in% names(new_entry$changes))

  # Test minor version increment (for value changes)
  L_new_vals <- L_old
  L_new_vals$paleoData[[1]]$measurementTable[[1]]$temp$values[1] <- 99
  changes_vals <- createChangelog(L_old, L_new_vals)
  L_updated_vals <- updateChangelog(L_old, changes_vals)
  expect_equal(L_updated_vals$changelog[[1]]$version, "1.1.0") # Minor increment
})
