# Test script for readLipd, writeLipd, and related helpers

test_that("writeLipd and readLipd perform a perfect round trip for a single file", {
  # 1. Create a LiPD object in R
  L_original <- create_test_lipd_object("RoundTrip1")

  # 2. Setup a temporary directory for this test
  temp_dir <- tempfile()
  dir.create(temp_dir)
  # Ensure cleanup, even if tests fail
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Define the path for the new .lpd file
  lipd_path <- file.path(temp_dir, "RoundTrip1.lpd")

  # 3. Write the LiPD object to the temporary file
  # Use suppressMessages to keep test output clean
  suppressMessages(
    writeLipd(L_original, path = lipd_path)
  )

  # Check that the file was actually created
  expect_true(file.exists(lipd_path))

  # 4. Read the file back in
  L_read <- suppressMessages(readLipd(lipd_path))

  # 5. Compare the read object to the original
  # Use all.equal as it gives more informative failures than identical()
  # and handles minor numeric precision differences if they were to occur.
  # We remove the 'savedEnsembles' field as this is a transient state
  # added during read and not part of the core data.
  L_read$savedEnsembles <- NULL

  #sort them both by alphabetical order:
  L_original <- L_original[order(names(L_original))]
  L_read <- L_read[order(names(L_read))]
  expect_setequal(L_read, L_original)
})

test_that("readLipd correctly reads a directory of LiPD files", {
  # 1. Create multiple LiPD objects
  L1 <- create_test_lipd_object("MultiRead1")
  L2 <- create_test_lipd_object("MultiRead2")

  # 2. Setup a temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # 3. Write both files to the directory
  suppressMessages({
    writeLipd(L1, path = temp_dir)
    writeLipd(L2, path = temp_dir)
  })

  # Check that files were created
  expect_true(file.exists(file.path(temp_dir, "MultiRead1.lpd")))
  expect_true(file.exists(file.path(temp_dir, "MultiRead2.lpd")))

  # 4. Read the entire directory
  D_read <- suppressMessages(readLipd(temp_dir))

  # 5. Verify the output
  expect_s3_class(D_read, "multi_lipd")
  expect_length(D_read, 2)
  expect_named(D_read, c("MultiRead1", "MultiRead2"))

  # Check one of the loaded objects for correctness
  D_read$MultiRead1$savedEnsembles <- NULL
  expect_setequal(D_read$MultiRead1, L1)
})

test_that("readLipd fails gracefully on bad path", {
  bad_path <- file.path(tempdir(), "this_file_does_not_exist.lpd")
  # Ensure file doesn't exist
  if(file.exists(bad_path)) {
    unlink(bad_path)
  }
  expect_error(readLipd(bad_path), "does not exist")
})

test_that("don.load.ensemble option works as expected", {
  # This tests the ensemble handling logic
  L_orig <- create_test_lipd_object("EnsembleTest")
  # Add a mock ensemble table to the object
  L_orig$chronData[[1]]$model <- list(list(
    "ensembleTable" = list(list(
      "values" = list(
        "variableName" = "ageEnsemble",
        "values" = matrix(1:10, 5, 2) # matrix indicates ensemble
      )
    ))
  ))

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  lipd_path <- file.path(temp_dir, "EnsembleTest.lpd")

  # Write the file
  suppressMessages(writeLipd(L_orig, path = lipd_path))

  # Read with dont.load.ensemble = TRUE
  L_read_no_ens <- suppressMessages(
    readLipd(lipd_path, dont.load.ensemble = TRUE)
  )

  # Check that the ensemble was NOT loaded into the object
  expect_null(L_read_no_ens$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values)
  # Check that the path to the saved ensembles was stored
  expect_true(is.character(L_read_no_ens$savedEnsembles))
  expect_true(dir.exists(L_read_no_ens$savedEnsembles))

  # Read with dont.load.ensemble = TRUE
  L_read_yes_ens <- suppressMessages(
    readLipd(lipd_path, dont.load.ensemble = FALSE)
  )

  # Check that the ensemble WAS loaded into the object
  expect_true(is.matrix(L_read_yes_ens$chronData[[1]]$model[[1]]$ensembleTable[[1]]$ageEnsemble$values))
  # Check that the path to the saved ensembles was not stored
  expect_null(L_read_yes_ens$savedEnsembles)
})
