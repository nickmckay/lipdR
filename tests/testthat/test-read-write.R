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

test_that("paleo model ensemble round-trips correctly", {
  L <- create_test_lipd_object("PaleoEnsembleTest")
  n_rows <- 50L
  n_members <- 100L
  ens_matrix <- matrix(rnorm(n_rows * n_members), nrow = n_rows, ncol = n_members)
  depth_vals <- seq_len(n_rows)

  L$paleoData[[1]]$model <- list(list(
    ensembleTable = list(list(
      depth = list(
        variableName = "depth",
        TSid = "TStest_paleo_depth",
        units = "cm",
        values = depth_vals
      ),
      temperature = list(
        variableName = "temperature",
        TSid = "TStest_paleo_temp",
        units = "degC",
        values = ens_matrix
      )
    ))
  ))

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  lipd_path <- file.path(temp_dir, "PaleoEnsembleTest.lpd")

  suppressMessages(writeLipd(L, path = lipd_path))
  expect_true(file.exists(lipd_path))

  L2 <- suppressMessages(readLipd(lipd_path))
  ens2 <- L2$paleoData[[1]]$model[[1]]$ensembleTable[[1]]

  expect_true(is.matrix(ens2$temperature$values))
  expect_equal(dim(ens2$temperature$values), c(n_rows, n_members))
  expect_equal(ens2$temperature$values, ens_matrix)
  expect_equal(ens2$depth$values, depth_vals)
})

# An all-missing column carries no type information in the CSV, so fread types
# it logical. In a LiPD measurement table that is a numeric column with no data,
# and leaving it logical made the file fail validation on a round trip with
# "depth values are not numeric". This affected 14 of the 7,177 files in the
# LiPDverse database.
test_that("an all-NaN numeric column keeps its type through a round trip", {
  d <- withr::local_tempdir()
  L <- create_test_lipd_object()
  n <- length(L$paleoData[[1]]$measurementTable[[1]]$age$values)
  L$paleoData[[1]]$measurementTable[[1]]$depth <- list(
    variableName = "depth", units = "cm", TSid = "tsid-depth-test",
    number = 3, values = rep(NaN, n))

  writeLipd(L, path = d, removeNamesFromLists = TRUE)
  back <- readLipd(file.path(d, "TestDSN.lpd"))
  depth <- back$paleoData[[1]]$measurementTable[[1]]$depth$values

  expect_true(is.numeric(unlist(depth)))
  expect_false(is.logical(unlist(depth)))
  expect_length(unlist(depth), n)
})

test_that("columns with data are unaffected by the all-missing coercion", {
  d <- withr::local_tempdir()
  L <- create_test_lipd_object()
  writeLipd(L, path = d, removeNamesFromLists = TRUE)
  back <- readLipd(file.path(d, "TestDSN.lpd"))
  tb <- back$paleoData[[1]]$measurementTable[[1]]

  expect_equal(unlist(tb$age$values), 1:5)
  expect_equal(unlist(tb$temp$values), 20:24)
})

# merge_csv_columns() decides which CSV columns an entry with no `number` should
# take. It used to compute that from the columns processed so far, so a
# numberless column appearing *before* the column that claims the data was
# handed the whole table -- and the claiming column then took it as well. Paleo
# ensembles doubled in width on every read/write cycle as a result.
test_that("a numberless column does not steal columns claimed later", {
  csvs <- list(c(1, 2), c(3, 4), c(5, 6))
  meta <- list(
    list(variableName = "depth"),                       # no number, listed first
    list(variableName = "ens", number = as.list(1:3))   # claims everything
  )
  out <- lipdR:::merge_csv_columns(csvs, meta)

  expect_null(out[[1]]$values)          # nothing left for it
  expect_null(out[[1]]$number)
  expect_equal(dim(out[[2]]$values), c(2, 3))
})

test_that("a numberless column still takes genuinely unclaimed columns", {
  csvs <- list(c(1, 2), c(3, 4), c(5, 6))
  meta <- list(
    list(variableName = "age", number = 1),
    list(variableName = "ens")            # 2 and 3 are unclaimed
  )
  out <- lipdR:::merge_csv_columns(csvs, meta)

  expect_equal(out[[1]]$values, c(1, 2))
  expect_equal(unlist(out[[2]]$number), c(2, 3))
  expect_equal(dim(out[[2]]$values), c(2, 2))
})

test_that("a single unclaimed column is assigned as a scalar, not a matrix", {
  csvs <- list(c(1, 2), c(3, 4))
  meta <- list(list(variableName = "age", number = 1), list(variableName = "temp"))
  out <- lipdR:::merge_csv_columns(csvs, meta)
  expect_equal(out[[2]]$number, 2)
  expect_equal(out[[2]]$values, c(3, 4))
})

test_that("a paleo ensemble keeps its width through a round trip", {
  d <- withr::local_tempdir()
  L <- create_test_lipd_object()
  ens <- matrix(seq_len(5 * 4), nrow = 5, ncol = 4)
  L$paleoData[[1]]$model <- list(list(ensembleTable = list(list(
    tableName = "paleo1model1ensemble1",
    depth = list(variableName = "depth", TSid = "tsid-ens-depth"),
    vals  = list(variableName = "ensemble", TSid = "tsid-ens-vals",
                 number = as.list(1:4), values = ens)))))

  writeLipd(L, path = d, removeNamesFromLists = TRUE)
  back <- readLipd(file.path(d, "TestDSN.lpd"))
  et <- back$paleoData[[1]]$model[[1]]$ensembleTable[[1]]
  col <- Filter(function(c) is.list(c) && identical(c$variableName, "ensemble"), et)[[1]]
  expect_equal(ncol(col$values), 4)
})
