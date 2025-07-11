# Test script for lipd-neotoma conversions

context("neotoma2lipd and lipd2neotoma conversions")

test_that("neotoma2lipd conversion works correctly", {
  # Skip this entire test if neotoma2 or sf is not installed
  skip_if_not_installed("neotoma2")
  skip_if_not_installed("sf")

  # 1. Create a mock neotoma2 object
  neo_site <- create_mock_neotoma_object()

  # 2. Convert to LiPD
  L_converted <- neotoma2lipd(neo_site)

  # 3. Validate the output
  expect_s3_class(L_converted, "lipd")

  # Check geo metadata
  expect_equal(L_converted$geo$siteName, "Neotoma Test Lake")
  expect_equal(L_converted$geo$latitude, 45)
  expect_equal(L_converted$geo$longitude, -100)
  expect_equal(L_converted$geo$elevation, 1500)
  expect_equal(L_converted$datasetId, "neotomaSiteId_123")

  # Check pub metadata
  expect_equal(L_converted$pub[[1]]$doi, "10.fake/doi")

  # Check paleoData structure
  expect_true("paleoData" %in% names(L_converted))
  paleo_table <- L_converted$paleoData[[1]]$measurementTable[[1]]
  expect_true(is.list(paleo_table))
  expect_true("Pollen" %in% names(paleo_table))
  expect_equal(paleo_table$Pollen$values, c(100, 120))

  # Check chronData structure
  expect_true("chronData" %in% names(L_converted))
  chron_table <- L_converted$chronData[[1]]$measurementTable[[1]]
  expect_true(is.list(chron_table))
  expect_true("age" %in% names(chron_table))
  expect_equal(chron_table$age$values, c(500, 2500))
})


test_that("lipd2neotoma conversion works correctly", {
  # Skip this entire test if neotoma2 or sf is not installed
  skip_if_not_installed("neotoma2")
  skip_if_not_installed("sf")

  # 1. Create a LiPD object
  L <- create_test_lipd_object()

  # 2. Convert to neotoma2 object
  # suppress console output from print() and message()
  neo_converted <- suppressMessages(suppressWarnings(
    lipd2neotoma(L)
  ))

  # 3. Validate the output
  expect_s4_class(neo_converted, "site")

  # Check site metadata
  expect_equal(neo_converted@sitename, "Test Site")
  expect_equal(sf::st_coordinates(neo_converted@geography)[1, "X"], -105.0)
  expect_equal(sf::st_coordinates(neo_converted@geography)[1, "Y"], 40.0)

  # Check that data was populated
  samples_df <- neotoma2::samples(neo_converted)
  expect_gt(nrow(samples_df), 0)
  expect_true("temp" %in% samples_df$variablename)

  # Check that chron was populated
  chron_df <- neotoma2::chronologies(neo_converted)[[1]]@chroncontrols
  expect_gt(nrow(chron_df), 0)
  expect_equal(chron_df$depth, seq(10, 50, by = 10))
})
