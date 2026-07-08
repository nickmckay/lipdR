# Tests for the 0.7.0 upgrades:
#  - inCompilationBeta -> inCompilation on-load migration
#  - standardized time metadata + resolution in extractTs
#  - column number carried through collapseTs
#  - filterByCompilation
#  - print/summary with no measurement table

context("0.7.0 upgrades")

test_that("migrate_incompilation_keys renames legacy inCompilationBeta keys", {
  d <- list(paleoData = list(list(measurementTable = list(list(columns = list(
    list(variableName = "d18O",
         inCompilationBeta1_compilationName = "Temp12k",
         inCompilationBeta = list(list(compilationName = "Temp12k"))))))))
  )
  d2 <- migrate_incompilation_keys(d)
  col <- d2$paleoData[[1]]$measurementTable[[1]]$columns[[1]]
  expect_true("inCompilation1_compilationName" %in% names(col))
  expect_true("inCompilation" %in% names(col))
  expect_false(any(grepl("Beta", names(col))))
})

test_that("extractTs adds standardized time metadata (BP)", {
  L <- readLipd(test_path("ODP1098B13.lpd"))
  ts <- extractTs(L)
  e <- ts[[1]]
  expect_false(is.null(e$time))
  expect_equal(e$timeUnits, "yr BP")
  expect_equal(e$timeDatum, 1950)
  expect_equal(e$timeDirection, "retrograde")
  expect_equal(e$timeExponent, 0)
  expect_true(is.numeric(e$timeMin) && is.numeric(e$timeMax))
  expect_lt(e$timeMin, e$timeMax)
})

test_that("extractTs recognises a calendar-year (CE/AD) time axis", {
  L <- readLipd(test_path("Carre.Saloum.2018.lpd"))
  ts <- extractTs(L)
  e <- ts[[1]]
  expect_equal(e$timeDatum, 0)
  expect_equal(e$timeDirection, "prograde")
})

test_that("calculateResolution flag is off by default and computes when TRUE", {
  L <- readLipd(test_path("ODP1098B13.lpd"))
  ts_on <- extractTs(L, calculateResolution = TRUE)
  e <- ts_on[[1]]
  expect_true(is.numeric(e$hasResolution_hasMedianValue))
  expect_gt(e$hasResolution_hasMedianValue, 0)
})

test_that("time_metadata_from_units handles common unit magnitudes", {
  expect_equal(time_metadata_from_units("age", "ka")$timeExponent, 3)
  expect_equal(time_metadata_from_units("age", "Ma")$timeExponent, 6)
  expect_equal(time_metadata_from_units("age", "Ga")$timeExponent, 9)
  expect_equal(time_metadata_from_units("age", "yr b2k")$timeDatum, 2000)
  expect_equal(time_metadata_from_units("year", "yr AD")$timeDirection, "prograde")
})

test_that("primary column detection prefers isPrimary over age/year", {
  table_data <- list(
    tableName = "t",
    age = list(variableName = "age", units = "yr BP", values = c(1, 2, 3)),
    calAge = list(variableName = "calAge", units = "yr BP", values = c(10, 20, 30),
                  isPrimary = TRUE)
  )
  tc <- identify_time_column(table_data)
  expect_equal(tc$variableName, "calAge")
})

test_that("collapseTs preserves column number", {
  L <- readLipd(test_path("ODP1098B13.lpd"))
  ts <- extractTs(L)
  L2 <- collapseTs(ts)
  mt <- L2$paleoData[[1]]$measurementTable[[1]]
  cols <- mt[sapply(mt, is.list)]
  nums <- sapply(cols, function(x) x$number)
  expect_true(all(!is.na(nums)))
  expect_equal(sort(unname(unlist(nums))), seq_along(cols))
})

test_that("filterByCompilation filters by name and version", {
  mk <- function(vn, comp = NULL, ver = NULL){
    e <- list(paleoData_variableName = vn, paleoData_values = 1:3,
              paleoData_TSid = paste0("T", vn), mode = "paleo")
    if(!is.null(comp)){
      e[["inCompilation1_compilationName"]] <- comp
      e[["inCompilation1_compilationVersion"]] <- ver
    }
    e
  }
  ts <- structure(list(
    mk("d18O", "Temp12k", "1.0.0"),
    mk("SST", "Temp12k", "2.1.0"),
    mk("Mg", "Pages2k", "1.0.0"),
    mk("depth")
  ), class = c("lipd_ts", "list"))

  expect_length(filterByCompilation(ts, "Temp12k"), 2)
  expect_length(filterByCompilation(ts, "Temp12k", version = "2.0", version.op = ">="), 1)
  expect_length(filterByCompilation(ts, "Temp12k", version = "1.0.0", version.op = "=="), 1)
  expect_length(filterByCompilation(ts, "pages2k"), 1) # case-insensitive
  expect_length(filterByCompilation(ts, "none"), 0)
})

test_that("print/summary work when a paleoData object has no measurement table", {
  L <- readLipd(test_path("ODP1098B13.lpd"))
  L$paleoData[[1]]$measurementTable <- NULL
  expect_error(suppressWarnings(capture.output(print(L))), NA)
  expect_error(suppressWarnings(capture.output(summary(L))), NA)
})
